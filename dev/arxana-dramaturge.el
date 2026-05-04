;;; arxana-dramaturge.el --- Read-time dramaturge for Arxana surfaces -*- lexical-binding: t; -*-

;; Author: Joe + Claude
;; Description: Walks actual rendered contents of Arxana surfaces and
;; asserts they match ground truth from the underlying substrate.
;;
;; Fills a specific gap. Arxana already has Reazon-backed invariants
;; (declarative relational assertions over data) and ERT (unit-level
;; Lisp testing). Neither catches the case where a *view* silently goes
;; out of sync with the data: the view renders plausible-looking output,
;; the operator assumes it's current, and the drift sits as "reasonable
;; looking lag" until someone happens to compare against a different
;; surface.
;;
;; The dramaturge fills that gap by walking the rendered contents of a
;; surface (calling the items-fn, inspecting buffer state, snapshotting
;; rendered output) and asserting structural truths about it against
;; the substrate.
;;
;; Generalisation: in the futon stack, "dramaturge" names a family of
;; pre/post/at-rest invariants over typed substrates. The Pattern
;; Peripheral / Sokoban (futon3a/src/futon/peripheral/pattern_author.clj)
;; is a write-time dramaturge; futon1a's model/verify.clj invariants are
;; data dramaturges; the HIT loop (M-pattern-retrieval-calibration) is
;; an operator-judgement dramaturge; this package provides Emacs-surface
;; dramaturges. All four share one shape — assert-some-truth-against-
;; substrate — and differ only in which substrate they assert over.
;;
;; Usage:
;;   (require 'arxana-dramaturge)
;;   M-x arxana-dramaturge-run-all   ;; runs every registered test, reports
;;
;; Adding a test:
;;   (arxana-dramaturge-deftest my-thing
;;     "What this asserts about which surface."
;;     (let ((items (some-view-fn)))
;;       (arxana-dramaturge-assert-true (> (length items) 0)
;;         "Surface should return at least one item")))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)

;;; --------------------------------------------------------------------
;;; Registry

(defvar arxana-dramaturge--registry nil
  "Alist of (NAME . PLIST) for registered tests.
PLIST keys: :doc :body-fn :tags.")

(defvar arxana-dramaturge--fail nil
  "Dynamic accumulator for the currently-running test's failures.
The runner `let'-binds this to `nil' before invoking each test's
body-fn; assertion macros `push' violations onto it; the runner
inspects it after the body returns. Declared with `defvar' so
lexical-binding-t bodies can still mutate it dynamically.")

(defmacro arxana-dramaturge-deftest (name docstring &rest body)
  "Register a dramaturge test NAME with DOCSTRING and BODY.
The body runs with `arxana-dramaturge--fail' dynamically bound;
assertion macros push violations onto it. The test passes iff
the binding is nil after the body returns."
  (declare (indent defun) (doc-string 2))
  `(let ((entry (list :doc ,docstring
                      :body-fn (lambda () ,@body))))
     (setq arxana-dramaturge--registry
           (cons (cons ',name entry)
                 (assq-delete-all ',name arxana-dramaturge--registry)))
     ',name))

;;; --------------------------------------------------------------------
;;; Assertions — push to `arxana-dramaturge--fail' when violated, do not throw

(defun arxana-dramaturge--push-fail (kind detail)
  "Push a structured failure record onto the running test's fail-list."
  (push (list :kind kind :detail detail) arxana-dramaturge--fail))

(defmacro arxana-dramaturge-assert-true (expr description)
  "Assert EXPR is non-nil. DESCRIPTION names the structural claim."
  `(unless ,expr
     (arxana-dramaturge--push-fail :assertion-failed
                                   (format "Expected: %s ; got nil for: %s"
                                           ,description (quote ,expr)))))

(defmacro arxana-dramaturge-assert-equal (a b description)
  "Assert A equals B (via `equal'). DESCRIPTION names the claim."
  `(let ((va ,a) (vb ,b))
     (unless (equal va vb)
       (arxana-dramaturge--push-fail :assertion-failed
                                     (format "Expected %s ; got %S vs %S"
                                             ,description va vb)))))

(defmacro arxana-dramaturge-assert-buffer-contains (buffer-name regexp description)
  "Assert BUFFER-NAME exists and contains REGEXP."
  `(let ((buf (get-buffer ,buffer-name)))
     (cond
      ((null buf)
       (arxana-dramaturge--push-fail :buffer-missing
                                     (format "Expected buffer %s : %s"
                                             ,buffer-name ,description)))
      ((not (with-current-buffer buf
              (save-excursion
                (goto-char (point-min))
                (re-search-forward ,regexp nil t))))
       (arxana-dramaturge--push-fail :buffer-pattern-missing
                                     (format "Buffer %s missing pattern %s : %s"
                                             ,buffer-name ,regexp ,description))))))

(defun arxana-dramaturge-await (predicate &optional timeout-seconds interval-seconds)
  "Poll PREDICATE up to TIMEOUT-SECONDS at INTERVAL-SECONDS; return final value."
  (let ((deadline (+ (float-time) (or timeout-seconds 3)))
        (interval (or interval-seconds 0.1))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (sit-for interval))
    result))

;;; --------------------------------------------------------------------
;;; Substrate fetch helpers (futon1a HTTP)

(defcustom arxana-dramaturge-futon1a-base
  (or (bound-and-true-p arxana-lab-futon1-server)
      "http://localhost:7071/api/alpha")
  "Base URL for futon1a's HTTP API used by dramaturge fetches."
  :type 'string
  :group 'arxana-dramaturge)

(defun arxana-dramaturge-fetch-json (path &optional timeout-seconds)
  "GET PATH (relative to `arxana-dramaturge-futon1a-base'); return parsed plist.
Sends `Accept: application/json' so futon1a returns proper JSON
(string-keyed). Returns nil on transport or parse failure."
  (let* ((base (string-remove-suffix "/" arxana-dramaturge-futon1a-base))
         (url (concat base path))
         (timeout (or timeout-seconds 5))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buffer (ignore-errors (url-retrieve-synchronously url t t timeout))))
    (when (buffer-live-p buffer)
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "\n\n" nil 'move)
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (ignore-errors
                (json-parse-string body
                                   :object-type 'plist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil))))
        (kill-buffer buffer)))))

;;; --------------------------------------------------------------------
;;; Runner

(defun arxana-dramaturge--run-one (name)
  "Run a single test by NAME; return a result plist."
  (let* ((entry (cdr (assq name arxana-dramaturge--registry)))
         (body-fn (plist-get entry :body-fn))
         (start (float-time))
         (arxana-dramaturge--fail nil))
    (when body-fn
      (condition-case err
          (funcall body-fn)
        (error
         (arxana-dramaturge--push-fail :test-error
                                       (format "Body raised: %s" (error-message-string err))))))
    (let ((elapsed (- (float-time) start)))
      (list :name name
            :pass? (null arxana-dramaturge--fail)
            :violations (or arxana-dramaturge--fail '())
            :elapsed-ms (round (* 1000 elapsed))
            :doc (plist-get entry :doc)))))

;;;###autoload
(defun arxana-dramaturge-run-all ()
  "Run every registered dramaturge test; report results.
Opens a results buffer; emits one `dramaturge-result' line per test
suitable for piping into evidence emission later."
  (interactive)
  (let* ((names (mapcar #'car (reverse arxana-dramaturge--registry)))
         (results (mapcar #'arxana-dramaturge--run-one names))
         (buf (get-buffer-create "*arxana-dramaturge*"))
         (passes (cl-count-if (lambda (r) (plist-get r :pass?)) results))
         (fails (- (length results) passes)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "arxana-dramaturge — %d tests, %d pass, %d fail (%s)\n\n"
                        (length results) passes fails
                        (format-time-string "%Y-%m-%dT%H:%M:%S")))
        (dolist (r results)
          (let ((tag (if (plist-get r :pass?) "PASS" "FAIL")))
            (insert (format "[%s] %-40s  %4d ms   %s\n"
                            tag
                            (plist-get r :name)
                            (plist-get r :elapsed-ms)
                            (or (plist-get r :doc) "")))
            (dolist (v (plist-get r :violations))
              (insert (format "       %s : %s\n"
                              (plist-get v :kind)
                              (plist-get v :detail))))))
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)
    (message "arxana-dramaturge: %d/%d passed" passes (length results))
    results))

;;;###autoload
(defun arxana-dramaturge-list ()
  "Show the registered dramaturge tests with their docstrings."
  (interactive)
  (let ((buf (get-buffer-create "*arxana-dramaturge-list*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "arxana-dramaturge — %d tests registered\n\n"
                        (length arxana-dramaturge--registry)))
        (dolist (entry (reverse arxana-dramaturge--registry))
          (insert (format "%s\n  %s\n\n"
                          (car entry)
                          (or (plist-get (cdr entry) :doc) "(no docstring)")))))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

;;; --------------------------------------------------------------------
;;; First test — evidence-sessions sync (the bug we just fixed)
;;;
;;; The Arxana evidence-sessions view's items-fn is supposed to surface
;;; every session that exists in futon1a's evidence store. Before the
;;; aggregator endpoint, the items-fn group-by'd a window of 500 entries
;;; and silently truncated. Today's fix: items-fn now calls
;;; /api/alpha/evidence/sessions which returns full session summaries.
;;;
;;; This test asserts the two never drift again: items-fn count agrees
;;; with the aggregator's total-sessions, sample session counts agree on
;;; both sides, and entry-count totals match within tolerance.

(arxana-dramaturge-deftest evidence-sessions-sync
  "Evidence-sessions view items must equal what the aggregator reports.
   View items-fn count == aggregator total-sessions; sample session
   counts agree exactly; total-entries summed across items matches
   aggregator total-entries."
  (let* ((aggregator (arxana-dramaturge-fetch-json "/evidence/sessions"))
         (truth-sessions (or (plist-get aggregator :sessions) '()))
         (truth-total-sessions (or (plist-get aggregator :total-sessions) 0))
         (truth-total-entries (or (plist-get aggregator :total-entries) 0)))
    (cond
     ((null aggregator)
      (arxana-dramaturge--push-fail :substrate-unreachable
                                    "Could not fetch /evidence/sessions; futon1a may be down."))
     ((not (fboundp 'arxana-browser--evidence-sessions-items))
      (arxana-dramaturge--push-fail :view-fn-missing
                                    "arxana-browser--evidence-sessions-items not defined."))
     (t
      (let* ((items (arxana-browser--evidence-sessions-items))
             (item-evidence-sessions (cl-remove-if-not
                                       (lambda (i) (eq (plist-get i :type) 'evidence-session))
                                       items))
             (view-count (length item-evidence-sessions))
             (view-total-entries
              (apply #'+ (mapcar (lambda (i) (or (plist-get i :entry-count) 0))
                                 item-evidence-sessions))))
        (arxana-dramaturge-assert-equal view-count truth-total-sessions
          "view item count == aggregator total-sessions")
        (arxana-dramaturge-assert-equal view-total-entries truth-total-entries
          "view summed entry-count == aggregator total-entries")
        ;; Sample-check: pick the first truth session, find it in view, compare counts.
        (when (and truth-sessions item-evidence-sessions)
          (let* ((sample (car truth-sessions))
                 (sid (plist-get sample :session-id))
                 (truth-count (plist-get sample :count))
                 (view-match (cl-find-if
                               (lambda (i) (equal (plist-get i :session-id) sid))
                               item-evidence-sessions))
                 (view-match-count (and view-match (plist-get view-match :entry-count))))
            (arxana-dramaturge-assert-true view-match
              (format "session %s in view items" sid))
            (when view-match
              (arxana-dramaturge-assert-equal view-match-count truth-count
                (format "session %s entry-count agrees with aggregator" sid))))))))))

;;; --------------------------------------------------------------------
;;; Test — candidate-invariants items have dispatchable :type
;;;
;;; Bug found 2026-05-04: opening the first item (priority-rank -2) in
;;; arxana://view/candidate-invariants was silently a no-op, while
;;; opening the priority-6 item worked. Cause: tracer items
;;; (pre-built in `arxana-browser--invariant-queue-tracer-items') have
;;; no `:type' keyword, so the row-open dispatcher in
;;; arxana-browser-core.el (case-by-:type) silently fell through.
;;;
;;; This test asserts every non-info item in the view has a non-nil
;;; `:type' — sufficient to catch the bug at test-run time instead of
;;; the operator noticing it by hand.

(arxana-dramaturge-deftest candidate-invariants-items-have-type
  "Every non-info item in candidate-invariants has a dispatchable :type.
   The view's row-open dispatcher (arxana-browser-core, case on
   `:type`) silently no-ops on items missing :type. Tracer items
   were the case in point 2026-05-04."
  (cond
   ((not (fboundp 'arxana-browser--candidate-invariants-items))
    (arxana-dramaturge--push-fail :view-fn-missing
                                  "arxana-browser--candidate-invariants-items not defined."))
   (t
    (let* ((items (arxana-browser--candidate-invariants-items))
           (entries (cl-remove-if (lambda (i) (eq (plist-get i :type) 'info))
                                  items))
           (untyped (cl-remove-if (lambda (i) (plist-get i :type)) entries)))
      (arxana-dramaturge-assert-true (> (length entries) 0)
        "candidate-invariants returns at least one entry")
      (arxana-dramaturge-assert-equal (length untyped) 0
        (format "every entry has :type (found %d untyped of %d total)"
                (length untyped) (length entries)))
      (when untyped
        (arxana-dramaturge--push-fail
         :sample-untyped
         (format "first untyped item: invariant=%S source=%S priority-rank=%S"
                 (plist-get (car untyped) :invariant)
                 (plist-get (car untyped) :source)
                 (plist-get (car untyped) :priority-rank))))))))

;;; --------------------------------------------------------------------
;;; Test — invariant-queue motion-flag must not lie about stack motion
;;;
;;; Bug found 2026-05-04: the stack-hud invariant-queue widget rendered
;;; "STUCK   open: 0   closed: 0   canary fires: 11". The "STUCK" label
;;; came from `stack-hud-widget--motion-flag', whose data path looks
;;; only at pipeline-tracer-tagged open/closed evidence — neither of
;;; which has been emitted in the window. But the same widget surfaces
;;; 11 canary fires alongside, demonstrating clearly that the system
;;; *is* moving by another tracked signal. The flag was over-narrow.
;;;
;;; Joe's read: "I have seen that before and it's just inaccurate
;;; b/c the Invariant Queue *is moving*."
;;;
;;; The structural fix: motion-flag must consider all motion signals
;;; the widget already reports, not just pipeline-tracer open/closed.
;;; This test asserts the operator-meaningful invariant:
;;;
;;;   if the widget reports any nonzero motion signal (closed-count or
;;;   canary-total > 0), motion-flag must not be STUCK.

(arxana-dramaturge-deftest invariant-queue-flag-honesty
  "Invariant-queue motion-flag must not say STUCK when any tracked
   motion signal in the same widget is nonzero. STUCK is reserved for
   genuine no-motion (closed=0 AND canary=0)."
  (cond
   ((not (fboundp 'stack-hud-widget--invariant-queue-data))
    (arxana-dramaturge--push-fail :widget-fn-missing
                                  "stack-hud-widget--invariant-queue-data not loaded."))
   ((not (fboundp 'stack-hud-widget--motion-flag))
    (arxana-dramaturge--push-fail :flag-fn-missing
                                  "stack-hud-widget--motion-flag not loaded."))
   (t
    (let* ((data (stack-hud-widget--invariant-queue-data))
           (open-count (length (plist-get data :open)))
           (closed-count (or (plist-get data :closed-count) 0))
           (canary-total (or (plist-get data :canary-total) 0))
           (flag-raw (stack-hud-widget--motion-flag open-count closed-count canary-total))
           (flag (substring-no-properties flag-raw))
           (any-motion? (or (> closed-count 0) (> canary-total 0))))
      (when (and any-motion? (string= flag "STUCK"))
        (arxana-dramaturge--push-fail
         :motion-flag-lies
         (format "Widget says STUCK but reports motion: closed=%d canary=%d open=%d"
                 closed-count canary-total open-count)))))))

;;; --------------------------------------------------------------------
;;; Test — HUD invariant-queue widget honestly tracks tracer state
;;;
;;; When a pipeline-tracer event gets instantiated (open) and later
;;; closed, the widget's data plist must reflect the transition:
;;; OPEN-only → MOVING/PULSING; OPEN+CLOSED for same track-id → CLOSED
;;; (track filtered out of `:open`, counted in `:closed-count`).
;;;
;;; Done with synthetic fixtures by `cl-letf`-shadowing
;;; `stack-hud-widget--fetch-coordination' — keeps the live evidence
;;; store untouched, exercises the downstream computation purely.
;;;
;;; Locks in three transitions in one test:
;;;   T1  open A only                       →  PULSING (closed=0, canary>0)
;;;   T2  open A + closed A (same id)       →  FLOWING (open list empty, closed=1)
;;;   T3  open A + closed A + open B + open C  →  MOVING (open=2 > closed=1)

(defun arxana-dramaturge--mock-coord-entry (tags body &optional event)
  "Build a synthetic coordination evidence entry shaped like
`stack-hud-widget--fetch-coordination' would return: a plist with
`:evidence/tags', `:evidence/at', `:evidence/body'."
  (list :evidence/at (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
        :evidence/tags tags
        :evidence/body (cond
                        (event (append body (list :event event)))
                        (t body))))

(arxana-dramaturge-deftest invariant-queue-tracer-state-transitions
  "Pipeline-tracer instantiation transitions are honestly reflected in
   the HUD invariant-queue widget's data plist. Three transitions are
   asserted via cl-letf-shadowed synthetic fixtures."
  (cond
   ((not (fboundp 'stack-hud-widget--invariant-queue-data))
    (arxana-dramaturge--push-fail :widget-fn-missing
                                  "stack-hud-widget--invariant-queue-data not loaded."))
   (t
    (let* ((open-only
            (list
             (arxana-dramaturge--mock-coord-entry
              '("pipeline-tracer" "open")
              '(:track-id "track-test-A" :title "Test A" :target-date "2026-05-10"))
             (arxana-dramaturge--mock-coord-entry
              '("family-canary")
              '(:family-id "evidence-per-turn" :outcome "ok")
              "family-fired")))
           (open-and-closed
            (cons (arxana-dramaturge--mock-coord-entry
                   '("pipeline-tracer" "closed")
                   '(:track-id "track-test-A"))
                  open-only))
           (open-two-closed-one
            (append
             (list
              (arxana-dramaturge--mock-coord-entry
               '("pipeline-tracer" "open")
               '(:track-id "track-test-B" :title "Test B" :target-date "2026-05-12"))
              (arxana-dramaturge--mock-coord-entry
               '("pipeline-tracer" "open")
               '(:track-id "track-test-C" :title "Test C" :target-date "2026-05-14")))
             open-and-closed)))
      ;; T1: open=1, closed=0, canary=1 → PULSING
      (cl-letf (((symbol-function 'stack-hud-widget--fetch-coordination)
                 (lambda (&rest _) open-only)))
        (let* ((data (stack-hud-widget--invariant-queue-data))
               (open (plist-get data :open))
               (closed (plist-get data :closed-count))
               (canary (plist-get data :canary-total))
               (flag (substring-no-properties
                      (stack-hud-widget--motion-flag (length open) closed canary))))
          (arxana-dramaturge-assert-equal (length open) 1
            "T1 open count = 1")
          (arxana-dramaturge-assert-equal closed 0
            "T1 closed-count = 0")
          (arxana-dramaturge-assert-equal canary 1
            "T1 canary-total = 1")
          (arxana-dramaturge-assert-equal flag "PULSING"
            "T1 flag = PULSING (closed=0 with canary>0)")))
      ;; T2: same track closed → open list empty, closed-count=1 → FLOWING
      (cl-letf (((symbol-function 'stack-hud-widget--fetch-coordination)
                 (lambda (&rest _) open-and-closed)))
        (let* ((data (stack-hud-widget--invariant-queue-data))
               (open (plist-get data :open))
               (closed (plist-get data :closed-count))
               (canary (plist-get data :canary-total))
               (flag (substring-no-properties
                      (stack-hud-widget--motion-flag (length open) closed canary))))
          (arxana-dramaturge-assert-equal (length open) 0
            "T2 open list empty (track-test-A filtered by closed counterpart)")
          (arxana-dramaturge-assert-equal closed 1
            "T2 closed-count = 1")
          (arxana-dramaturge-assert-equal flag "FLOWING"
            "T2 flag = FLOWING (closed >= open)")))
      ;; T3: second open without closed → MOVING
      (cl-letf (((symbol-function 'stack-hud-widget--fetch-coordination)
                 (lambda (&rest _) open-two-closed-one)))
        (let* ((data (stack-hud-widget--invariant-queue-data))
               (open (plist-get data :open))
               (closed (plist-get data :closed-count))
               (canary (plist-get data :canary-total))
               (flag (substring-no-properties
                      (stack-hud-widget--motion-flag (length open) closed canary))))
          (arxana-dramaturge-assert-equal (length open) 2
            "T3 open list = 2 (track-test-B + track-test-C; track-test-A filtered)")
          (arxana-dramaturge-assert-equal closed 1
            "T3 closed-count = 1")
          (arxana-dramaturge-assert-equal flag "MOVING"
            "T3 flag = MOVING (closed < open)")))))))

(provide 'arxana-dramaturge)
;;; arxana-dramaturge.el ends here
