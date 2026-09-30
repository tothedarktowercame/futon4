;;; arxana-dramaturge-scene.el --- Drive Emacs surfaces from scripted scenes -*- lexical-binding: t; -*-

;; Author: kimi-3 for M-象-2000 INTERACT-1 (§0)
;; Description: Extends `arxana-dramaturge' (read-time assertions over
;; rendered surfaces) with a *driver*: scripted scenes that type, press
;; keys through real keymaps, await conditions, assert on buffers, and
;; snapshot rendered output.  Scenes run in a SEPARATE Emacs daemon
;; (server name "dramaturge"), never in the operator's Emacs; see
;; run-dramaturge-scenes.sh.
;;
;; Usage (inside the dramaturge daemon):
;;   (require 'arxana-dramaturge-scene)
;;   (arxana-dramaturge-scene my-scene
;;     "What this scene drives and asserts."
;;     (open-buffer "*scratch-scene*")
;;     (type "hello")
;;     (key "RET")
;;     (await (lambda () t) :timeout 2)
;;     (assert-buffer "hello")
;;     (assert (= 1 1) "arithmetic still works")
;;     (snapshot "my-scene"))
;;   (arxana-dramaturge-run-scenes)           ; interactive report
;;   (arxana-dramaturge-scenes-batch)         ; writes report file, returns "PASS"/"FAIL"
;;
;; Step status vocabulary: pass | fail | timed-out.
;; A scene passes iff every step passed.  The runner always continues
;; to the next scene; `arxana-dramaturge-scenes-batch' returns "PASS"
;; or "FAIL" for the shell wrapper to turn into an exit code.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'arxana-dramaturge)

(declare-function htmlize-buffer "htmlize")

;;; -------------------------------------------------------------------- customs

(defcustom arxana-dramaturge-scene-out-dir
  (expand-file-name "scene-output"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Directory for scene reports and snapshots."
  :type 'directory
  :group 'arxana-dramaturge)

(defcustom arxana-dramaturge-scene-streams-dir
  "/home/joe/code/futon3c/emacs/scenes/streams/"
  "Directory holding canned NDJSON streams for the stub agent replayer."
  :type 'directory
  :group 'arxana-dramaturge)

(defcustom arxana-dramaturge-scene-dirs
  (list (expand-file-name "scenes"
                          (file-name-directory (or load-file-name
                                                   buffer-file-name
                                                   default-directory))))
  "Directories whose *.el files the batch runner loads as scenes."
  :type '(repeat directory)
  :group 'arxana-dramaturge)

;;; -------------------------------------------------------------------- registry

(defvar arxana-dramaturge-scene--registry nil
  "Alist of (NAME . PLIST) with :doc :body-fn :live.")

(defvar arxana-dramaturge-scene--steps nil
  "Dynamic accumulator of step reports for the running scene.")

(defvar arxana-dramaturge-scene--buffer nil
  "Dynamic: the buffer the running scene drives (set by open-buffer).")

(defun arxana-dramaturge-scene--register (name doc live expected-fail body-fn)
  (setq arxana-dramaturge-scene--registry
        (cons (cons name (list :doc doc :live live
                               :expected-fail expected-fail
                               :body-fn body-fn))
              (assq-delete-all name arxana-dramaturge-scene--registry)))
  name)

(defun arxana-dramaturge-scene--counts-as-pass (result expected-fail)
  "Verdict accounting: an :expected-fail scene counts iff it DID fail."
  (if expected-fail
      (not (plist-get result :pass?))
    (plist-get result :pass?)))

;;; -------------------------------------------------------------------- step machinery

(defun arxana-dramaturge-scene--report (status form &optional detail)
  "Record a step report. STATUS is pass, fail, or timed-out."
  (push (list :status status :form form :detail detail)
        arxana-dramaturge-scene--steps)
  status)

(defun arxana-dramaturge-scene--require-buffer ()
  (unless (buffer-live-p arxana-dramaturge-scene--buffer)
    (error "Scene has no live target buffer; call open-buffer first"))
  arxana-dramaturge-scene--buffer)

(defun arxana-dramaturge-scene--open-buffer (name-or-form)
  (condition-case err
      (let* ((name (if (stringp name-or-form) name-or-form
                     (eval name-or-form)))
             (buf (get-buffer-create name)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer))
          (fundamental-mode))
        (setq arxana-dramaturge-scene--buffer buf)
        (arxana-dramaturge-scene--report 'pass `(open-buffer ,name-or-form)
                                         (format "buffer %s" name)))
    (error (arxana-dramaturge-scene--report 'fail `(open-buffer ,name-or-form)
                                            (error-message-string err)))))

(defun arxana-dramaturge-scene--execute-key (keys)
  "Press KEYS in the current buffer: resolve through the ACTIVE KEYMAPS
\(so keymap bugs are visible) and run the bound command.
We cannot use `execute-kbd-macro': the command loop re-selects the
selected window's buffer on each iteration, which in a frameless daemon
is *scratch* — keys would silently land in the wrong buffer.  And
`command-execute' hangs on self-insert keys (SPC) in a frameless
daemon, when it tries to echo the recorded keystrokes with no frame.
Instead we do what the command loop does for one key sequence, minus
the echo: look up the binding, set `last-command-event'/`this-command',
and invoke the command directly.
Returns the command, or :unbound.

Note: KEYS is read with `kbd' UNLESS it is a single character —
\(kbd \" \") returns the empty sequence (a space is a separator in key
descriptions), and `key-binding' of the empty sequence returns the
list of active keymaps instead of a command."
  (let* ((k (if (= (length keys) 1)
                (vector (aref keys 0))
              (kbd keys)))
         (binding (key-binding k)))
    (cond
     ((or (null binding) (eq binding 'undefined))
      :unbound)
     ((not (commandp binding))
      (error "%s resolves to non-command %S (buffer %s, mode %s, point %s, props %S)"
             keys binding (buffer-name) major-mode (point)
             (text-properties-at (point))))
     (t
      (let ((last-command-event (aref k (1- (length k))))
            (last-command nil)
            (this-command binding))
        (if (eq binding 'self-insert-command)
            (self-insert-command 1 last-command-event)
          (call-interactively binding nil))
        binding)))))

(defun arxana-dramaturge-scene--type (text)
  (condition-case err
      (with-current-buffer (arxana-dramaturge-scene--require-buffer)
        ;; Each character through its key binding, so self-insert
        ;; (or whatever the mode binds) is what actually runs.
        (cl-loop for c across text
                 for res = (arxana-dramaturge-scene--execute-key (string c))
                 when (eq res :unbound)
                 do (error "%c is not bound in %s" c (buffer-name)))
        (arxana-dramaturge-scene--report 'pass `(type ,text)))
    (error (arxana-dramaturge-scene--report 'fail `(type ,text)
                                            (error-message-string err)))))

(defun arxana-dramaturge-scene--key (keys)
  "Press KEYS in the scene buffer via its active keymaps.
Fails the step (without executing) when KEYS are not bound to a
command — this is exactly the shape of the r/R reload bug
\(futon3c 1253a8cc), where the key was simply undefined after a reload."
  (condition-case err
      (with-current-buffer (arxana-dramaturge-scene--require-buffer)
        (let ((res (arxana-dramaturge-scene--execute-key keys)))
          (if (eq res :unbound)
              (arxana-dramaturge-scene--report
               'fail `(key ,keys)
               (format "%s is not bound in %s's active keymaps"
                       keys (buffer-name)))
            (arxana-dramaturge-scene--report
             'pass `(key ,keys) (format "bound to %s" res)))))
    (error (arxana-dramaturge-scene--report 'fail `(key ,keys)
                                            (error-message-string err)))))

(defun arxana-dramaturge-scene--await (pred timeout)
  (let ((ok (arxana-dramaturge-await pred timeout)))
    (if ok
        (arxana-dramaturge-scene--report 'pass `(await ,pred :timeout ,timeout))
      (arxana-dramaturge-scene--report 'timed-out `(await ,pred :timeout ,timeout)
                                       (format "predicate still nil after %ss" timeout)))))

(defun arxana-dramaturge-scene--assert-buffer (regexp)
  (with-current-buffer (arxana-dramaturge-scene--require-buffer)
    (if (save-excursion
          (goto-char (point-min))
          (re-search-forward regexp nil t))
        (arxana-dramaturge-scene--report 'pass `(assert-buffer ,regexp))
      (arxana-dramaturge-scene--report 'fail `(assert-buffer ,regexp)
                                       (format "buffer %s lacks %s"
                                               (buffer-name) regexp)))))

(defun arxana-dramaturge-scene--assert (pred description)
  (if (funcall pred)
      (arxana-dramaturge-scene--report 'pass `(assert ,pred ,description))
    (arxana-dramaturge-scene--report 'fail `(assert ,pred ,description)
                                     description)))

(defun arxana-dramaturge-scene--snapshot (name)
  "Write the scene buffer to <out-dir>/NAME.html (htmlize) or NAME.txt.
Returns the path written; the step detail records which format was used."
  (condition-case err
      (progn
        (make-directory arxana-dramaturge-scene-out-dir t)
        (with-current-buffer (arxana-dramaturge-scene--require-buffer)
          (if (require 'htmlize nil t)
              (let* ((path (expand-file-name (concat name ".html")
                                             arxana-dramaturge-scene-out-dir))
                     (html (htmlize-buffer)))
                (with-current-buffer html
                  (write-region nil nil path nil 'silent)
                  (kill-buffer html))
                (arxana-dramaturge-scene--report 'pass `(snapshot ,name)
                                                 (format "htmlize -> %s" path)))
            (let ((path (expand-file-name (concat name ".txt")
                                          arxana-dramaturge-scene-out-dir)))
              (write-region (buffer-substring-no-properties (point-min) (point-max))
                            nil path nil 'silent)
              (arxana-dramaturge-scene--report 'pass `(snapshot ,name)
                                               (format "plain text (htmlize unavailable) -> %s"
                                                       path))))))
    (error (arxana-dramaturge-scene--report 'fail `(snapshot ,name)
                                            (error-message-string err)))))

;;; -------------------------------------------------------------------- the macro

(defmacro arxana-dramaturge-scene (name docstring &rest body)
  "Register scene NAME with DOCSTRING and step BODY.
BODY may begin with keyword options: :live t marks a scene that
spends real tokens (skipped unless requested); :expected-fail t marks
a scene whose failure is the acceptance signal (planted bugs,
deliberate timeouts) — it counts as passing the run iff it fails.
Steps are written
with the bare names open-buffer, type, key, await, assert-buffer,
assert, snapshot — macrolet-bound inside the body."
  (declare (indent defun) (doc-string 2))
  (let ((live nil) (expected-fail nil))
    (while (keywordp (car body))
      (let ((k (pop body)) (v (pop body)))
        (pcase k
          (:live (setq live v))
          (:expected-fail (setq expected-fail v))
          (_ (error "arxana-dramaturge-scene: unknown option %S" k)))))
    `(arxana-dramaturge-scene--register
      ',name ,docstring ,live ,expected-fail
      (lambda ()
        (cl-macrolet
            ((open-buffer (name-or-form)
               `(arxana-dramaturge-scene--open-buffer ,name-or-form))
             (type (text) `(arxana-dramaturge-scene--type ,text))
             (key (keys) `(arxana-dramaturge-scene--key ,keys))
             (await (pred &rest opts)
               `(arxana-dramaturge-scene--await
                 ,pred ,(or (plist-get opts :timeout) 3)))
             (assert-buffer (regexp)
               `(arxana-dramaturge-scene--assert-buffer ,regexp))
             (assert (pred description)
               `(arxana-dramaturge-scene--assert ,pred ,description))
             (snapshot (name) `(arxana-dramaturge-scene--snapshot ,name)))
          nil
          ,@body)))))

;;; -------------------------------------------------------------------- runner

(defun arxana-dramaturge-scene--run-one (name)
  "Run scene NAME; return a result plist.  Never throws; never hangs
past the sum of its step timeouts."
  (let* ((entry (cdr (assq name arxana-dramaturge-scene--registry)))
         (body-fn (plist-get entry :body-fn))
         (expected-fail (plist-get entry :expected-fail))
         (start (float-time))
         (arxana-dramaturge-scene--steps nil)
         (arxana-dramaturge-scene--buffer nil))
    (when body-fn
      (condition-case err
          (funcall body-fn)
        (error
         (arxana-dramaturge-scene--report 'fail '(scene-body)
                                          (format "raised: %s"
                                                  (error-message-string err))))))
    (list :name name
          :pass? (cl-every (lambda (s)
                             (eq (plist-get s :status) 'pass))
                           arxana-dramaturge-scene--steps)
          :steps (nreverse arxana-dramaturge-scene--steps)
          :elapsed-ms (round (* 1000 (- (float-time) start)))
          :expected-fail expected-fail
          :doc (plist-get entry :doc))))

(defun arxana-dramaturge-scene--format-report (results)
  "Pure: render RESULTS (list of run-one plists) as a report string."
  (let* ((passes (cl-count-if (lambda (r)
                                (arxana-dramaturge-scene--counts-as-pass
                                 r (plist-get r :expected-fail)))
                              results))
         (lines nil))
    (push (format "arxana-dramaturge scenes — %d scenes, %d pass, %d fail (%s)"
                  (length results) passes (- (length results) passes)
                  (format-time-string "%Y-%m-%dT%H:%M:%S"))
          lines)
    (dolist (r results)
      (push "" lines)
      (push (format "scene %s: %s (%d ms) — %s"
                    (plist-get r :name)
                    (if (plist-get r :pass?)
                        (if (plist-get r :expected-fail)
                            "PASS (UNEXPECTED — expected-fail scene passed)"
                          "PASS")
                      (if (plist-get r :expected-fail)
                          "FAIL (expected)"
                        "FAIL"))
                    (plist-get r :elapsed-ms)
                    (or (plist-get r :doc) ""))
            lines)
      (dolist (s (plist-get r :steps))
        (push (format "  [%-9s] %-40s %s"
                      (plist-get s :status)
                      (format "%S" (plist-get s :form))
                      (or (plist-get s :detail) ""))
              lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

;;;###autoload
(defun arxana-dramaturge-run-scenes (&optional only-name include-live)
  "Run registered scenes (or ONLY-NAME); show the report buffer.
Skips :live scenes unless INCLUDE-LIVE.  Returns the results list."
  (interactive)
  (let* ((names (if only-name (list only-name)
                  (mapcar #'car (reverse arxana-dramaturge-scene--registry))))
         (names (cl-remove-if
                 (lambda (n)
                   (and (not include-live)
                        (plist-get (cdr (assq n arxana-dramaturge-scene--registry))
                                   :live)))
                 names))
         (results (mapcar #'arxana-dramaturge-scene--run-one names))
         (report (arxana-dramaturge-scene--format-report results))
         (buf (get-buffer-create "*arxana-dramaturge-scenes*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert report "\n")
        (goto-char (point-min)))
      (special-mode))
    (when (called-interactively-p 'any)
      (display-buffer buf))
    results))

(defun arxana-dramaturge-scene--load-scene-files ()
  "Load every *.el under `arxana-dramaturge-scene-dirs' (non-recursive)."
  (dolist (dir arxana-dramaturge-scene-dirs)
    (when (file-directory-p dir)
      (dolist (f (directory-files dir t "\\.el\\'"))
        (load f nil t)))))

(defun arxana-dramaturge-scenes-batch (&optional only-name)
  "Noninteractive entry for run-dramaturge-scenes.sh.
Loads scene files, runs scenes (or ONLY-NAME), writes the report to
<out-dir>/report.txt, and returns the string \"PASS\" or \"FAIL\"."
  (make-directory arxana-dramaturge-scene-out-dir t)
  (arxana-dramaturge-scene--load-scene-files)
  (let* ((results (arxana-dramaturge-run-scenes only-name))
         (report (arxana-dramaturge-scene--format-report results))
         (verdict (if (and results
                           (cl-every (lambda (r)
                                       (arxana-dramaturge-scene--counts-as-pass
                                        r (plist-get r :expected-fail)))
                                     results))
                      "PASS" "FAIL")))
    (with-temp-file (expand-file-name "report.txt" arxana-dramaturge-scene-out-dir)
      (insert report "\n"))
    verdict))

;;; -------------------------------------------------------------------- stub agent

(defun arxana-dramaturge-scene--read-ndjson (file)
  "Parse FILE as NDJSON; return the list of parsed plists, in order.
Pure with respect to Emacs state (only reads the file); ERT-tested."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((events nil))
      (while (re-search-forward "^.+$" nil t)
        (push (json-parse-string (match-string 0)
                                 :object-type 'plist
                                 :array-type 'list)
              events))
      (nreverse events))))

(defvar arxana-dramaturge-scene--stub-originals nil
  "Alist of (SYMBOL . original function definition) for installed stubs.")

(defun arxana-dramaturge-scene-stub-install (stream-file)
  "Replace `claude-repl--call-claude-streaming' (TEXT CALLBACK signature)
with a deterministic replayer of the NDJSON file STREAM-FILE (relative
to `arxana-dramaturge-scene-streams-dir' unless absolute).  Each parsed
line is delivered to CALLBACK in order on a zero timer, so scenes await
the REPL as they would with a live seat.  No tokens are spent.
Returns non-nil on success; restore with `arxana-dramaturge-scene-stub-reset'."
  (let* ((path (if (file-name-absolute-p stream-file) stream-file
                 (expand-file-name stream-file
                                   arxana-dramaturge-scene-streams-dir)))
         (events (arxana-dramaturge-scene--read-ndjson path))
         (sym 'claude-repl--call-claude-streaming))
    (unless (assq sym arxana-dramaturge-scene--stub-originals)
      (push (cons sym (and (fboundp sym) (symbol-function sym)))
            arxana-dramaturge-scene--stub-originals))
    (fset sym (lambda (_text callback)
                (dolist (ev events)
                  (run-at-time 0 nil callback ev))))
    t))

(defun arxana-dramaturge-scene-stub-reset ()
  "Restore every function replaced by `arxana-dramaturge-scene-stub-install'."
  (dolist (pair arxana-dramaturge-scene--stub-originals)
    (if (cdr pair)
        (fset (car pair) (cdr pair))
      (fmakunbound (car pair))))
  (setq arxana-dramaturge-scene--stub-originals nil))

(provide 'arxana-dramaturge-scene)
;;; arxana-dramaturge-scene.el ends here
