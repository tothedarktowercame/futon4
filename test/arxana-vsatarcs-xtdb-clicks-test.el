;;; arxana-vsatarcs-xtdb-clicks-test.el --- Tests for VSATARCS XTDB-clicks projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-xtdb-clicks.el' (R10 engagement-time
;; surface via XTDB query).  Mocks `arxana-store-fetch-hyperedges' so
;; tests don't depend on a live futon1a server.  Covers:
;;
;;   - Per-hyperedge summary extraction (alist/plist key-shape
;;     defensiveness)
;;   - Per-stream fetch wrapping (loaded? / total-in-stream / records)
;;   - Snapshot aggregation across configured stream types
;;   - Defensive handling when one stream errors / times out
;;   - Stable snapshot shape on (server-up / server-down / mixed)
;;   - Digest line case-distinction

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-xtdb-clicks)

;; ---------------------------------------------------------------------
;; Mock helpers
;; ---------------------------------------------------------------------

(defvar arxana-vsatarcs-xtdb-clicks-test--responses nil
  "Alist of (TYPE-STRING . RESPONSE-OR-FN).
Used by the mocked `arxana-store-fetch-hyperedges' to choose what
to return per query.  When the value is a function, it's invoked
with no args (lets tests simulate errors via signalling).")

(defun arxana-vsatarcs-xtdb-clicks-test--mock-fetch (&rest args)
  "Mock for `arxana-store-fetch-hyperedges': lookup response by type."
  (let* ((hx-type (plist-get args :type))
         (entry (assoc hx-type arxana-vsatarcs-xtdb-clicks-test--responses))
         (value (cdr entry)))
    (cond
     ((null entry) nil)
     ((functionp value) (funcall value))
     (t value))))

(defmacro arxana-vsatarcs-xtdb-clicks-test--with-mock (responses &rest body)
  "Bind the mock + RESPONSES then run BODY.
Forces the sequential code path (`-parallel-enabled' = nil) so the
sync `arxana-store-fetch-hyperedges' mock is the only network
surface tests need to inject at."
  (declare (indent 1))
  `(let ((arxana-vsatarcs-xtdb-clicks-test--responses ,responses)
         (arxana-vsatarcs-xtdb-clicks-parallel-enabled nil))
     (cl-letf (((symbol-function 'arxana-store-fetch-hyperedges)
                #'arxana-vsatarcs-xtdb-clicks-test--mock-fetch))
       ,@body)))

(defun arxana-vsatarcs-xtdb-clicks-test--mk-hx
    (&optional repo source cycle ts)
  "Build a fake watcher-event hyperedge alist."
  (list (cons 'hx/type "code/v05/watcher-event")
        (cons 'hx/props
              (list (cons "repo" (or repo "futon2-d"))
                    (cons "source" (or source "heartbeat"))
                    (cons "cycle" (or cycle 42))
                    (cons "ts" (or ts 1716195000000))
                    (cons "run-id" "run-abc")
                    (cons "files-seen" 12)
                    (cons "files-changed" 3)
                    (cons "n-deleted" 0)
                    (cons "n-renamed" 0)
                    (cons "n-added" 1)
                    (cons "n-cross-root-moves" 0)))))

(defun arxana-vsatarcs-xtdb-clicks-test--mk-response (n)
  "Build a {:hyperedges [N records] :count N} response."
  (list (cons 'hyperedges
              (cl-loop for i from 0 below n
                       collect (arxana-vsatarcs-xtdb-clicks-test--mk-hx
                                "futon2-d" "heartbeat" (+ 100 i)
                                (+ 1716195000000 (* i 1000)))))
        (cons 'count n)))

;; ---------------------------------------------------------------------
;; --extract-props + --prop defensiveness
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-xtdb-clicks-extract-props-alist ()
  (let ((hx (list (cons 'hx/type "code/v05/watcher-event")
                  (cons 'hx/props (list (cons "ts" 12345))))))
    (let ((props (arxana-vsatarcs-xtdb-clicks--extract-props hx)))
      (should props)
      (should (= 12345 (cdr (assoc "ts" props)))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-prop-string-key ()
  (let ((props (list (cons "ts" 999))))
    (should (= 999 (arxana-vsatarcs-xtdb-clicks--prop props "ts")))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-prop-symbol-key ()
  (let ((props (list (cons 'ts 999))))
    (should (= 999 (arxana-vsatarcs-xtdb-clicks--prop props "ts")))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-prop-missing-returns-nil ()
  (should (null (arxana-vsatarcs-xtdb-clicks--prop '() "absent"))))

;; ---------------------------------------------------------------------
;; --summarise-hx
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-xtdb-clicks-summarise-watcher-event ()
  (let* ((hx (arxana-vsatarcs-xtdb-clicks-test--mk-hx
              "futon4-d" "file-change" 7 1716200000000))
         (s (arxana-vsatarcs-xtdb-clicks--summarise-hx hx :watcher-event)))
    (should (eq :watcher-event (plist-get s :subclass)))
    (should (= 1716200000000 (plist-get s :ts)))
    (should (equal "file-change" (plist-get s :source)))
    (should (equal "futon4-d" (plist-get s :repo)))
    (should (= 7 (plist-get s :cycle)))
    (should (equal "run-abc" (plist-get s :run-id)))
    (should (= 12 (cdr (assoc :files-seen (plist-get s :counts)))))
    (should (= 3 (cdr (assoc :files-changed (plist-get s :counts)))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-summarise-carries-raw ()
  ;; :raw lets the chrome drill into the unprojected hyperedge if needed.
  (let* ((hx (arxana-vsatarcs-xtdb-clicks-test--mk-hx))
         (s (arxana-vsatarcs-xtdb-clicks--summarise-hx hx :watcher-event)))
    (should (eq hx (plist-get s :raw)))))

;; ---------------------------------------------------------------------
;; --fetch-stream
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-xtdb-clicks-fetch-stream-ok ()
  (arxana-vsatarcs-xtdb-clicks-test--with-mock
      (list (cons "code/v05/watcher-event"
                  (arxana-vsatarcs-xtdb-clicks-test--mk-response 3)))
    (let ((result (arxana-vsatarcs-xtdb-clicks--fetch-stream
                   "code/v05/watcher-event" :watcher-event 10)))
      (should result)
      (should (plist-get result :stream-loaded?))
      (should (= 3 (plist-get result :total-in-stream)))
      (should (= 3 (length (plist-get result :records)))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-fetch-stream-empty ()
  ;; Type queried but no records — loaded?=t, total=0.
  (arxana-vsatarcs-xtdb-clicks-test--with-mock
      (list (cons "code/v05/watcher-event"
                  (list (cons 'hyperedges nil) (cons 'count 0))))
    (let ((result (arxana-vsatarcs-xtdb-clicks--fetch-stream
                   "code/v05/watcher-event" :watcher-event 10)))
      (should (plist-get result :stream-loaded?))
      (should (= 0 (plist-get result :total-in-stream)))
      (should (equal '() (plist-get result :records))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-fetch-stream-error ()
  ;; Mocked fetch returns nil (server unreachable / timeout / etc.).
  (arxana-vsatarcs-xtdb-clicks-test--with-mock
      (list (cons "code/v05/watcher-event" nil))
    (let ((result (arxana-vsatarcs-xtdb-clicks--fetch-stream
                   "code/v05/watcher-event" :watcher-event 10)))
      ;; nil return means stream unavailable today (callers fill in
      ;; the degenerate snapshot shape).
      (should (null result)))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-fetch-stream-thrown-error ()
  ;; Mocked fetch signals an error — ignore-errors in the module
  ;; should catch and return nil.
  (arxana-vsatarcs-xtdb-clicks-test--with-mock
      (list (cons "code/v05/watcher-event"
                  (lambda () (error "Simulated timeout"))))
    (let ((result (arxana-vsatarcs-xtdb-clicks--fetch-stream
                   "code/v05/watcher-event" :watcher-event 10)))
      (should (null result)))))

;; ---------------------------------------------------------------------
;; Snapshot aggregation
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-xtdb-clicks-snapshot-all-loaded ()
  ;; Tests bind stream-types explicitly so the test is independent of
  ;; the production default-list size.
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("code/v05/watcher-event" . :watcher-event)
           ("futon1a/invoke-job"     . :invoke-job))))
    (arxana-vsatarcs-xtdb-clicks-test--with-mock
        (list (cons "code/v05/watcher-event"
                    (arxana-vsatarcs-xtdb-clicks-test--mk-response 5))
              (cons "futon1a/invoke-job"
                    (arxana-vsatarcs-xtdb-clicks-test--mk-response 2)))
      (let* ((snap (arxana-vsatarcs-xtdb-clicks-snapshot))
             (streams (plist-get snap :streams)))
        (should (= 2 (length streams)))
        (dolist (s streams)
          (should (plist-get s :stream-loaded?)))
        (should (= 5 (plist-get (nth 0 streams) :total-in-stream)))
        (should (= 2 (plist-get (nth 1 streams) :total-in-stream)))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-snapshot-one-stream-unavail ()
  ;; First stream returns response; second stream errors → degenerate.
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("code/v05/watcher-event" . :watcher-event)
           ("futon1a/invoke-job"     . :invoke-job))))
    (arxana-vsatarcs-xtdb-clicks-test--with-mock
        (list (cons "code/v05/watcher-event"
                    (arxana-vsatarcs-xtdb-clicks-test--mk-response 3))
              (cons "futon1a/invoke-job" nil))
      (let* ((snap (arxana-vsatarcs-xtdb-clicks-snapshot))
             (streams (plist-get snap :streams)))
        (should (= 2 (length streams)))
        (should (plist-get (nth 0 streams) :stream-loaded?))
        (should-not (plist-get (nth 1 streams) :stream-loaded?))
        (should (null (plist-get (nth 1 streams) :total-in-stream)))
        (should (null (plist-get (nth 1 streams) :records)))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-snapshot-shape-stable-all-unavail ()
  ;; Server down across the board — snapshot still has stable shape.
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("code/v05/watcher-event" . :watcher-event)
           ("futon1a/invoke-job"     . :invoke-job))))
    (arxana-vsatarcs-xtdb-clicks-test--with-mock
        (list (cons "code/v05/watcher-event" nil)
              (cons "futon1a/invoke-job" nil))
      (let* ((snap (arxana-vsatarcs-xtdb-clicks-snapshot))
             (streams (plist-get snap :streams)))
        (should (= 2 (length streams)))
        (dolist (s streams)
          (should-not (plist-get s :stream-loaded?)))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-snapshot-custom-limit ()
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("code/v05/watcher-event" . :watcher-event))))
    (arxana-vsatarcs-xtdb-clicks-test--with-mock
        (list (cons "code/v05/watcher-event"
                    (arxana-vsatarcs-xtdb-clicks-test--mk-response 5)))
      (let ((snap (arxana-vsatarcs-xtdb-clicks-snapshot 25)))
        (should (= 25 (plist-get snap :limit)))))))

;; ---------------------------------------------------------------------
;; Wider defaults (v0.5.19)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-xtdb-clicks-default-stream-types-count ()
  ;; v0.5.19 declares 6 default stream types — operator extends or
  ;; replaces freely; this test guards the count so adding/removing
  ;; defaults is a closure-worthy schema change.
  (should (= 6 (length arxana-vsatarcs-xtdb-clicks-stream-types))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-default-stream-types-include-multi-watcher ()
  ;; The watcher-event family + invoke-job + forum-post are the
  ;; substrate-source-of-truth defaults; verify each.
  (let ((hx-types (mapcar #'car arxana-vsatarcs-xtdb-clicks-stream-types)))
    (dolist (t- '("code/v05/watcher-event"
                  "code/v05/heartbeat"
                  "code/v05/file-change"
                  "code/v05/cross-root-move"
                  "futon1a/invoke-job"
                  "futon3c/forum-post"))
      (should (member t- hx-types)))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-parallel-flag-default-true ()
  ;; Operator gets the parallel path by default; tests opt out.
  (should arxana-vsatarcs-xtdb-clicks-parallel-enabled))

;; ---------------------------------------------------------------------
;; Parallel path smoke (mocked async fetcher)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-xtdb-clicks-parallel-path-smoke ()
  ;; Mock `arxana-store--request-async' to fire its callback synchronously
  ;; with a stubbed response.  Verify the snapshot has the same shape as
  ;; the sequential path.
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("code/v05/watcher-event" . :watcher-event)
           ("futon1a/invoke-job"     . :invoke-job)))
        (arxana-vsatarcs-xtdb-clicks-parallel-enabled t))
    (cl-letf (((symbol-function 'arxana-store--request-async)
               (lambda (_method _path callback &optional _payload query)
                 ;; Pick response based on query-string `type=` value.
                 (let* ((type-match (when (string-match "type=\\([^&]+\\)"
                                                       (or query ""))
                                      (url-unhex-string
                                       (match-string 1 query))))
                        (resp (cond
                               ((equal type-match "code/v05/watcher-event")
                                (arxana-vsatarcs-xtdb-clicks-test--mk-response 4))
                               ((equal type-match "futon1a/invoke-job")
                                (arxana-vsatarcs-xtdb-clicks-test--mk-response 2))
                               (t nil))))
                   (funcall callback resp '(:status :ok))))))
      (let* ((snap (arxana-vsatarcs-xtdb-clicks-snapshot))
             (streams (plist-get snap :streams)))
        (should (plist-get snap :parallel?))
        (should (= 2 (length streams)))
        (should (plist-get (nth 0 streams) :stream-loaded?))
        (should (= 4 (plist-get (nth 0 streams) :total-in-stream)))
        (should (= 2 (plist-get (nth 1 streams) :total-in-stream)))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-parallel-path-handles-timeout ()
  ;; Mock async fetcher to NEVER fire the callback → wait loop hits
  ;; deadline → streams come back as unavailable.  Tight timeout so
  ;; the test stays fast.
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("code/v05/watcher-event" . :watcher-event)))
        (arxana-vsatarcs-xtdb-clicks-parallel-enabled t)
        (arxana-vsatarcs-xtdb-clicks-per-stream-timeout 0.2))
    (cl-letf (((symbol-function 'arxana-store--request-async)
               (lambda (&rest _args) nil)))
      (let* ((snap (arxana-vsatarcs-xtdb-clicks-snapshot))
             (streams (plist-get snap :streams)))
        (should (= 1 (length streams)))
        (should-not (plist-get (nth 0 streams) :stream-loaded?))))))

;; ---------------------------------------------------------------------
;; Digest line case-distinction
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-xtdb-clicks-digest-mixed ()
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("code/v05/watcher-event" . :watcher-event)
           ("futon1a/invoke-job"     . :invoke-job))))
    (arxana-vsatarcs-xtdb-clicks-test--with-mock
        (list (cons "code/v05/watcher-event"
                    (arxana-vsatarcs-xtdb-clicks-test--mk-response 3))
              (cons "futon1a/invoke-job" nil))
      (let ((line (plist-get (arxana-vsatarcs-xtdb-clicks-snapshot) :digest-line)))
        (should (string-match-p "1/2 streams loaded" line))
        (should (string-match-p "watcher-event=3" line))
        (should (string-match-p "invoke-job=unavail" line))
        (should (string-match-p "3 total records" line))))))

(ert-deftest arxana-vsatarcs-xtdb-clicks-digest-all-unavail ()
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("code/v05/watcher-event" . :watcher-event)
           ("futon1a/invoke-job"     . :invoke-job))))
    (arxana-vsatarcs-xtdb-clicks-test--with-mock
        (list (cons "code/v05/watcher-event" nil)
              (cons "futon1a/invoke-job" nil))
      (let ((line (plist-get (arxana-vsatarcs-xtdb-clicks-snapshot) :digest-line)))
        (should (string-match-p "0/2 streams loaded" line))
        (should (string-match-p "0 total records" line))))))

;; ---------------------------------------------------------------------
;; Custom stream-types config
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-xtdb-clicks-stream-types-custom ()
  ;; Operator extends the list with a new type (e.g., M-INC events).
  (let ((arxana-vsatarcs-xtdb-clicks-stream-types
         '(("m-inc/state-spawned" . :state-spawned))))
    (arxana-vsatarcs-xtdb-clicks-test--with-mock
        (list (cons "m-inc/state-spawned"
                    (arxana-vsatarcs-xtdb-clicks-test--mk-response 7)))
      (let* ((snap (arxana-vsatarcs-xtdb-clicks-snapshot))
             (streams (plist-get snap :streams)))
        (should (= 1 (length streams)))
        (should (eq :state-spawned
                    (plist-get (car streams) :stream-subclass)))
        (should (= 7 (plist-get (car streams) :total-in-stream)))))))

(provide 'arxana-vsatarcs-xtdb-clicks-test)
;;; arxana-vsatarcs-xtdb-clicks-test.el ends here
