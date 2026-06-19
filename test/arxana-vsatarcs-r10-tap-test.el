;;; arxana-vsatarcs-r10-tap-test.el --- Tests for R10 file-notify tap -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-r10-tap.el' — R10 wakeup signal on WM
;; trace growth.  The tests cover:
;;   - Path filter (wm-trace EDN file detection)
;;   - Date extraction
;;   - Handler logic with synthesised events (no actual file-notify)
;;   - Start/stop idempotence
;;   - Active-p reporting
;;
;; The handler-with-real-file-notify integration path is exercised
;; manually (interactive M-x); the unit tests synthesise the event
;; tuple directly to keep tests fast + deterministic.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-belief)
(require 'arxana-vsatarcs-trace)
(require 'arxana-vsatarcs-r10-tap)

;; ---------------------------------------------------------------------
;; Path filter + date extraction
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r10-tap-wm-trace-file-p-recognises-canonical-shape ()
  (should (arxana-vsatarcs-r10-tap--wm-trace-file-p
           "/home/joe/code/futon2/data/wm-trace/wm-trace-2026-05-19.edn"))
  (should (arxana-vsatarcs-r10-tap--wm-trace-file-p
           "wm-trace-2099-12-31.edn")))

(ert-deftest arxana-vsatarcs-r10-tap-wm-trace-file-p-rejects-other-shapes ()
  (dolist (p '("/path/to/some-other-file.edn"
               "/path/to/wm-trace-bad-date.edn"
               "/path/to/wm-trace-2026-05-19.txt"
               "wm-trace-2026-05-19.edn.backup"
               ""
               nil))
    (should-not (arxana-vsatarcs-r10-tap--wm-trace-file-p p))))

(ert-deftest arxana-vsatarcs-r10-tap-date-from-path-extracts-yyyy-mm-dd ()
  (should (equal "2026-05-19"
                 (arxana-vsatarcs-r10-tap--date-from-path
                  "/dir/wm-trace-2026-05-19.edn")))
  (should (equal "1999-01-01"
                 (arxana-vsatarcs-r10-tap--date-from-path
                  "wm-trace-1999-01-01.edn")))
  (should (null (arxana-vsatarcs-r10-tap--date-from-path
                 "/dir/some-other.edn"))))

;; ---------------------------------------------------------------------
;; Handler logic (synthesised events)
;; ---------------------------------------------------------------------

(defmacro arxana-vsatarcs-r10-tap-test--with-isolated-state (&rest body)
  "Reset tap counters + state for one test; restore afterwards."
  (declare (indent 0))
  `(let ((arxana-vsatarcs-r10-tap--fire-count 0)
         (arxana-vsatarcs-r10-tap--last-fire-result nil))
     (unwind-protect (progn ,@body)
       (setq arxana-vsatarcs-r10-tap--fire-count 0
             arxana-vsatarcs-r10-tap--last-fire-result nil))))

(ert-deftest arxana-vsatarcs-r10-tap-handler-ignores-irrelevant-actions ()
  (arxana-vsatarcs-r10-tap-test--with-isolated-state
    (cl-letf* ((follow-wm-called nil)
               ((symbol-function 'arxana-vsatarcs-trace-follow-wm)
                (lambda (&optional _date) (setq follow-wm-called t) 0)))
      ;; Pass through any non-(created/changed) actions
      (dolist (action '(deleted attribute-changed renamed))
        (arxana-vsatarcs-r10-tap-handler
         (list :descriptor action "/dir/wm-trace-2026-05-19.edn")))
      (should-not follow-wm-called)
      (should (= 0 arxana-vsatarcs-r10-tap--fire-count)))))

(ert-deftest arxana-vsatarcs-r10-tap-handler-ignores-non-trace-files ()
  (arxana-vsatarcs-r10-tap-test--with-isolated-state
    (cl-letf* ((follow-wm-called nil)
               ((symbol-function 'arxana-vsatarcs-trace-follow-wm)
                (lambda (&optional _date) (setq follow-wm-called t) 0)))
      (arxana-vsatarcs-r10-tap-handler
       '(:descriptor changed "/dir/some-other-file.txt"))
      (arxana-vsatarcs-r10-tap-handler
       '(:descriptor changed "/dir/wm-trace-bad.edn"))
      (should-not follow-wm-called)
      (should (= 0 arxana-vsatarcs-r10-tap--fire-count)))))

(ert-deftest arxana-vsatarcs-r10-tap-handler-fires-follow-wm-on-changed-wm-trace ()
  (arxana-vsatarcs-r10-tap-test--with-isolated-state
    (let ((received-date nil))
      (cl-letf (((symbol-function 'arxana-vsatarcs-trace-follow-wm)
                 (lambda (&optional date) (setq received-date date) 3)))
        (arxana-vsatarcs-r10-tap-handler
         '(:descriptor changed "/dir/wm-trace-2026-05-19.edn"))
        (should (equal "2026-05-19" received-date))
        (should (= 1 arxana-vsatarcs-r10-tap--fire-count))
        (let ((result arxana-vsatarcs-r10-tap--last-fire-result))
          (should (equal 3 (plist-get result :emitted)))
          (should (equal "2026-05-19" (plist-get result :date)))
          (should (stringp (plist-get result :at))))))))

(ert-deftest arxana-vsatarcs-r10-tap-handler-fires-on-created ()
  (arxana-vsatarcs-r10-tap-test--with-isolated-state
    (cl-letf (((symbol-function 'arxana-vsatarcs-trace-follow-wm)
               (lambda (&optional _date) 1)))
      (arxana-vsatarcs-r10-tap-handler
       '(:descriptor created "/dir/wm-trace-2026-05-19.edn"))
      (should (= 1 arxana-vsatarcs-r10-tap--fire-count)))))

(ert-deftest arxana-vsatarcs-r10-tap-handler-fire-count-accumulates ()
  (arxana-vsatarcs-r10-tap-test--with-isolated-state
    (cl-letf (((symbol-function 'arxana-vsatarcs-trace-follow-wm)
               (lambda (&optional _date) 0)))
      (dotimes (_ 5)
        (arxana-vsatarcs-r10-tap-handler
         '(:descriptor changed "/dir/wm-trace-2026-05-19.edn")))
      (should (= 5 arxana-vsatarcs-r10-tap--fire-count)))))

;; ---------------------------------------------------------------------
;; Start / stop idempotence
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r10-tap-start-and-stop-idempotent ()
  (let ((tmp-dir (make-temp-file "wm-trace-r10-tap-" t)))
    (unwind-protect
        (let ((arxana-vsatarcs-r10-tap-trace-directory tmp-dir))
          ;; Make sure we start clean
          (when (arxana-vsatarcs-r10-tap-active-p)
            (arxana-vsatarcs-r10-tap-stop))
          (should-not (arxana-vsatarcs-r10-tap-active-p))
          (let ((d1 (arxana-vsatarcs-r10-tap-start)))
            (should d1)
            (should (arxana-vsatarcs-r10-tap-active-p))
            ;; Idempotent — same descriptor returned
            (let ((d2 (arxana-vsatarcs-r10-tap-start)))
              (should (eq d1 d2)))
            (arxana-vsatarcs-r10-tap-stop)
            (should-not (arxana-vsatarcs-r10-tap-active-p))
            ;; Stop again — no error
            (arxana-vsatarcs-r10-tap-stop)
            (should-not (arxana-vsatarcs-r10-tap-active-p))))
      (when (arxana-vsatarcs-r10-tap-active-p)
        (arxana-vsatarcs-r10-tap-stop))
      (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest arxana-vsatarcs-r10-tap-start-creates-missing-directory ()
  (let* ((parent (make-temp-file "wm-trace-r10-tap-parent-" t))
         (dir (expand-file-name "subdir/wm-trace/" parent)))
    (unwind-protect
        (let ((arxana-vsatarcs-r10-tap-trace-directory dir))
          (should-not (file-directory-p dir))
          (arxana-vsatarcs-r10-tap-start)
          (should (file-directory-p dir))
          (arxana-vsatarcs-r10-tap-stop))
      (when (arxana-vsatarcs-r10-tap-active-p)
        (arxana-vsatarcs-r10-tap-stop))
      (ignore-errors (delete-directory parent t)))))


(provide 'arxana-vsatarcs-r10-tap-test)
;;; arxana-vsatarcs-r10-tap-test.el ends here
