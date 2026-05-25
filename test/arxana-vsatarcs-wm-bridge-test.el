;;; arxana-vsatarcs-wm-bridge-test.el --- Tests for the cross-side WM belief bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-wm-bridge.el' — the cross-side bridge
;; that lets the VSATARCS reader compare its belief against the
;; WM-side belief read from a WM trace file.
;;
;; Fixtures use synthetic single-line EDN trace records that mirror
;; the shape claude-2's WM `judge' apparatus writes (`:timestamp',
;; `:mu-pre', `:mu-post', forward-compatible fields).

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-belief)
(require 'arxana-vsatarcs-wm-bridge)

(defconst arxana-vsatarcs-wm-bridge-test--record
  ;; Two entities + the meta-sorry keyword.  Uniform priors throughout.
  ;; Mirrors the shape of the post-v0.9 WM trace records.
  "{:timestamp \"2026-05-18T20:54:12.717822372Z\", :mu-pre {\"arxana/test/leaf/1\" {:strengthened 0.14285714285714285, :addressed 0.14285714285714285, :falsified 0.14285714285714285, :reopened 0.14285714285714285, :spawned 0.14285714285714285, :refined 0.14285714285714285, :foreclosed 0.14285714285714285}, \"arxana/test/leaf/2\" {:strengthened 0.14285714285714285, :addressed 0.14285714285714285, :falsified 0.14285714285714285, :reopened 0.14285714285714285, :spawned 0.14285714285714285, :refined 0.14285714285714285, :foreclosed 0.14285714285714285}, :sorry/wm-aif-substrate-addressability {:strengthened 0.14285714285714285, :addressed 0.14285714285714285, :falsified 0.14285714285714285, :reopened 0.14285714285714285, :spawned 0.14285714285714285, :refined 0.14285714285714285, :foreclosed 0.14285714285714285}}, :mu-post {\"arxana/test/leaf/1\" {:strengthened 0.14285714285714285, :addressed 0.14285714285714285, :falsified 0.14285714285714285, :reopened 0.14285714285714285, :spawned 0.14285714285714285, :refined 0.14285714285714285, :foreclosed 0.14285714285714285}, \"arxana/test/leaf/2\" {:strengthened 0.14285714285714285, :addressed 0.14285714285714285, :falsified 0.14285714285714285, :reopened 0.14285714285714285, :spawned 0.14285714285714285, :refined 0.14285714285714285, :foreclosed 0.14285714285714285}, :sorry/wm-aif-substrate-addressability {:strengthened 0.14285714285714285, :addressed 0.14285714285714285, :falsified 0.14285714285714285, :reopened 0.14285714285714285, :spawned 0.14285714285714285, :refined 0.14285714285714285, :foreclosed 0.14285714285714285}}}")

(defmacro arxana-vsatarcs-wm-bridge-test--with-fixture-trace (path-var date &rest body)
  "Bind PATH-VAR to a temp trace file containing the fixture; run BODY.
DATE is the YYYY-MM-DD string used to name the file."
  (declare (indent 2))
  `(let* ((tmp-dir (make-temp-file "wm-trace-fixture-" t))
          (,path-var (expand-file-name
                      (format "wm-trace-%s.edn" ,date) tmp-dir))
          (arxana-vsatarcs-wm-bridge-trace-directory tmp-dir))
     (unwind-protect
         (progn
           (with-temp-file ,path-var
             (insert arxana-vsatarcs-wm-bridge-test--record "\n"))
           ,@body)
       (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest arxana-vsatarcs-wm-bridge-read-last-record-parses-mu-post ()
  (arxana-vsatarcs-wm-bridge-test--with-fixture-trace path "2026-05-18"
    (let* ((record (arxana-vsatarcs-wm-bridge--read-last-record path))
           (mu-post (plist-get record :mu-post)))
      (should record)
      (should mu-post))))

(ert-deftest arxana-vsatarcs-wm-bridge-read-wm-belief-shape ()
  (arxana-vsatarcs-wm-bridge-test--with-fixture-trace _ "2026-05-18"
    (let* ((wm (arxana-vsatarcs-wm-bridge-read-wm-belief "2026-05-18")))
      (should (= 3 (length wm)))
      (should (cl-find-if (lambda (kv) (equal (car kv) "arxana/test/leaf/1")) wm))
      (should (cl-find-if (lambda (kv) (equal (car kv) "arxana/test/leaf/2")) wm))
      (should (cl-find-if (lambda (kv)
                            (equal (car kv) :sorry/wm-aif-substrate-addressability))
                          wm)))))

(ert-deftest arxana-vsatarcs-wm-bridge-posterior-has-bare-status-keys ()
  (arxana-vsatarcs-wm-bridge-test--with-fixture-trace _ "2026-05-18"
    (let* ((wm (arxana-vsatarcs-wm-bridge-read-wm-belief "2026-05-18"))
           (post (cdr (assoc "arxana/test/leaf/1" wm))))
      (should (arxana-vsatarcs-belief-valid-posterior-p post))
      ;; Keys should be bare symbols (spawned, refined, ...), not :keywords
      (dolist (s arxana-vsatarcs-belief-status-set)
        (should (assoc s post))))))

(ert-deftest arxana-vsatarcs-wm-bridge-missing-file-errors ()
  (let ((arxana-vsatarcs-wm-bridge-trace-directory "/definitely/not/a/path/wm-trace/"))
    (should-error (arxana-vsatarcs-wm-bridge-compare-with-local nil "2099-01-01")
                  :type 'user-error)))

(ert-deftest arxana-vsatarcs-wm-bridge-compare-matches-when-local-mirrors-wm ()
  ;; Local bootstrap with the same two test ids → 2 equal, 0 drift,
  ;; meta-sorry filtered as expected-in-wm-only.
  (arxana-vsatarcs-wm-bridge-test--with-fixture-trace _ "2026-05-18"
    (arxana-vsatarcs-belief-reset)
    (setq arxana-vsatarcs-belief--current
          (arxana-vsatarcs-belief-initial-state
           '("arxana/test/leaf/1" "arxana/test/leaf/2")))
    (let ((rep (arxana-vsatarcs-wm-bridge-compare-with-local nil "2026-05-18")))
      (should (null (plist-get rep :only-in-local)))
      (should (null (plist-get rep :only-in-wm-side))) ; meta-sorry filtered
      (should (equal '(:sorry/wm-aif-substrate-addressability)
                     (plist-get rep :expected-in-wm-only)))
      (should (null (plist-get rep :posterior-diffs)))
      (should (= 2 (plist-get rep :equal-count))))
    (arxana-vsatarcs-belief-reset)))

(ert-deftest arxana-vsatarcs-wm-bridge-compare-detects-posterior-drift ()
  ;; Local has accumulated evidence on leaf/1; WM-side is uniform → drift.
  (arxana-vsatarcs-wm-bridge-test--with-fixture-trace _ "2026-05-18"
    (arxana-vsatarcs-belief-reset)
    (setq arxana-vsatarcs-belief--current
          (arxana-vsatarcs-belief-initial-state
           '("arxana/test/leaf/1" "arxana/test/leaf/2")))
    (arxana-vsatarcs-belief-ingest-events
     '((:entity-id "arxana/test/leaf/1" :type :strengthened :weight 5.0)))
    (let* ((rep (arxana-vsatarcs-wm-bridge-compare-with-local nil "2026-05-18"))
           (diffs (plist-get rep :posterior-diffs)))
      (should (= 1 (length diffs)))
      (should (equal "arxana/test/leaf/1" (car (car diffs))))
      (should (= 1 (plist-get rep :equal-count))))
    (arxana-vsatarcs-belief-reset)))

(ert-deftest arxana-vsatarcs-wm-bridge-compare-detects-only-in-local ()
  (arxana-vsatarcs-wm-bridge-test--with-fixture-trace _ "2026-05-18"
    (arxana-vsatarcs-belief-reset)
    (setq arxana-vsatarcs-belief--current
          (arxana-vsatarcs-belief-initial-state
           '("arxana/test/leaf/1" "arxana/test/leaf/2" "arxana/test/leaf/3")))
    (let ((rep (arxana-vsatarcs-wm-bridge-compare-with-local nil "2026-05-18")))
      (should (equal '("arxana/test/leaf/3") (plist-get rep :only-in-local)))
      (should (null (plist-get rep :only-in-wm-side)))
      (should (= 2 (plist-get rep :equal-count))))
    (arxana-vsatarcs-belief-reset)))

(provide 'arxana-vsatarcs-wm-bridge-test)
;;; arxana-vsatarcs-wm-bridge-test.el ends here
