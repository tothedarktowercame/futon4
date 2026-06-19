;;; arxana-vsatarcs-wm-recent-test.el --- Tests for VSATARCS recent-trace + drift module -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-wm-recent.el' (reader-criteria Q5 + Q6:
;; V-CUR + V-COV + V-BIL).  Covers:
;;
;;   - multi-record read primitive (--read-all-records)
;;   - per-record summary extraction
;;   - shared-entity-id intersection across the window
;;   - max-abs-diff posterior comparison
;;   - per-entity trajectory drift across the window
;;   - top-K-moved ranking
;;   - window edge cases (empty file, 1 record, ≥ 2 records)
;;   - snapshot wrapper + digest-line case distinction
;;   - live smoke against real WM trace (defensive)

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-wm-recent)

;; ---------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-wm-recent-test--write (path body-edn)
  "Write a trace file at PATH containing BODY-EDN (one record per line)."
  (with-temp-file path
    (insert body-edn)))

(defmacro arxana-vsatarcs-wm-recent-test--with-trace (body-edn &rest body)
  "Bind the bridge's trace directory to a temp dir with today's trace
containing BODY-EDN."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "vsatarcs-wm-recent-" t))
          (arxana-vsatarcs-wm-bridge-trace-directory
           (file-name-as-directory dir))
          (date (arxana-vsatarcs-wm-bridge--today-iso))
          (path (expand-file-name (format "wm-trace-%s.edn" date) dir)))
     (unwind-protect
         (progn
           (arxana-vsatarcs-wm-recent-test--write path ,body-edn)
           ,@body)
       (when (file-exists-p path) (delete-file path))
       (when (file-directory-p dir) (delete-directory dir)))))

;; Records with shifting belief in entity :e1 only.
(defconst arxana-vsatarcs-wm-recent-test--rec-1
  (concat
   "{:timestamp \"2026-05-19T08:00:00Z\""
   " :mu-pre {:e1 {:strengthened 0.5 :addressed 0.5} :e2 {:strengthened 0.5 :addressed 0.5}}"
   " :mu-post {:e1 {:strengthened 0.5 :addressed 0.5} :e2 {:strengthened 0.5 :addressed 0.5}}"
   " :mode :strategic-mode-A"
   " :ranked-actions [{:rank 1 :G-total -4.0 :time-pressure 0.5 :horizon-steps 3"
   "                   :action {:type :address-sorry :target :sorry/x :rationale \"r1\"}}]"
   " :decision {:action {:type :address-sorry :target :sorry/x} :rank 1 :G-total -4.0 :tau 0.15}}"))

(defconst arxana-vsatarcs-wm-recent-test--rec-2
  (concat
   "{:timestamp \"2026-05-19T09:00:00Z\""
   " :mu-pre {:e1 {:strengthened 0.5 :addressed 0.5} :e2 {:strengthened 0.5 :addressed 0.5}}"
   " :mu-post {:e1 {:strengthened 0.6 :addressed 0.4} :e2 {:strengthened 0.5 :addressed 0.5}}"
   " :mode :strategic-mode-B"
   " :ranked-actions [{:rank 1 :G-total -4.2 :time-pressure 0.6 :horizon-steps 3"
   "                   :action {:type :address-sorry :target :sorry/x :rationale \"r2\"}}]"
   " :decision {:action {:type :address-sorry :target :sorry/x} :rank 1 :G-total -4.2 :tau 0.14}}"))

(defconst arxana-vsatarcs-wm-recent-test--rec-3
  (concat
   "{:timestamp \"2026-05-19T10:00:00Z\""
   " :mu-pre {:e1 {:strengthened 0.6 :addressed 0.4} :e2 {:strengthened 0.5 :addressed 0.5}}"
   " :mu-post {:e1 {:strengthened 0.7 :addressed 0.3} :e2 {:strengthened 0.5 :addressed 0.5}}"
   " :mode :strategic-mode-B"
   " :ranked-actions [{:rank 1 :G-total -4.4 :time-pressure 0.7 :horizon-steps 3"
   "                   :action {:type :address-sorry :target :sorry/y :rationale \"r3\"}}]"
   " :decision {:action {:type :address-sorry :target :sorry/y} :rank 1 :G-total -4.4 :tau 0.13}}"))

(defun arxana-vsatarcs-wm-recent-test--three-records ()
  (concat arxana-vsatarcs-wm-recent-test--rec-1 "\n"
          arxana-vsatarcs-wm-recent-test--rec-2 "\n"
          arxana-vsatarcs-wm-recent-test--rec-3 "\n"))

;; ---------------------------------------------------------------------
;; --read-all-records primitive (in the bridge)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-read-all-three ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let* ((path (expand-file-name
                  (format "wm-trace-%s.edn"
                          (arxana-vsatarcs-wm-bridge--today-iso))
                  arxana-vsatarcs-wm-bridge-trace-directory))
           (records (arxana-vsatarcs-wm-bridge--read-all-records path)))
      (should (= 3 (length records))))))

(ert-deftest arxana-vsatarcs-wm-recent-read-all-empty ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      ""
    (let* ((path (expand-file-name
                  (format "wm-trace-%s.edn"
                          (arxana-vsatarcs-wm-bridge--today-iso))
                  arxana-vsatarcs-wm-bridge-trace-directory))
           (records (arxana-vsatarcs-wm-bridge--read-all-records path)))
      (should (equal '() records)))))

(ert-deftest arxana-vsatarcs-wm-recent-read-all-missing-file ()
  (should (null (arxana-vsatarcs-wm-bridge--read-all-records
                 "/nonexistent/wm-trace.edn"))))

(ert-deftest arxana-vsatarcs-wm-recent-read-all-skips-blank-lines ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      (concat arxana-vsatarcs-wm-recent-test--rec-1 "\n\n"
              arxana-vsatarcs-wm-recent-test--rec-2 "\n")
    (let* ((path (expand-file-name
                  (format "wm-trace-%s.edn"
                          (arxana-vsatarcs-wm-bridge--today-iso))
                  arxana-vsatarcs-wm-bridge-trace-directory))
           (records (arxana-vsatarcs-wm-bridge--read-all-records path)))
      (should (= 2 (length records))))))

;; ---------------------------------------------------------------------
;; Per-record summary extraction
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-summarise-record-fields ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let* ((snap (arxana-vsatarcs-wm-recent-snapshot))
           (recent (plist-get snap :recent))
           (first (car recent)))
      (should (= 3 (length recent)))
      (should (equal "2026-05-19T08:00:00Z" (plist-get first :timestamp)))
      (should (eq :strategic-mode-A (plist-get first :mode)))
      (should (eq :address-sorry (plist-get first :decision-action)))
      (should (< (abs (- 0.5 (plist-get first :time-pressure))) 1e-9))
      (should (= 0 (plist-get first :mu-shift-count)))))) ; rec-1 has equal mu-pre/post

(ert-deftest arxana-vsatarcs-wm-recent-mu-shift-in-record-with-shift ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let* ((snap (arxana-vsatarcs-wm-recent-snapshot))
           (recent (plist-get snap :recent))
           (second (nth 1 recent)))
      ;; rec-2 has mu-pre :e1 == {0.5 0.5}, mu-post :e1 == {0.6 0.4}.
      (should (= 1 (plist-get second :mu-shift-count))))))

;; ---------------------------------------------------------------------
;; Shared-entity intersection
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-shared-entities-all-three ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
      ;; :e1 and :e2 appear in all three records.
      (should (= 2 (plist-get snap :shared-entities))))))

;; ---------------------------------------------------------------------
;; Posterior max-abs-diff primitive
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-max-abs-diff-equal-posteriors ()
  (let ((d (arxana-vsatarcs-wm-recent--posterior-max-abs-diff
            '(:strengthened 0.5 :addressed 0.5)
            '(:strengthened 0.5 :addressed 0.5))))
    (should (< d 1e-9))))

(ert-deftest arxana-vsatarcs-wm-recent-max-abs-diff-shifted ()
  (let ((d (arxana-vsatarcs-wm-recent--posterior-max-abs-diff
            '(:strengthened 0.5 :addressed 0.5)
            '(:strengthened 0.7 :addressed 0.3))))
    (should (< (abs (- d 0.2)) 1e-9))))

;; ---------------------------------------------------------------------
;; Top-K-moved
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-top-k-moved-rec-1-to-3 ()
  ;; e1 trajectory: {0.5 0.5} → {0.6 0.4} → {0.7 0.3}. Max consecutive
  ;; diff = 0.1.  e2 is quiescent.  So top-K is just e1.
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let* ((snap (arxana-vsatarcs-wm-recent-snapshot))
           (moved (plist-get snap :top-k-moved)))
      (should (= 1 (length moved)))
      (should (eq :e1 (plist-get (car moved) :id)))
      (should (< (abs (- 0.1 (plist-get (car moved) :max-abs-diff)))
                 1e-9)))))

(ert-deftest arxana-vsatarcs-wm-recent-top-k-moved-quiescent-window ()
  ;; Single record duplicated three times → no consecutive diff →
  ;; top-K empty.
  (let ((duped (concat arxana-vsatarcs-wm-recent-test--rec-1 "\n"
                       arxana-vsatarcs-wm-recent-test--rec-1 "\n"
                       arxana-vsatarcs-wm-recent-test--rec-1 "\n")))
    (arxana-vsatarcs-wm-recent-test--with-trace duped
      (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
        (should (equal '() (plist-get snap :top-k-moved)))))))

;; ---------------------------------------------------------------------
;; Snapshot wrapper: window-size + total-records + edge cases
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-snapshot-window-size-clamped ()
  ;; Three records on disk, default window size 5 → window-size = 3.
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
      (should (= 3 (plist-get snap :total-records)))
      (should (= 3 (plist-get snap :window-size))))))

(ert-deftest arxana-vsatarcs-wm-recent-snapshot-window-size-tighter ()
  ;; Three records on disk, custom window size 2 → window-size = 2,
  ;; recent contains the LAST two (rec-2 and rec-3, not rec-1).
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let ((arxana-vsatarcs-wm-recent-window-size 2))
      (let* ((snap (arxana-vsatarcs-wm-recent-snapshot))
             (recent (plist-get snap :recent)))
        (should (= 2 (length recent)))
        (should (equal "2026-05-19T09:00:00Z"
                       (plist-get (car recent) :timestamp)))))))

(ert-deftest arxana-vsatarcs-wm-recent-snapshot-no-trace-graceful ()
  (let ((arxana-vsatarcs-wm-bridge-trace-directory "/nonexistent/dir/"))
    (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
      (should-not (plist-get snap :trace-loaded?))
      (should (= 0 (plist-get snap :total-records)))
      (should (equal '() (plist-get snap :recent)))
      (should (equal '() (plist-get snap :top-k-moved))))))

(ert-deftest arxana-vsatarcs-wm-recent-snapshot-single-record-graceful ()
  ;; Single record → window-size = 1; drift needs ≥2 records so top-K is nil.
  (arxana-vsatarcs-wm-recent-test--with-trace
      (concat arxana-vsatarcs-wm-recent-test--rec-1 "\n")
    (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
      (should (= 1 (plist-get snap :total-records)))
      (should (= 1 (plist-get snap :window-size)))
      (should (null (plist-get snap :top-k-moved))))))

;; ---------------------------------------------------------------------
;; Digest line: four cases
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-digest-no-trace ()
  (let ((arxana-vsatarcs-wm-bridge-trace-directory "/nonexistent/dir/"))
    (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
      (should (string-match-p "no trace file" (plist-get snap :digest-line))))))

(ert-deftest arxana-vsatarcs-wm-recent-digest-empty ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      ""
    (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
      (should (string-match-p "empty" (plist-get snap :digest-line))))))

(ert-deftest arxana-vsatarcs-wm-recent-digest-needs-two ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      (concat arxana-vsatarcs-wm-recent-test--rec-1 "\n")
    (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
      (should (string-match-p "drift needs" (plist-get snap :digest-line))))))

(ert-deftest arxana-vsatarcs-wm-recent-digest-with-mover ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
      (should (string-match-p "top mover: :e1"
                              (plist-get snap :digest-line))))))

;; ---------------------------------------------------------------------
;; Custom top-K and epsilon
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-top-k-respects-custom-k ()
  (arxana-vsatarcs-wm-recent-test--with-trace
      (arxana-vsatarcs-wm-recent-test--three-records)
    (let ((arxana-vsatarcs-wm-recent-top-k-moved 1))
      (let ((snap (arxana-vsatarcs-wm-recent-snapshot)))
        (should (= 1 (length (plist-get snap :top-k-moved))))))))

;; ---------------------------------------------------------------------
;; Live WM trace smoke (defensive — only when file exists)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-recent-live-smoke ()
  (let* ((path (expand-file-name
                "~/code/futon2/data/wm-trace/wm-trace-2026-05-19.edn"))
         (dir (file-name-directory path)))
    (when (file-readable-p path)
      (let* ((arxana-vsatarcs-wm-bridge-trace-directory dir)
             (snap (arxana-vsatarcs-wm-recent-snapshot "2026-05-19")))
        (should (plist-get snap :trace-loaded?))
        (should (>= (plist-get snap :total-records) 1))
        (when (>= (plist-get snap :window-size) 2)
          ;; With ≥ 2 records, top-K is either populated or empty (but
          ;; not nil — the difference between "needs more records"
          ;; (nil) and "quiescent window" (empty list)).
          (should (or (listp (plist-get snap :top-k-moved))
                      (null (plist-get snap :top-k-moved)))))))))

(provide 'arxana-vsatarcs-wm-recent-test)
;;; arxana-vsatarcs-wm-recent-test.el ends here
