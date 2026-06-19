;;; arxana-vsatarcs-wm-decision-test.el --- Tests for VSATARCS WM-decision surfacing -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-wm-decision.el' (reader-criterion Q2:
;; V-CUR + V-COV).  Covers summary extraction from a WM trace record,
;; absence-of-decision handling, digest formatting across the
;; (trace-missing / record-empty / decision-missing / decision-present)
;; cases, and the snapshot wrapper.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-wm-decision)

;; ---------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-wm-decision-test--write (path body-edn)
  "Write a trace file at PATH containing one EDN record line BODY-EDN."
  (with-temp-file path
    (insert body-edn "\n")))

(defmacro arxana-vsatarcs-wm-decision-test--with-trace (body-edn &rest body)
  "Write a temp trace file holding BODY-EDN; eval BODY with the bridge's
trace directory rebound so the snapshot reads it.  The temp file lives
at <dir>/wm-trace-YYYY-MM-DD.edn with YYYY-MM-DD taken from
`arxana-vsatarcs-wm-bridge--today-iso'."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "vsatarcs-wm-decision-" t))
          (arxana-vsatarcs-wm-bridge-trace-directory
           (file-name-as-directory dir))
          (date (arxana-vsatarcs-wm-bridge--today-iso))
          (path (expand-file-name (format "wm-trace-%s.edn" date) dir)))
     (unwind-protect
         (progn
           (arxana-vsatarcs-wm-decision-test--write path ,body-edn)
           ,@body)
       (when (file-exists-p path) (delete-file path))
       (when (file-directory-p dir) (delete-directory dir)))))

;; WM trace files use EDN-LINES format: each complete record is one
;; line.  Fixtures below are single-line so the bridge's
;; `--read-last-record' (which reads the file's last line) sees a full
;; record.  Real WM traces (e.g., ~/code/futon2/data/wm-trace/) follow
;; the same convention — one EDN form per line.

(defconst arxana-vsatarcs-wm-decision-test--record-with-decision
  (concat
   "{:timestamp \"2026-05-19T12:04:15.080079890Z\""
   " :mu-post {:e1 {:strengthened 0.5 :addressed 0.5}}"
   " :mode :strategic-mode-A"
   " :decision {:action {:type :address-sorry"
   "                     :target :sorry/wm-aif-substrate-addressability"
   "                     :weight 1.0"
   "                     :rationale \"open sorry: WM action types need addressable substrate; meta-sorry registers itself as the first sorry\"}"
   "            :rank 1"
   "            :G-total -4.208103685546497"
   "            :tau 0.16379890756944454}}"))

(defconst arxana-vsatarcs-wm-decision-test--record-without-decision
  (concat
   "{:timestamp \"2026-05-19T08:00:00Z\""
   " :mu-post {:e1 {:strengthened 0.5 :addressed 0.5}}}"))

;; ---------------------------------------------------------------------
;; Summarise: decision-bearing record
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-decision-summarise-action-type ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-decision
    (let* ((snap (arxana-vsatarcs-wm-decision-snapshot))
           (s (plist-get snap :summary)))
      (should (plist-get s :decision-present?))
      (should (eq :address-sorry (plist-get s :action-type))))))

(ert-deftest arxana-vsatarcs-wm-decision-summarise-target ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-decision
    (let* ((snap (arxana-vsatarcs-wm-decision-snapshot))
           (s (plist-get snap :summary)))
      (should (eq :sorry/wm-aif-substrate-addressability
                  (plist-get s :target))))))

(ert-deftest arxana-vsatarcs-wm-decision-summarise-rank-and-G ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-decision
    (let* ((snap (arxana-vsatarcs-wm-decision-snapshot))
           (s (plist-get snap :summary)))
      (should (= 1 (plist-get s :rank)))
      (should (< (abs (- -4.208103685546497 (plist-get s :G-total))) 1e-9))
      (should (< (abs (- 0.16379890756944454 (plist-get s :tau))) 1e-9)))))

(ert-deftest arxana-vsatarcs-wm-decision-summarise-mode ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-decision
    (let* ((snap (arxana-vsatarcs-wm-decision-snapshot))
           (s (plist-get snap :summary)))
      (should (eq :strategic-mode-A (plist-get s :mode))))))

(ert-deftest arxana-vsatarcs-wm-decision-summarise-rationale ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-decision
    (let* ((snap (arxana-vsatarcs-wm-decision-snapshot))
           (s (plist-get snap :summary)))
      (should (stringp (plist-get s :rationale)))
      (should (string-match-p "open sorry" (plist-get s :rationale))))))

(ert-deftest arxana-vsatarcs-wm-decision-summarise-weight ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-decision
    (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
      (should (numberp (plist-get s :weight))))))

(ert-deftest arxana-vsatarcs-wm-decision-summarise-timestamp ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-decision
    (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
      (should (equal "2026-05-19T12:04:15.080079890Z"
                     (plist-get s :timestamp))))))

;; ---------------------------------------------------------------------
;; Absence-of-decision handling
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-decision-record-without-decision ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-without-decision
    (let* ((snap (arxana-vsatarcs-wm-decision-snapshot))
           (s (plist-get snap :summary)))
      (should (plist-get snap :record-present?))
      (should-not (plist-get snap :decision-present?))
      (should-not (plist-get s :decision-present?))
      (should (null (plist-get s :action-type)))
      (should (null (plist-get s :rank)))
      (should (null (plist-get s :G-total))))))

(ert-deftest arxana-vsatarcs-wm-decision-no-trace-file ()
  (let ((arxana-vsatarcs-wm-bridge-trace-directory "/nonexistent/dir/"))
    (let ((snap (arxana-vsatarcs-wm-decision-snapshot)))
      (should-not (plist-get snap :trace-loaded?))
      (should-not (plist-get snap :record-present?))
      (should-not (plist-get snap :decision-present?)))))

(ert-deftest arxana-vsatarcs-wm-decision-empty-trace ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      ""
    (let ((snap (arxana-vsatarcs-wm-decision-snapshot)))
      (should (plist-get snap :trace-loaded?))
      (should-not (plist-get snap :record-present?))
      (should-not (plist-get snap :decision-present?)))))

;; ---------------------------------------------------------------------
;; Digest line: distinguishes the four cases
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-decision-digest-no-trace ()
  (let ((arxana-vsatarcs-wm-bridge-trace-directory "/nonexistent/dir/"))
    (let ((snap (arxana-vsatarcs-wm-decision-snapshot)))
      (should (string-match-p "no trace file" (plist-get snap :digest-line))))))

(ert-deftest arxana-vsatarcs-wm-decision-digest-empty-trace ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      ""
    (let ((snap (arxana-vsatarcs-wm-decision-snapshot)))
      (should (string-match-p "empty" (plist-get snap :digest-line))))))

(ert-deftest arxana-vsatarcs-wm-decision-digest-no-decision-field ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-without-decision
    (let ((snap (arxana-vsatarcs-wm-decision-snapshot)))
      (should (string-match-p "no :decision" (plist-get snap :digest-line))))))

(ert-deftest arxana-vsatarcs-wm-decision-digest-decision-present ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-decision
    (let ((snap (arxana-vsatarcs-wm-decision-snapshot)))
      (let ((line (plist-get snap :digest-line)))
        (should (string-match-p ":address-sorry" line))
        (should (string-match-p "rank 1" line))
        (should (string-match-p "G=-4" line))
        (should (string-match-p "tau=0" line))
        (should (string-match-p "mode :strategic-mode-A" line))))))

;; ---------------------------------------------------------------------
;; Date parameter — read alternate-day trace
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-decision-explicit-date ()
  (let* ((dir (make-temp-file "vsatarcs-wm-decision-date-" t))
         (arxana-vsatarcs-wm-bridge-trace-directory
          (file-name-as-directory dir))
         (path (expand-file-name "wm-trace-2026-05-15.edn" dir)))
    (unwind-protect
        (progn
          (arxana-vsatarcs-wm-decision-test--write
           path arxana-vsatarcs-wm-decision-test--record-with-decision)
          (let ((snap (arxana-vsatarcs-wm-decision-snapshot "2026-05-15")))
            (should (plist-get snap :decision-present?))
            (should (equal "2026-05-15" (plist-get snap :trace-date)))))
      (when (file-exists-p path) (delete-file path))
      (when (file-directory-p dir) (delete-directory dir)))))

;; ---------------------------------------------------------------------
;; Top-K alternatives + composition (per claude-2 2026-05-20 handoff)
;; ---------------------------------------------------------------------

;; Fixture record with 4 ranked actions for top-K testing.
(defconst arxana-vsatarcs-wm-decision-test--record-with-ranked-actions
  (concat
   "{:timestamp \"2026-05-19T12:04:15Z\""
   " :mu-pre {:e1 {:strengthened 0.5 :addressed 0.5}}"
   " :mu-post {:e1 {:strengthened 0.6 :addressed 0.4}}"
   " :mode :strategic-mode-A"
   " :ranked-actions [{:rank 1 :G-total -4.21 :time-pressure 0.78 :horizon-steps 3"
   "                   :action {:type :address-sorry :target :sorry/x :weight 1.0 :rationale \"R1\"}}"
   "                  {:rank 2 :G-total -3.97 :time-pressure 0.78 :horizon-steps 3"
   "                   :action {:type :address-sorry :target :sorry/y :weight 0.3 :rationale \"R2\"}}"
   "                  {:rank 3 :G-total -3.50 :time-pressure 0.78 :horizon-steps 3"
   "                   :action {:type :learn-action-class :target :fire-pattern :weight 0.1 :rationale \"R3\"}}"
   "                  {:rank 4 :G-total -3.10 :time-pressure 0.78 :horizon-steps 3"
   "                   :action {:type :open-mission :target :M-foo :weight 0.05 :rationale \"R4\"}}]"
   " :decision {:action {:type :address-sorry :target :sorry/x :weight 1.0 :rationale \"chosen\"}"
   "            :rank 1 :G-total -4.21 :tau 0.16}}"))

(ert-deftest arxana-vsatarcs-wm-decision-top-k-count ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-ranked-actions
    (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
      ;; Default K=3
      (should (= 3 (length (plist-get s :top-k)))))))

(ert-deftest arxana-vsatarcs-wm-decision-top-k-order ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-ranked-actions
    (let* ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary))
           (top-k (plist-get s :top-k))
           (ranks (mapcar (lambda (a) (plist-get a :rank)) top-k)))
      (should (equal '(1 2 3) ranks)))))

(ert-deftest arxana-vsatarcs-wm-decision-top-k-content ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-ranked-actions
    (let* ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary))
           (top-k (plist-get s :top-k)))
      ;; First alternative (rank 2) — same action type, different target.
      (should (eq :address-sorry (plist-get (nth 1 top-k) :action-type)))
      (should (eq :sorry/y (plist-get (nth 1 top-k) :target)))
      ;; Third alternative (rank 3) — different action type.
      (should (eq :learn-action-class
                  (plist-get (nth 2 top-k) :action-type))))))

(ert-deftest arxana-vsatarcs-wm-decision-top-k-custom-k ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-ranked-actions
    (let ((arxana-vsatarcs-wm-decision-top-k 2))
      (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
        (should (= 2 (length (plist-get s :top-k))))))))

(ert-deftest arxana-vsatarcs-wm-decision-chosen-time-pressure ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-ranked-actions
    (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
      (should (< (abs (- 0.78 (plist-get s :chosen-time-pressure))) 1e-9)))))

(ert-deftest arxana-vsatarcs-wm-decision-horizon-steps ()
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-ranked-actions
    (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
      (should (= 3 (plist-get s :horizon-steps))))))

(ert-deftest arxana-vsatarcs-wm-decision-mu-shift-count-positive ()
  ;; Fixture has e1 mu-pre {0.5 0.5} vs mu-post {0.6 0.4} → 1 entity shifted.
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-with-ranked-actions
    (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
      (should (= 1 (plist-get s :mu-shift-count))))))

(ert-deftest arxana-vsatarcs-wm-decision-mu-shift-count-zero-when-equal ()
  ;; mu-pre == mu-post → 0 shift.
  (arxana-vsatarcs-wm-decision-test--with-trace
      (concat
       "{:timestamp \"2026-05-19T12:04:15Z\""
       " :mu-pre {:e1 {:strengthened 0.5 :addressed 0.5}}"
       " :mu-post {:e1 {:strengthened 0.5 :addressed 0.5}}"
       " :ranked-actions []"
       " :decision {:action {:type :abstain} :rank 1 :G-total 0.0 :tau 0.1}}")
    (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
      (should (= 0 (plist-get s :mu-shift-count))))))

(ert-deftest arxana-vsatarcs-wm-decision-top-k-absent-when-no-decision ()
  ;; No-decision record → top-k is nil (not just an empty list).
  (arxana-vsatarcs-wm-decision-test--with-trace
      arxana-vsatarcs-wm-decision-test--record-without-decision
    (let ((s (plist-get (arxana-vsatarcs-wm-decision-snapshot) :summary)))
      (should (null (plist-get s :top-k))))))

;; ---------------------------------------------------------------------
;; Live WM trace smoke (defensive — only when file exists)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-wm-decision-live-smoke ()
  (let* ((path (expand-file-name
                "~/code/futon2/data/wm-trace/wm-trace-2026-05-19.edn"))
         (dir (file-name-directory path)))
    (when (file-readable-p path)
      (let* ((arxana-vsatarcs-wm-bridge-trace-directory dir)
             (snap (arxana-vsatarcs-wm-decision-snapshot "2026-05-19")))
        (should (plist-get snap :trace-loaded?))
        (should (plist-get snap :record-present?))
        (should (plist-get snap :decision-present?))
        (let ((s (plist-get snap :summary)))
          (should (eq :address-sorry (plist-get s :action-type)))
          (should (eq :sorry/wm-aif-substrate-addressability
                      (plist-get s :target)))
          (should (= 1 (plist-get s :rank))))))))

(provide 'arxana-vsatarcs-wm-decision-test)
;;; arxana-vsatarcs-wm-decision-test.el ends here
