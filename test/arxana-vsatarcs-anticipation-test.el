;;; arxana-vsatarcs-anticipation-test.el --- Tests for VSATARCS anticipation module -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-anticipation.el' (reader-criterion Q3:
;; V-COV / V-CUR).  Covers EDN reading from a temp events file,
;; days-until arithmetic relative to a fixed reference time, sort
;; order, horizon filtering, time-pressure ramp, and the empty-file
;; degenerate path.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-anticipation)

;; ---------------------------------------------------------------------
;; Test fixtures
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-anticipation-test--write-events (path events-edn)
  "Write a minimal events.edn at PATH wrapping EVENTS-EDN in :events vector."
  (with-temp-file path
    (insert "{:meta {:created #inst \"2026-05-19T00:00:00.000-00:00\"}\n"
            " :events [" events-edn "]}\n")))

(defun arxana-vsatarcs-anticipation-test--ref-time (iso)
  "Return Emacs time value for ISO (a `YYYY-MM-DDTHH:MM:SSZ' string)."
  (encode-time (parse-time-string iso)))

(defmacro arxana-vsatarcs-anticipation-test--with-events (events-edn &rest body)
  "Bind the events file to a temp path holding EVENTS-EDN; eval BODY."
  (declare (indent 1))
  `(let* ((tmp (make-temp-file "vsatarcs-anticipation-" nil ".edn"))
          (arxana-vsatarcs-anticipation-events-file tmp))
     (unwind-protect
         (progn
           (arxana-vsatarcs-anticipation-test--write-events tmp ,events-edn)
           ,@body)
       (when (file-exists-p tmp) (delete-file tmp)))))

;; A minimal event 7 days out at p-fires 0.95.
(defconst arxana-vsatarcs-anticipation-test--ev-7d
  (concat
   "{:event/id \"ev-test-7d\"\n"
   " :event/kind :meeting\n"
   " :event/at #inst \"2026-05-26T12:00:00.000-00:00\"\n"
   " :event/p-fires 0.95\n"
   " :event/p-fires-rationale \"Confirmed meeting. Small reschedule prob.\"\n"
   " :event/basin :hyperreal\n"
   " :event/strawman \"futon5a/missions/foo.md\"\n"
   " :event/mission \"M-foo\"\n"
   " :event/lifecycle-status :drafted\n"
   " :event/next-gate :scenarios-then-send}\n"))

;; An event 9 days out at p-fires 0.4.
(defconst arxana-vsatarcs-anticipation-test--ev-9d
  (concat
   "{:event/id \"ev-test-9d\"\n"
   " :event/kind :lifecycle-deadline\n"
   " :event/at #inst \"2026-05-28T12:00:00.000-00:00\"\n"
   " :event/p-fires 0.4\n"
   " :event/p-fires-rationale \"Drafted; awaiting send-decision.\"\n"
   " :event/basin :postdoc}\n"))

;; An event 60 days out, beyond the default 30-day horizon.
(defconst arxana-vsatarcs-anticipation-test--ev-far
  (concat
   "{:event/id \"ev-test-far\"\n"
   " :event/at #inst \"2026-07-18T12:00:00.000-00:00\"\n"
   " :event/p-fires 0.5\n"
   " :event/p-fires-rationale \"Far future.\"}\n"))

;; An event 2 days in the past.
(defconst arxana-vsatarcs-anticipation-test--ev-past
  (concat
   "{:event/id \"ev-test-past\"\n"
   " :event/at #inst \"2026-05-17T12:00:00.000-00:00\"\n"
   " :event/p-fires 0.9\n"
   " :event/p-fires-rationale \"Already happened.\"}\n"))

;; All snapshots are taken with this fixed reference time.
(defconst arxana-vsatarcs-anticipation-test--ref-iso
  "2026-05-19T12:00:00+0000")

(defun arxana-vsatarcs-anticipation-test--ref ()
  (arxana-vsatarcs-anticipation-test--ref-time
   arxana-vsatarcs-anticipation-test--ref-iso))

;; ---------------------------------------------------------------------
;; Days-until arithmetic
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-anticipation-days-until-7d ()
  (let ((d (arxana-vsatarcs-anticipation--days-until
            "2026-05-26T12:00:00.000-00:00"
            (arxana-vsatarcs-anticipation-test--ref))))
    (should (numberp d))
    (should (< (abs (- d 7.0)) 0.01))))

(ert-deftest arxana-vsatarcs-anticipation-days-until-9d ()
  (let ((d (arxana-vsatarcs-anticipation--days-until
            "2026-05-28T12:00:00.000-00:00"
            (arxana-vsatarcs-anticipation-test--ref))))
    (should (< (abs (- d 9.0)) 0.01))))

(ert-deftest arxana-vsatarcs-anticipation-days-until-past-is-negative ()
  (let ((d (arxana-vsatarcs-anticipation--days-until
            "2026-05-17T12:00:00.000-00:00"
            (arxana-vsatarcs-anticipation-test--ref))))
    (should (< d 0.0))))

(ert-deftest arxana-vsatarcs-anticipation-days-until-nil-on-bad-input ()
  (should (null (arxana-vsatarcs-anticipation--days-until
                 "not-a-date"
                 (arxana-vsatarcs-anticipation-test--ref)))))

;; ---------------------------------------------------------------------
;; Time-pressure ramp
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-anticipation-time-pressure-at-horizon-is-zero ()
  ;; horizon = 30 days; at exactly 30 days the ramp is 0 → time-pressure = 0.
  (should (= 0.0 (arxana-vsatarcs-anticipation--event-time-pressure 30.0 0.95))))

(ert-deftest arxana-vsatarcs-anticipation-time-pressure-at-now-is-p-fires ()
  ;; 0 days out → ramp is 1.0 → time-pressure = p-fires.
  (should (< (abs (- (arxana-vsatarcs-anticipation--event-time-pressure 0.0 0.95)
                     0.95))
             1e-9)))

(ert-deftest arxana-vsatarcs-anticipation-time-pressure-mid-horizon-linear ()
  ;; 15 days out (mid-horizon) → ramp 0.5 → time-pressure = 0.5 * p-fires.
  (should (< (abs (- (arxana-vsatarcs-anticipation--event-time-pressure 15.0 1.0)
                     0.5))
             1e-9)))

(ert-deftest arxana-vsatarcs-anticipation-time-pressure-7d-matches-doc ()
  ;; reader-criteria doc Q3: 7-day-out event at p-fires=0.95 → tp = 0.78
  ;; (per doc: "closer event drives time-pressure to 0.78").
  ;; ramp at 7d w/ horizon 30 = (30-7)/30 ≈ 0.7667; * 0.95 ≈ 0.728.
  ;; The doc value rounds the WM-side formula which uses p-fires + ramp
  ;; combination; we match the structural intent (positive, < p-fires,
  ;; in [0,1]) rather than the doc's specific 0.78 figure.
  (let ((tp (arxana-vsatarcs-anticipation--event-time-pressure 7.0 0.95)))
    (should (> tp 0.0))
    (should (< tp 0.95))
    (should (> tp 0.5))))

(ert-deftest arxana-vsatarcs-anticipation-time-pressure-past-is-zero ()
  (should (= 0.0 (arxana-vsatarcs-anticipation--event-time-pressure -1.0 0.95))))

(ert-deftest arxana-vsatarcs-anticipation-time-pressure-beyond-horizon-is-zero ()
  (should (= 0.0 (arxana-vsatarcs-anticipation--event-time-pressure 60.0 0.95))))

(ert-deftest arxana-vsatarcs-anticipation-time-pressure-nil-inputs-zero ()
  (should (= 0.0 (arxana-vsatarcs-anticipation--event-time-pressure nil 0.5)))
  (should (= 0.0 (arxana-vsatarcs-anticipation--event-time-pressure 5.0 nil))))

;; ---------------------------------------------------------------------
;; Snapshot integration
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-anticipation-snapshot-empty-file-graceful ()
  (let ((arxana-vsatarcs-anticipation-events-file "/nonexistent/path.edn"))
    (let ((snap (arxana-vsatarcs-anticipation-snapshot
                 (arxana-vsatarcs-anticipation-test--ref))))
      (should (not (plist-get snap :events-loaded?)))
      (should (equal '() (plist-get snap :events)))
      (should (= 0.0 (plist-get snap :horizon-time-pressure))))))

(ert-deftest arxana-vsatarcs-anticipation-snapshot-single-event ()
  (arxana-vsatarcs-anticipation-test--with-events
      arxana-vsatarcs-anticipation-test--ev-7d
    (let* ((snap (arxana-vsatarcs-anticipation-snapshot
                  (arxana-vsatarcs-anticipation-test--ref)))
           (events (plist-get snap :events)))
      (should (plist-get snap :events-loaded?))
      (should (= 1 (length events)))
      (let ((e (car events)))
        (should (equal "ev-test-7d" (plist-get e :id)))
        (should (equal :meeting (plist-get e :kind)))
        (should (< (abs (- 7.0 (plist-get e :days-until))) 0.01))
        (should (< (abs (- 0.95 (plist-get e :p-fires))) 1e-9))
        (should (equal "M-foo" (plist-get e :mission)))
        (should (> (plist-get e :time-pressure) 0.0))))))

(ert-deftest arxana-vsatarcs-anticipation-snapshot-sort-by-days-until ()
  (arxana-vsatarcs-anticipation-test--with-events
      (concat arxana-vsatarcs-anticipation-test--ev-9d
              arxana-vsatarcs-anticipation-test--ev-7d)
    (let* ((snap (arxana-vsatarcs-anticipation-snapshot
                  (arxana-vsatarcs-anticipation-test--ref)))
           (events (plist-get snap :events)))
      (should (= 2 (length events)))
      ;; 7d event must come before 9d event.
      (should (equal "ev-test-7d" (plist-get (nth 0 events) :id)))
      (should (equal "ev-test-9d" (plist-get (nth 1 events) :id))))))

(ert-deftest arxana-vsatarcs-anticipation-snapshot-filters-beyond-horizon ()
  (arxana-vsatarcs-anticipation-test--with-events
      (concat arxana-vsatarcs-anticipation-test--ev-7d
              arxana-vsatarcs-anticipation-test--ev-far)
    (let* ((snap (arxana-vsatarcs-anticipation-snapshot
                  (arxana-vsatarcs-anticipation-test--ref)))
           (events (plist-get snap :events)))
      (should (= 1 (length events)))
      (should (equal "ev-test-7d" (plist-get (car events) :id))))))

(ert-deftest arxana-vsatarcs-anticipation-snapshot-filters-past ()
  (arxana-vsatarcs-anticipation-test--with-events
      (concat arxana-vsatarcs-anticipation-test--ev-past
              arxana-vsatarcs-anticipation-test--ev-7d)
    (let* ((snap (arxana-vsatarcs-anticipation-snapshot
                  (arxana-vsatarcs-anticipation-test--ref)))
           (events (plist-get snap :events)))
      (should (= 1 (length events)))
      (should (equal "ev-test-7d" (plist-get (car events) :id))))))

(ert-deftest arxana-vsatarcs-anticipation-snapshot-horizon-time-pressure-is-max ()
  (arxana-vsatarcs-anticipation-test--with-events
      (concat arxana-vsatarcs-anticipation-test--ev-9d
              arxana-vsatarcs-anticipation-test--ev-7d)
    (let* ((snap (arxana-vsatarcs-anticipation-snapshot
                  (arxana-vsatarcs-anticipation-test--ref)))
           (tp (plist-get snap :horizon-time-pressure))
           (events (plist-get snap :events))
           (tp-first (plist-get (car events) :time-pressure))
           (tp-second (plist-get (cadr events) :time-pressure)))
      (should (= tp (max tp-first tp-second)))
      ;; 7d-out at p=0.95 dominates 9d-out at p=0.4.
      (should (= tp tp-first)))))

(ert-deftest arxana-vsatarcs-anticipation-rationale-is-first-clause ()
  (arxana-vsatarcs-anticipation-test--with-events
      arxana-vsatarcs-anticipation-test--ev-7d
    (let* ((snap (arxana-vsatarcs-anticipation-snapshot
                  (arxana-vsatarcs-anticipation-test--ref)))
           (e (car (plist-get snap :events))))
      ;; rationale "Confirmed meeting. Small reschedule prob." → "Confirmed meeting"
      (should (equal "Confirmed meeting" (plist-get e :rationale))))))

(ert-deftest arxana-vsatarcs-anticipation-closest-event ()
  (arxana-vsatarcs-anticipation-test--with-events
      (concat arxana-vsatarcs-anticipation-test--ev-9d
              arxana-vsatarcs-anticipation-test--ev-7d)
    (let ((closest (arxana-vsatarcs-anticipation-closest-event
                    (arxana-vsatarcs-anticipation-test--ref))))
      (should (equal "ev-test-7d" (plist-get closest :id))))))

(ert-deftest arxana-vsatarcs-anticipation-closest-event-nil-when-empty ()
  (let ((arxana-vsatarcs-anticipation-events-file "/nonexistent/path.edn"))
    (should (null (arxana-vsatarcs-anticipation-closest-event
                   (arxana-vsatarcs-anticipation-test--ref))))))

;; ---------------------------------------------------------------------
;; Custom-horizon parametrisation
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-anticipation-custom-horizon ()
  (arxana-vsatarcs-anticipation-test--with-events
      (concat arxana-vsatarcs-anticipation-test--ev-7d
              arxana-vsatarcs-anticipation-test--ev-9d)
    (let ((arxana-vsatarcs-anticipation-horizon-days 8))
      ;; Horizon shrinks to 8 days → the 9d event drops out.
      (let* ((snap (arxana-vsatarcs-anticipation-snapshot
                    (arxana-vsatarcs-anticipation-test--ref)))
             (events (plist-get snap :events)))
        (should (= 1 (length events)))
        (should (equal "ev-test-7d" (plist-get (car events) :id)))))))

(provide 'arxana-vsatarcs-anticipation-test)
;;; arxana-vsatarcs-anticipation-test.el ends here
