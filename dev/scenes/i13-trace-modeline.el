;;; i13-trace-modeline.el --- I13 acceptance: live trace rules + 象 modeline -*- lexical-binding: t; -*-

;; INTERACT-1 I13: the turn rules in futon3c/emacs/xiang-trace.el run on
;; EVERY recorded event, and a violation shows in the 象 modeline segment.
;; Scenes A/B/C per the requisition (E-kimi-task-136).  No agent is
;; driven; the scenes plant trace events and record through
;; `xiang-trace-record', then read the modeline state and the rendered
;; lighter (session-mode--analysis-lighter with the xiang-trace advice).

(require 'arxana-dramaturge-scene)
;; -Q daemon: no package-initialize, so reazon (xiang-trace's rule
;; engine) is not on the load-path yet.  Find the ELPA install.
(let ((dir (car (file-expand-wildcards
                 (expand-file-name "~/.emacs.d/elpa/reazon-*")))))
  (when dir (add-to-list 'load-path dir)))
(require 'session-mode)            ; the 象 lighter we sit beside
(require 'xiang-trace)

(defun i13--with-clean-trace (open-after f)
  "Run F with an empty in-memory trace, a temp trace file, OPEN-AFTER."
  (let ((xiang-trace-file (make-temp-file "i13-trace" nil ".jsonl"))
        (xiang-trace--events nil)
        (xiang-trace--violations nil)
        (xiang-trace--live-check-error nil)
        (xiang-trace-open-after open-after)
        (session-mode--analysis-health nil)
        (session-mode--analysis-health-detail nil))
    (unwind-protect
        (funcall f)
      (setq xiang-trace--events nil
            xiang-trace--violations nil
            xiang-trace--live-check-error nil)
      (delete-file xiang-trace-file))))

(defun i13--segment-text ()
  "The rendered violation segment as plain text, or nil."
  (let ((s (xiang-trace--modeline-segment)))
    (and s (substring-no-properties s))))

(defun i13--lighter-text ()
  "The rendered 象 lighter (health + violation segment) as plain text."
  (substring-no-properties (session-mode--analysis-lighter)))

(arxana-dramaturge-scene i13-a-violation-after-next-event
  "A: an overdue reply-ended with no dispatch is open while it stands
alone; after the NEXT recorded event the modeline names the rule."
  (i13--with-clean-trace
   10                                   ; open window: 10 s
   (lambda ()
     ;; Plant the overdue reply end directly (older than the window).
     (push (list :at (- (float-time) 3600) :kind 'reply-ended
                 :path "t1.json" :session "s")
           xiang-trace--events)
     (assert (lambda () (null (i13--segment-text)))
             "before any new event, nothing is shown")
     ;; The next recorded event re-runs the rules.
     (xiang-trace-record 'sent "/tmp/t2.json" :session "s")
     (assert (lambda ()
               (equal '(("reply end → dispatch" "t1.json" violation))
                      xiang-trace--violations))
             "modeline state names reply end → dispatch as a violation")
     (assert (lambda () (equal "!reply end → dispatch ×1" (i13--segment-text)))
             "segment shows the rule name and count")
     (assert (lambda () (equal " 象!reply end → dispatch ×1" (i13--lighter-text)))
             "象 lighter carries the violation beside the health states")
     ;; mouse-1 on the segment is bound to xiang-trace-check.
     (assert (lambda ()
               (eq (lookup-key (get-text-property 0 'local-map
                                                  (xiang-trace--modeline-segment))
                               [mode-line mouse-1])
                   #'xiang-trace-check))
             "mouse-1 opens *象 trace check*"))))

(arxana-dramaturge-scene i13-b-young-reply-end-is-open
  "B: the same trace with the reply end younger than the open window
shows NO violation (the step is `open', not late)."
  (i13--with-clean-trace
   900
   (lambda ()
     (xiang-trace-record 'reply-ended "/tmp/t1.json")
     (xiang-trace-record 'sent "/tmp/t2.json" :session "s")
     (assert (lambda () (null xiang-trace--violations))
             "no violation while the step is open")
     (assert (lambda () (null (i13--segment-text)))
             "nothing in the modeline")
     (assert (lambda () (equal " 象" (i13--lighter-text)))
             "lighter unchanged"))))

(arxana-dramaturge-scene i13-c-outcome-clears-the-state
  "C: a silent dispatch is a violation (window 0, so it is overdue at
once); recording the failure outcome clears the modeline state."
  (i13--with-clean-trace
   0
   (lambda ()
     (xiang-trace-record 'reply-ended "/tmp/t1.json")
     (xiang-trace-record 'dispatched "/tmp/t1.json")
     (assert (lambda () xiang-trace--violations)
             "silent dispatch shows as a violation")
     (xiang-trace-record 'failed "/tmp/t1.json")
     (assert (lambda () (null xiang-trace--violations))
             "after dispatch + failure the state clears")
     (assert (lambda () (equal " 象" (i13--lighter-text)))
             "the segment returns to normal"))))

;;; i13-trace-modeline.el ends here
