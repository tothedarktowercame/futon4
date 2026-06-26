;;; arxana-vsatarcs-clock.el --- Arxana Clock view (system time-drivers) -*- lexical-binding: t; -*-

;;; Commentary:

;; The Arxana Clock: one navigable view over the system's TIME-DRIVERS — the
;; crons, systemd timers, and in-JVM cyder processes that keep the futon stack
;; running.  Answers, at a glance: what keeps this system ticking, when did each
;; last fire, and when does it fire next?
;;
;; Sibling to `arxana-vsatarcs-ledger.el' (same shape: a read-only Arxana view
;; over an EDN snapshot).  The snapshot is produced by the read-only aggregator
;; `futon3c/scripts/arxana_clock.bb' (E-arxana-clock car 1), which normalises
;; cron + systemd + cyder into `{:name :mechanism :cadence :last-fired
;; :next-fire :futon? :status}'.  Reuses the in-tree EDN reader from
;; `arxana-browser-rewrites' (no parseedn dependency).
;;
;; E-arxana-clock car 2 (`futon3c/holes/excursions/E-arxana-clock.md').

;;; Code:

(require 'arxana-browser-rewrites) ; shared EDN reader (true/false -> t/nil, interns keywords)
(require 'seq)

(defgroup arxana-vsatarcs-clock nil
  "Arxana Clock — the system's time-drivers on one surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-clock-snapshot-file
  (expand-file-name "~/code/futon3c/holes/excursions/arxana-clock-snapshot.edn")
  "EDN snapshot written by the clock aggregator."
  :type 'file :group 'arxana-vsatarcs-clock)

(defcustom arxana-clock-aggregator
  (expand-file-name "~/code/futon3c/scripts/arxana_clock.bb")
  "The read-only aggregator script; `g' re-runs it to refresh the snapshot."
  :type 'file :group 'arxana-vsatarcs-clock)

(defcustom arxana-clock-show-non-futon nil
  "When non-nil, also show non-futon OS drivers (snap, launchpadlib, …)."
  :type 'boolean :group 'arxana-vsatarcs-clock)

(defvar arxana-clock--buffer "*Arxana Clock*")

(defconst arxana-clock--mechanisms
  '((:cron    "⏰ cron — wall-clock (crontab)")
    (:systemd "⏱ systemd — user timers")
    (:cyder   "⚙ cyder — in-JVM processes"))
  "Render order + section headers, keyed by `:mechanism'.")

;; ---------------------------------------------------------------------------
;; Data
;; ---------------------------------------------------------------------------

(defun arxana-clock--read ()
  "Read the clock snapshot EDN (a plist with `:drivers'); nil if unreadable."
  (when (file-readable-p arxana-clock-snapshot-file)
    (ignore-errors (arxana-browser-rewrites--read-edn-file arxana-clock-snapshot-file))))

(defun arxana-clock--drivers (data)
  "The driver plists from snapshot DATA."
  (plist-get data :drivers))

(defun arxana-clock--regenerate ()
  "Re-run the aggregator to refresh the snapshot.  Returns t on success.
The aggregator uses absolute paths, so the working directory is irrelevant."
  (and (file-exists-p arxana-clock-aggregator)
       (zerop (call-process "bb" nil nil nil arxana-clock-aggregator "--quiet"))))

(defun arxana-clock--by-mech (drivers mech)
  (seq-filter (lambda (d) (eq (plist-get d :mechanism) mech)) drivers))

(defun arxana-clock--visible (drivers)
  (if arxana-clock-show-non-futon drivers
    (seq-filter (lambda (d) (plist-get d :futon?)) drivers)))

(defun arxana-clock--fmt (v)
  (if (or (null v) (and (stringp v) (string-empty-p v))) "—" (format "%s" v)))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(defvar arxana-clock-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "g" #'arxana-clock-refresh)
    (define-key m "f" #'arxana-clock-toggle-non-futon)
    (define-key m "E" #'arxana-clock-edit-crontab)
    (define-key m "?" #'arxana-clock-help)
    m)
  "Keymap for `arxana-clock-mode'.")

;; Reassert on reload (defvar keeps an existing keymap).
(define-key arxana-clock-mode-map "g" #'arxana-clock-refresh)
(define-key arxana-clock-mode-map "f" #'arxana-clock-toggle-non-futon)

(define-derived-mode arxana-clock-mode special-mode "Arxana-Clock"
  "Major mode for the Arxana Clock view.
\\{arxana-clock-mode-map}")

(defvar-local arxana-clock--refresh-fn nil
  "Thunk re-rendering the current Arxana Clock view (bound to `g').")

(defun arxana-clock--render-frame (body-fn &optional refresh-fn)
  (let ((buf (get-buffer-create arxana-clock--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'arxana-clock-mode) (arxana-clock-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall body-fn)
        (goto-char (point-min)))
      (when refresh-fn (setq arxana-clock--refresh-fn refresh-fn)))
    (pop-to-buffer buf)))

(defun arxana-clock--insert-driver (d)
  "Insert one driver row; non-futon rows are shadowed."
  (let* ((futon (plist-get d :futon?))
         (line (format "  %-26s  cadence %-12s  next %-26s  last %-26s%s\n"
                       (arxana-clock--fmt (plist-get d :name))
                       (arxana-clock--fmt (plist-get d :cadence))
                       (arxana-clock--fmt (plist-get d :next-fire))
                       (arxana-clock--fmt (plist-get d :last-fired))
                       (if futon "" "   · non-futon"))))
    (insert (if futon line (propertize line 'face 'shadow)))))

(defun arxana-clock-browse ()
  "Open the Arxana Clock — the system's time-drivers on one surface."
  (interactive)
  (let* ((data (arxana-clock--read))
         (drivers (arxana-clock--visible (arxana-clock--drivers data))))
    (arxana-clock--render-frame
     (lambda ()
       (if (null data)
           (insert (format "Arxana Clock — no snapshot at %s\n  Press g to generate it.\n"
                           arxana-clock-snapshot-file))
         (insert "Arxana Clock — the system's time-drivers\n")
         (insert (format "As of: %s   ·   %d driver(s) shown%s\n\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S %Z")
                         (length drivers)
                         (if arxana-clock-show-non-futon " (incl. non-futon)" " (futon-relevant)")))
         (dolist (spec arxana-clock--mechanisms)
           (let* ((mech (nth 0 spec))
                  (ds (sort (arxana-clock--by-mech drivers mech)
                            (lambda (a b)
                              (string< (or (plist-get a :next-fire) "zzz")
                                       (or (plist-get b :next-fire) "zzz"))))))
             (insert (format "%s  (%d)\n" (nth 1 spec) (length ds)))
             (if (null ds)
                 (insert "  —\n")
               (dolist (d ds) (arxana-clock--insert-driver d)))
             (insert "\n")))
         (insert "  g=refresh (re-run aggregator)  ·  f=toggle non-futon  ·  E=edit crontab  ·  ?=help\n")))
     #'arxana-clock-browse)))

;; ---------------------------------------------------------------------------
;; Commands
;; ---------------------------------------------------------------------------

(defun arxana-clock-refresh ()
  "Re-run the aggregator and re-render."
  (interactive)
  (message "Arxana Clock: refreshing…")
  (arxana-clock--regenerate)
  (if arxana-clock--refresh-fn (funcall arxana-clock--refresh-fn) (arxana-clock-browse))
  (message "Arxana Clock: refreshed."))

(defun arxana-clock-toggle-non-futon ()
  "Toggle whether non-futon OS drivers are shown."
  (interactive)
  (setq arxana-clock-show-non-futon (not arxana-clock-show-non-futon))
  (if arxana-clock--refresh-fn (funcall arxana-clock--refresh-fn) (arxana-clock-browse)))

(defun arxana-clock-edit-crontab ()
  "Open the user crontab for editing (the external cron drivers)."
  (interactive)
  (async-shell-command "crontab -e" "*crontab*"))

(defun arxana-clock-help ()
  "Describe the Arxana Clock key bindings."
  (interactive)
  (message "Arxana Clock: g=refresh  f=toggle-non-futon  E=edit-crontab  q=quit"))

(provide 'arxana-vsatarcs-clock)
;;; arxana-vsatarcs-clock.el ends here
