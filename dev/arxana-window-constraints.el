;;; arxana-window-constraints.el --- Reazon-based window validation  -*- lexical-binding: t; -*-

;;; Commentary:
;; Validate window layout constraints using Reazon (miniKanren).

;;; Code:

(defgroup arxana-window-constraints nil
  "Reazon-based validation of Arxana window layouts."
  :group 'arxana)

(defcustom arxana-window-constraints-enable nil
  "When non-nil, validate window layouts after Arxana layout changes."
  :type 'boolean
  :group 'arxana-window-constraints)

(defcustom arxana-window-constraints-width-tolerance 0.05
  "Allowed deviation when checking window width fractions."
  :type 'number
  :group 'arxana-window-constraints)

(defcustom arxana-window-constraints-timeout 0.2
  "Max time in seconds for Reazon queries."
  :type 'number
  :group 'arxana-window-constraints)

(defcustom arxana-window-constraints-failure-action 'message
  "How to report failed window constraints.
Use `message' to warn, `error' to raise, or nil to ignore."
  :type '(choice (const :tag "Message" message)
                 (const :tag "Error" error)
                 (const :tag "Ignore" nil))
  :group 'arxana-window-constraints)

(defvar arxana-window-constraints--reazon-tried nil)
(defvar arxana-window-constraints--reazon-missing-reported nil)
(defvar arxana-window-constraints--relations-defined nil)

;;; Helpers

(defun arxana-window-constraints--ensure-reazon ()
  "Return non-nil when Reazon is available."
  (or (featurep 'reazon)
      (unless arxana-window-constraints--reazon-tried
        (setq arxana-window-constraints--reazon-tried t)
        (require 'reazon nil t))))

(defun arxana-window-constraints--define-relations ()
  "Define Reazon relations used for window checks."
  (unless arxana-window-constraints--relations-defined
    (setq arxana-window-constraints--relations-defined t)
    (reazon-defrel arxana-window-constraints--two-windowo (wins left right)
      (reazon-== wins `(,left ,right)))
    (reazon-defrel arxana-window-constraints--three-windowo (wins left mid right)
      (reazon-== wins `(,left ,mid ,right)))
    (reazon-defrel arxana-window-constraints--window-buffero (wininfo buffer)
      (reazon-project (wininfo)
        (reazon-== buffer (plist-get wininfo :buffer)))))
    (reazon-defrel arxana-window-constraints--left-ofo (left right)
      (reazon-project (left right)
        (reazon-== t (< (plist-get left :left)
                        (plist-get right :left)))))
    (reazon-defrel arxana-window-constraints--dedicated-o (wininfo expected ok)
      (reazon-project (wininfo expected)
        (reazon-== ok (eq (plist-get wininfo :dedicated) expected))))
    (reazon-defrel arxana-window-constraints--side-windowo
        (wininfo side ratio tolerance ok)
      (reazon-project (wininfo side ratio tolerance)
        (let* ((frame (plist-get wininfo :frame))
               (width (plist-get wininfo :width))
               (frame-width (frame-width frame))
               (actual (if (> frame-width 0)
                           (/ (float width) frame-width)
                         0.0)))
          (reazon-== ok (and (eq (plist-get wininfo :side) side)
                             (<= (abs (- actual ratio)) tolerance))))))
    (reazon-defrel arxana-window-constraints--hud-windowo
        (wininfo ratio tolerance ok)
      (reazon-project (wininfo ratio tolerance)
        (let* ((frame (plist-get wininfo :frame))
               (width (plist-get wininfo :width))
               (frame-width (frame-width frame))
               (actual (if (> frame-width 0)
                           (/ (float width) frame-width)
                         0.0)))
          (reazon-== ok (and (plist-get wininfo :hud-owner)
                             (<= (abs (- actual ratio)) tolerance))))))
    (reazon-defrel arxana-window-constraints--docbook-two-up-o
        (docinfo sourceinfo ok)
      (reazon-conde
       ((arxana-window-constraints--left-ofo docinfo sourceinfo)
        (reazon-fresh (doc-ok source-ok)
          (arxana-window-constraints--dedicated-o docinfo t doc-ok)
          (arxana-window-constraints--dedicated-o sourceinfo t source-ok)
          (reazon-== ok (and doc-ok source-ok)))))
    (reazon-defrel arxana-window-constraints--docbook-browser-left-o
        (browserinfo docinfo ok)
      (reazon-conde
       ((arxana-window-constraints--left-ofo browserinfo docinfo)
        (reazon-fresh (doc-ok)
          (arxana-window-constraints--dedicated-o docinfo t doc-ok)
          (reazon-== ok doc-ok)))))
    (reazon-defrel arxana-window-constraints--ui-focal-layouto (infos ok)
      (reazon-project (infos)
        (let ((managed nil)
              (focal nil)
              (min-left nil))
          (dolist (info infos)
            (when (plist-get info :managed)
              (push info managed)
              (let ((left (plist-get info :left)))
                (when (or (null min-left) (< left min-left))
                  (setq min-left left)))
              (when (plist-get info :focal)
                (push info focal))))
          (setq managed (nreverse managed))
          (setq focal (nreverse focal))
          (reazon-== ok (or (null managed)
                            (and (= (length focal) 1)
                                 (= (plist-get (car focal) :left) min-left)))))))
    (reazon-defrel arxana-window-constraints--scholium-layouto
        (wins code-buf source-buf display-buf ok)
      (reazon-conde
       ((reazon-fresh (left right)
          (arxana-window-constraints--two-windowo wins left right)
          (arxana-window-constraints--window-buffero left source-buf)
          (arxana-window-constraints--window-buffero right display-buf)
          (arxana-window-constraints--left-ofo left right)
          (reazon-== ok t)))
       ((reazon-fresh (left mid right)
          (arxana-window-constraints--three-windowo wins left mid right)
          (arxana-window-constraints--window-buffero left code-buf)
          (arxana-window-constraints--window-buffero mid source-buf)
          (arxana-window-constraints--window-buffero right display-buf)
          (arxana-window-constraints--left-ofo left mid)
          (arxana-window-constraints--left-ofo mid right)
          (reazon-== ok t)))))))

(defun arxana-window-constraints--ensure-relations ()
  "Load Reazon and define relations if possible."
  (when (arxana-window-constraints--ensure-reazon)
    (arxana-window-constraints--define-relations)
    t))

(defun arxana-window-constraints--report (label ok details)
  "Report constraint result with LABEL, OK, and DETAILS."
  (when (and (not ok) arxana-window-constraints-failure-action)
    (pcase arxana-window-constraints-failure-action
      ('message (message "Window constraint failed (%s): %s" label details))
      ('error (error "Window constraint failed (%s): %s" label details))
      (_ nil)))
  ok)

(defun arxana-window-constraints--window-info (win)
  "Return a plist describing WIN for validation."
  (let* ((buf (window-buffer win))
         (edges (window-edges win))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bottom (nth 3 edges))
         (managed nil)
         (focal nil))
    (with-current-buffer buf
      (when (boundp 'arxana-ui-managed)
        (setq managed arxana-ui-managed))
      (when (boundp 'arxana-ui--focal)
        (setq focal arxana-ui--focal)))
    (list :window win
          :buffer buf
          :frame (window-frame win)
          :side (window-parameter win 'window-side)
          :dedicated (window-dedicated-p win)
          :hud-owner (window-parameter win 'futon-hud-owner)
          :managed managed
          :focal focal
          :left left
          :top top
          :right right
          :bottom bottom
          :width (- right left)
          :height (- bottom top))))

(defun arxana-window-constraints--sorted-window-info (frame)
  "Return window info for FRAME sorted by left edge."
  (let ((infos (mapcar #'arxana-window-constraints--window-info
                       (window-list frame 'no-mini))))
    (sort infos (lambda (a b)
                  (< (plist-get a :left) (plist-get b :left))))))

(defun arxana-window-constraints--query (form)
  "Evaluate FORM as a Reazon query and return non-nil on success."
  (let ((reazon-timeout arxana-window-constraints-timeout))
    (condition-case _err
        (and (eval form) t)
      (error nil))))

;;; Public API

(defun arxana-window-constraints-validate-scholium (source-buffer display-buffer
                                                                 &optional code-buffer frame)
  "Validate scholium layout for SOURCE-BUFFER and DISPLAY-BUFFER."
  (if (not arxana-window-constraints-enable)
      t
    (if (not (arxana-window-constraints--ensure-relations))
        (progn
          (unless arxana-window-constraints--reazon-missing-reported
            (setq arxana-window-constraints--reazon-missing-reported t)
            (message "Reazon not available; window constraints skipped"))
          nil)
      (let* ((frame (or frame (selected-frame)))
             (windows (arxana-window-constraints--sorted-window-info frame))
             (ok (arxana-window-constraints--query
                  `(reazon-run 1 q
                     (arxana-window-constraints--scholium-layouto
                      ',windows ,code-buffer ,source-buffer ,display-buffer q)))))
        (arxana-window-constraints--report
         "scholium"
         ok
         "expected source left of display (docbook adds code pane)")))))

(defun arxana-window-constraints-validate-code-docs (docs-buffer &optional frame)
  "Validate code/docs split for DOCS-BUFFER."
  (if (not arxana-window-constraints-enable)
      t
    (if (not (arxana-window-constraints--ensure-relations))
        (progn
          (unless arxana-window-constraints--reazon-missing-reported
            (setq arxana-window-constraints--reazon-missing-reported t)
            (message "Reazon not available; window constraints skipped"))
          nil)
      (let* ((frame (or frame (selected-frame)))
             (win (get-buffer-window docs-buffer frame))
             (info (and win (arxana-window-constraints--window-info win)))
             (ok (and info
                      (arxana-window-constraints--query
                       `(reazon-run 1 q
                          (arxana-window-constraints--side-windowo
                           ',info
                           ',arxana-browser-code-docs-side
                           ,arxana-browser-code-docs-width
                           ,arxana-window-constraints-width-tolerance
                           q))))))
        (arxana-window-constraints--report
         "code-docs"
         ok
         "expected docs buffer in side window with configured width")))))

(defun arxana-window-constraints-validate-org-hover (hover-window)
  "Validate hover preview layout for HOVER-WINDOW."
  (if (not arxana-window-constraints-enable)
      t
    (if (not (arxana-window-constraints--ensure-relations))
        (progn
          (unless arxana-window-constraints--reazon-missing-reported
            (setq arxana-window-constraints--reazon-missing-reported t)
            (message "Reazon not available; window constraints skipped"))
          nil)
      (let* ((info (and (window-live-p hover-window)
                        (arxana-window-constraints--window-info hover-window)))
             (ok (and info
                      (arxana-window-constraints--query
                       `(reazon-run 1 q
                          (arxana-window-constraints--side-windowo
                           ',info
                           'right
                           ,arxana-org-links-hover-window-width
                           ,arxana-window-constraints-width-tolerance
                           q))))))
        (arxana-window-constraints--report
         "org-hover"
         ok
         "expected hover window on the right with configured width")))))

(defun arxana-window-constraints-validate-docbook-two-up (doc-buffer &optional source-buffer frame)
  "Validate docbook two-up layout for DOC-BUFFER and SOURCE-BUFFER."
  (if (not arxana-window-constraints-enable)
      t
    (if (not (arxana-window-constraints--ensure-relations))
        (progn
          (unless arxana-window-constraints--reazon-missing-reported
            (setq arxana-window-constraints--reazon-missing-reported t)
            (message "Reazon not available; window constraints skipped"))
          nil)
      (let* ((frame (or frame (selected-frame)))
             (source-buffer (or source-buffer
                                (and (boundp 'arxana-docbook--source-buffer)
                                     (get-buffer arxana-docbook--source-buffer))))
             (doc-win (and doc-buffer (get-buffer-window doc-buffer frame)))
             (source-win (and source-buffer (get-buffer-window source-buffer frame)))
             (doc-info (and doc-win (arxana-window-constraints--window-info doc-win)))
             (source-info (and source-win (arxana-window-constraints--window-info source-win)))
             (ok (and doc-info source-info
                      (arxana-window-constraints--query
                       `(reazon-run 1 q
                          (arxana-window-constraints--docbook-two-up-o
                           ',doc-info ',source-info q))))))
        (arxana-window-constraints--report
         "docbook-two-up"
         ok
         "expected doc left of source, both dedicated")))))

(defun arxana-window-constraints-validate-docbook-browser-left (browser-buffer doc-buffer
                                                                               &optional frame)
  "Validate docbook browser-left layout for BROWSER-BUFFER and DOC-BUFFER."
  (if (not arxana-window-constraints-enable)
      t
    (if (not (arxana-window-constraints--ensure-relations))
        (progn
          (unless arxana-window-constraints--reazon-missing-reported
            (setq arxana-window-constraints--reazon-missing-reported t)
            (message "Reazon not available; window constraints skipped"))
          nil)
      (let* ((frame (or frame (selected-frame)))
             (browser-win (and browser-buffer (get-buffer-window browser-buffer frame)))
             (doc-win (and doc-buffer (get-buffer-window doc-buffer frame)))
             (browser-info (and browser-win (arxana-window-constraints--window-info browser-win)))
             (doc-info (and doc-win (arxana-window-constraints--window-info doc-win)))
             (ok (and browser-info doc-info
                      (arxana-window-constraints--query
                       `(reazon-run 1 q
                          (arxana-window-constraints--docbook-browser-left-o
                           ',browser-info ',doc-info q))))))
        (arxana-window-constraints--report
         "docbook-browser-left"
         ok
         "expected browser left of docbook, docbook dedicated")))))

(defun arxana-window-constraints-validate-patterns-hud (hud-window &optional frame)
  "Validate pattern HUD window constraints."
  (if (not arxana-window-constraints-enable)
      t
    (if (not (arxana-window-constraints--ensure-relations))
        (progn
          (unless arxana-window-constraints--reazon-missing-reported
            (setq arxana-window-constraints--reazon-missing-reported t)
            (message "Reazon not available; window constraints skipped"))
          nil)
      (let* ((frame (or frame (selected-frame)))
             (hud-win (and (window-live-p hud-window) hud-window))
             (hud-info (and hud-win (arxana-window-constraints--window-info hud-win)))
             (ok (and hud-info
                      (arxana-window-constraints--query
                       `(reazon-run 1 q
                          (arxana-window-constraints--hud-windowo
                           ',hud-info
                           ,arxana-browser-patterns-hud-window-width
                           ,arxana-window-constraints-width-tolerance
                           q))))))
        (arxana-window-constraints--report
         "patterns-hud"
         ok
         "expected HUD with owner marker and configured width")))))

(defun arxana-window-constraints-validate-ui-focal (&optional frame)
  "Validate Arxana UI focal window selection in FRAME."
  (if (not arxana-window-constraints-enable)
      t
    (if (not (arxana-window-constraints--ensure-relations))
        (progn
          (unless arxana-window-constraints--reazon-missing-reported
            (setq arxana-window-constraints--reazon-missing-reported t)
            (message "Reazon not available; window constraints skipped"))
          nil)
      (let* ((frame (or frame (selected-frame)))
             (infos (arxana-window-constraints--sorted-window-info frame))
             (ok (arxana-window-constraints--query
                  `(reazon-run 1 q
                     (arxana-window-constraints--ui-focal-layouto
                      ',infos q)))))
        (arxana-window-constraints--report
         "ui-focal"
         ok
         "expected single focal managed window at leftmost edge")))))

(provide 'arxana-window-constraints)

;;; arxana-window-constraints.el ends here
