;;; arxana-browser-scans.el --- Daily scan frames browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Lists completed daily-scan frames from futon5a/data/frames/.
;; Each entry is a frame-daily-scan-NNN.edn written after a daily scan
;; workup.  RET opens the frame EDN file.  LEFT/b returns to the
;; browser root.
;;
;; Entry point is wired through arxana-browser-core's top-level menu
;; as the `scans' view.  Brief and workup markdown files live in
;; futon7/data/briefs/ and are referenced by :frame/source-files within
;; each frame EDN.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup arxana-browser-scans nil
  "Daily-scan frame browser support."
  :group 'arxana)

(defcustom arxana-browser-scans-frames-directory
  (expand-file-name "~/code/futon5a/data/frames")
  "Directory scanned for `frame-daily-scan-*.edn' files."
  :type 'directory
  :group 'arxana-browser-scans)

(defcustom arxana-browser-scans-file-pattern
  "^frame-daily-scan-.*\\.edn\\'"
  "Regexp matching daily-scan frame filenames."
  :type 'regexp
  :group 'arxana-browser-scans)

(defun arxana-browser-scans--frame-paths ()
  "Return readable daily-scan frame paths, sorted."
  (let ((dir (expand-file-name arxana-browser-scans-frames-directory)))
    (when (file-directory-p dir)
      (sort (directory-files dir t arxana-browser-scans-file-pattern)
            #'string<))))

(defun arxana-browser-scans--file-contents (path)
  "Return contents of PATH as a string, or nil on error."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (error nil)))

(defun arxana-browser-scans--scan-field (text regex)
  "Return first capture group of REGEX in TEXT, or nil."
  (when (and text (string-match regex text))
    (match-string 1 text)))

(defun arxana-browser-scans--frame-item (path)
  "Build a browser item plist for frame at PATH.
Fields are extracted via regex on the EDN text: :frame/id,
:frame/day, :frame/subseries, :frame/subseries-day,
:frame/timestamp.  This avoids a full EDN parser in Emacs Lisp."
  (let* ((text (arxana-browser-scans--file-contents path))
         (id (or (arxana-browser-scans--scan-field
                  text ":frame/id[[:space:]]+\"\\([^\"]+\\)\"")
                 (file-name-base path)))
         (day (arxana-browser-scans--scan-field
               text ":frame/day[[:space:]]+\\([0-9]+\\)"))
         (subseries (arxana-browser-scans--scan-field
                     text ":frame/subseries[[:space:]]+:\\([a-zA-Z0-9_-]+\\)"))
         (sub-day (arxana-browser-scans--scan-field
                   text ":frame/subseries-day[[:space:]]+\\([0-9]+\\)"))
         (ts (arxana-browser-scans--scan-field
              text ":frame/timestamp[[:space:]]+\"\\([^\"]+\\)\""))
         (date (and ts (substring ts 0 (min 10 (length ts)))))
         (shown-day (or day sub-day))
         (day-prefix (cond
                      (subseries (format ":%s Day %s" subseries (or sub-day "?")))
                      (day (format "Day %s" day))
                      (t nil)))
         (label (cond
                 ((and day-prefix date) (format "%s — %s" day-prefix date))
                 (day-prefix day-prefix)
                 (date date)
                 (t (file-name-base path)))))
    (list :type 'scans-frame
          :label label
          :frame-id id
          :day (or shown-day "")
          :subseries (or subseries "")
          :date (or date "")
          :path path)))

(defun arxana-browser-scans-items ()
  "Return browser rows for daily-scan frames."
  (let ((paths (arxana-browser-scans--frame-paths)))
    (if paths
        (mapcar #'arxana-browser-scans--frame-item paths)
      (list (list :type 'info
                  :label "No daily-scan frames"
                  :description
                  (format "Add frame-daily-scan-*.edn to %s."
                          arxana-browser-scans-frames-directory))))))

(defun arxana-browser-scans-format ()
  "Return tabulated format for the Scans view."
  [("Series" 12 t)
   ("Day"    5  t)
   ("Date"   12 t)
   ("Frame ID" 24 t)
   ("Path"   0  nil)])

(defun arxana-browser-scans-row (item)
  "Return a tabulated row for a daily-scan ITEM."
  (let ((sub (plist-get item :subseries)))
    (vector (if (and sub (not (string-empty-p sub))) sub "main")
            (or (plist-get item :day) "")
            (or (plist-get item :date) "")
            (or (plist-get item :frame-id) "")
            (or (plist-get item :path) ""))))

(defun arxana-browser-scans-open (item)
  "Open the frame EDN file for ITEM in another window."
  (let ((path (plist-get item :path)))
    (unless (and path (file-readable-p path))
      (user-error "Scan frame is missing: %s" path))
    (find-file-other-window path)))

(provide 'arxana-browser-scans)
;;; arxana-browser-scans.el ends here
