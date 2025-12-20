(require 'outline)
(with-temp-buffer
  (insert "* Heading\nBody\n")
  (goto-char (point-min))
  (outline-mode)
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'after-string "\nPLACEHOLDER\n")
    (outline-hide-sublevels 1)
    (princ (buffer-string))))
