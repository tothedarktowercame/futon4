;;; arxana-browser.el --- Browser entry points -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry points that wire the browser modules together.

;;; Code:

(require 'arxana-browser-core)
(require 'arxana-browser-docbook)
(require 'arxana-browser-encyclopedia)
(require 'arxana-browser-forum)
(require 'arxana-browser-lab)
(require 'arxana-browser-marks)
(require 'arxana-media)
(require 'arxana-browser-browse)

;;;###autoload
(defun arxana-browser-browse ()
  "Open the Arxana browser buffer."
  (interactive)
  (with-current-buffer (get-buffer-create arxana-browser--buffer)
    (setq arxana-browser--stack nil
          arxana-browser--context nil))
  (arxana-browser--render)
  (when (fboundp 'arxana-ui-refresh)
    (arxana-ui-refresh)))

;;;###autoload
(defalias 'arxana-patterns-browse #'arxana-browser-browse)

(defalias 'arxana-browse #'arxana-browser-browse)

(provide 'arxana-browser)
;;; arxana-browser.el ends here
