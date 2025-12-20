(add-to-list 'load-path (expand-file-name "dev" "."))
(load "/home/joe/code/futon3/contrib/flexiarg.el")
(provide 'flexiarg)
(require 'arxana-patterns)
(with-temp-buffer
  (arxana-flexiarg-collection-mode)
  (arxana-flexiarg--prepare-buffer '("/home/joe/code/futon4/docs/testing.flexiarg"))
  (message "metadata-overlays:%d" (length arxana-flexiarg--metadata-overlays)))
