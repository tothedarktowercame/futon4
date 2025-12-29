;;; arxana-docbook-order-test.el --- Tests for docbook ordering -*- lexical-binding: t; -*-

(require 'ert)
(setq load-prefer-newer t)
(defvar flexiarg-mode-map (make-sparse-keymap))
(require 'arxana-patterns)
(require 'arxana-browser-docbook)

(defun arxana-docbook-order-test--items ()
  (list
   (list :type 'docbook-heading
         :doc-id "A"
         :title "Overview"
         :outline '("Overview")
         :path_string "Overview"
         :level 1)
   (list :type 'docbook-heading
         :doc-id "B"
         :title "Intro"
         :outline '("Overview" "Intro")
         :path_string "Overview / Intro"
         :level 2)
   (list :type 'docbook-heading
         :doc-id "C"
         :title "Quickstart"
         :outline '("Quickstart")
         :path_string "Quickstart"
         :level 1)
   (list :type 'docbook-heading
         :doc-id "D"
         :title "Step 1"
         :outline '("Quickstart" "Step 1")
         :path_string "Quickstart / Step 1"
         :level 2)))

(defun arxana-docbook-order-test--entries (items)
  (mapcar (lambda (item)
            (list item (arxana-browser--docbook-contents-row item)))
          items))

(defun arxana-docbook-order-test--goto-doc-id (doc-id)
  (goto-char (point-min))
  (while (and (not (eobp)) (null (tabulated-list-get-id)))
    (forward-line 1))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((entry (tabulated-list-get-id)))
        (when (and entry (equal doc-id (plist-get entry :doc-id)))
          (setq found t))
        (unless found
          (forward-line 1))))
    (unless found
      (error "Could not find doc-id %s" doc-id))))

(defmacro arxana-docbook-order-test--with-contents (items &rest body)
  (declare (indent 1))
  `(let ((items ,items))
     (with-temp-buffer
       (arxana-browser-mode)
       (setq arxana-browser--stack (list (list :view 'docbook-contents :book "futon4")))
       (setq tabulated-list-format (arxana-browser--docbook-contents-format))
       (setq tabulated-list-entries (arxana-docbook-order-test--entries items))
       (tabulated-list-init-header)
       (tabulated-list-print t)
       (cl-letf (((symbol-function 'arxana-browser--render)
                  (lambda ()
                    (let* ((book "futon4")
                           (order (arxana-browser--docbook-contents-order-get book))
                           (ordered-items (if order
                                              (arxana-browser--docbook-contents-order-items items order)
                                            items)))
                      (setq tabulated-list-entries (arxana-docbook-order-test--entries ordered-items))
                      (tabulated-list-print t))))
                 ((symbol-function 'arxana-browser--goto-doc-id)
                  (lambda (doc-id)
                    (arxana-docbook-order-test--goto-doc-id doc-id)
                    t)))
         ,@body))))

(ert-deftest arxana-docbook-order-move-item-up ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (arxana-docbook-order-test--goto-doc-id "D")
   (arxana-browser-docbook-move-item-up)
   (should (equal (arxana-browser--docbook-contents-order-get "futon4")
                  '("A" "B" "D" "C")))))

(ert-deftest arxana-docbook-order-move-item-down ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (arxana-docbook-order-test--goto-doc-id "B")
   (arxana-browser-docbook-move-item-down)
   (should (equal (arxana-browser--docbook-contents-order-get "futon4")
                  '("A" "C" "B" "D")))))

(ert-deftest arxana-docbook-order-move-section-down ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (arxana-docbook-order-test--goto-doc-id "A")
   (arxana-browser-docbook-move-section-down)
   (should (equal (arxana-browser--docbook-contents-order-get "futon4")
                  '("C" "D" "A" "B")))))

(ert-deftest arxana-docbook-order-move-section-up ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (arxana-docbook-order-test--goto-doc-id "C")
   (arxana-browser-docbook-move-section-up)
   (should (equal (arxana-browser--docbook-contents-order-get "futon4")
                  '("C" "D" "A" "B")))))

(ert-deftest arxana-docbook-order-move-item-up-boundary ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (arxana-docbook-order-test--goto-doc-id "A")
   (should-error (arxana-browser-docbook-move-item-up))))

(ert-deftest arxana-docbook-order-move-item-down-boundary ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (arxana-docbook-order-test--goto-doc-id "D")
   (should-error (arxana-browser-docbook-move-item-down))))

(ert-deftest arxana-docbook-order-move-section-up-boundary ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (arxana-docbook-order-test--goto-doc-id "A")
   (should-error (arxana-browser-docbook-move-section-up))))

(ert-deftest arxana-docbook-order-move-section-down-boundary ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (arxana-docbook-order-test--goto-doc-id "C")
   (should-error (arxana-browser-docbook-move-section-down))))

(ert-deftest arxana-docbook-order-initial-contents-respects-top-order ()
  (with-temp-buffer
    (arxana-browser-mode)
    (setq-local arxana-browser--docbook-contents-order nil)
    (setq-local arxana-browser-docbook-prefer-toc-order nil)
    (cl-letf (((symbol-function 'arxana-docbook--remote-available-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'arxana-docbook--filesystem-available-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'arxana-docbook--toc)
               (lambda (_book)
                 (list (list :doc-id "quickstart"
                             :title "Quickstart"
                             :outline '("Quickstart")
                             :path_string "Quickstart"
                             :level 1)
                       (list :doc-id "overview"
                             :title "Overview"
                             :outline '("Overview")
                             :path_string "Overview"
                             :level 1)))))
      (let* ((items (arxana-browser--docbook-contents-items "futon4"))
             (doc-ids (delq nil (mapcar (lambda (item) (plist-get item :doc-id))
                                        items))))
        (should (equal (list (nth 0 doc-ids) (nth 1 doc-ids))
                       '("overview" "quickstart")))))))

(ert-deftest arxana-docbook-order-dirty-after-reorder ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (setq-local arxana-browser--docbook-contents-view-items items)
   (setq-local arxana-browser--docbook-contents-synced-order '("A" "B" "C" "D"))
   (arxana-docbook-order-test--goto-doc-id "B")
   (arxana-browser-docbook-move-item-down)
   (should (arxana-browser--docbook-contents-dirty-p "futon4"))))

(ert-deftest arxana-docbook-order-dirty-when-toc-order-missing ()
  (with-temp-buffer
    (arxana-browser-mode)
    (setq arxana-browser--stack (list (list :view 'docbook-contents :book "futon4")))
    (setq-local arxana-browser--docbook-contents-order nil)
    (setq-local arxana-browser--docbook-contents-synced-order nil)
    (cl-letf (((symbol-function 'arxana-docbook--remote-available-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'arxana-docbook--filesystem-available-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'arxana-docbook--toc)
               (lambda (_book)
                 (arxana-docbook-order-test--items)))
              ((symbol-function 'arxana-docbook--toc-doc-id)
               (lambda (_heading) nil)))
      (let* ((items (arxana-browser--docbook-contents-items "futon4")))
        (setq tabulated-list-format (arxana-browser--docbook-contents-format))
        (setq tabulated-list-entries (arxana-docbook-order-test--entries items))
        (tabulated-list-init-header)
        (tabulated-list-print t)
        (should (equal arxana-browser--docbook-contents-synced-order
                       '("A" "B" "C" "D")))
        (arxana-docbook-order-test--goto-doc-id "B")
        (arxana-browser-docbook-move-item-down)
        (should (arxana-browser--docbook-contents-dirty-p "futon4"))))))

(ert-deftest arxana-docbook-order-sync-clears-dirty ()
  (arxana-docbook-order-test--with-contents
   (arxana-docbook-order-test--items)
   (setq-local arxana-browser--docbook-contents-view-items items)
   (setq-local arxana-browser--docbook-contents-synced-order '("A" "B" "C" "D"))
   (arxana-docbook-order-test--goto-doc-id "B")
   (arxana-browser-docbook-move-item-down)
   (should (arxana-browser--docbook-contents-dirty-p "futon4"))
   (cl-letf (((symbol-function 'arxana-docbook--remote-available-p)
              (lambda (&rest _args) t))
             ((symbol-function 'arxana-docbook--remote-update-toc-order)
              (lambda (&rest _args) (list :status :ok)))
             ((symbol-function 'arxana-docbook--filesystem-available-p)
              (lambda (&rest _args) nil)))
     (arxana-browser--docbook-sync-order
      "futon4"
      (arxana-browser--docbook-contents-order-get "futon4")
      nil))
   (should (not (arxana-browser--docbook-contents-dirty-p "futon4")))))

(provide 'arxana-docbook-order-test)
;;; arxana-docbook-order-test.el ends here
