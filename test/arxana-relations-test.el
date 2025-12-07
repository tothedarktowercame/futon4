;;; arxana-relations-test.el --- Tests for relation browsing -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'arxana-relations)

(defun arxana-relations-test--buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun arxana-relations-test--with-buffer (title renderer body)
  (let ((arxana-relations-buffer-name "*Arxana Relations Test*"))
    (arxana-relations--with-buffer title renderer body nil)
    (with-current-buffer arxana-relations-buffer-name
      (arxana-relations-test--buffer-string))))

(ert-deftest arxana-relations-render-tail-section ()
  (let* ((sample '((:relations . (((:src (:name "A" :type :article))
                                   (:dst (:name "B"))
                                   (:type :hx/links)
                                   (:confidence . 0.9)
                                   (:last-seen . 42))))))
         (text (arxana-relations-test--with-buffer
                "Tail"
                #'arxana-relations--render-tail
                sample)))
    (should (string-match-p "Recent relations" text))
    (should (string-match-p "A" text))
    (should (string-match-p "B" text))))

(ert-deftest arxana-relations-render-ego-section ()
  (let* ((sample '((:ego . ((:entity (:name "Demo" :type :article))
                            (:links . (((:direction :out)
                                        (:relation :hx/links)
                                        (:entity (:entity/name "Other")))))))))
         (text (arxana-relations-test--with-buffer
                "Ego"
                #'arxana-relations--render-ego
                sample)))
    (should (string-match-p "Outgoing" text))
    (should (string-match-p "Demo" text))
    (should (string-match-p "Other" text))))

(ert-deftest arxana-relations-render-cooccur-section ()
  (let* ((sample '((:cooccur . ((:entity (:name "Demo"))
                                (:rows . (((:name "Peer") (:count . 4))))))))
         (text (arxana-relations-test--with-buffer
                "Cooccur"
                #'arxana-relations--render-cooccur
                sample)))
    (should (string-match-p "Co-occurrences" text))
    (should (string-match-p "Peer" text))))

(ert-deftest arxana-relations-context-captures-futon-id ()
  (let ((arxana-relations-buffer-name "*Arxana Relations Context*"))
    (cl-letf* (((symbol-function 'arxana-store-ego)
                (lambda (&rest _) '((:ego . nil))))
               ((symbol-function 'futon4-lookup-article-id)
                (lambda (_) "id-demo")))
      (arxana-relations-show-ego "Demo" 5)
      (with-current-buffer arxana-relations-buffer-name
        (should (plist-get arxana-relations-context :endpoint))
        (should (string= (plist-get arxana-relations-context :id) "id-demo"))
        (should (string-match-p "Futon: id-demo"
                                (format "%s" header-line-format)))))))

(ert-deftest arxana-relations-refresh-reuses-fetch-context ()
  (let ((arxana-relations-buffer-name "*Arxana Relations Context*")
        (calls 0))
    (cl-letf* (((symbol-function 'arxana-store-ego)
                (lambda (&rest _)
                  (setq calls (1+ calls))
                  '((:ego . nil))))
               ((symbol-function 'futon4-lookup-article-id)
                (lambda (_) "id-demo")))
      (arxana-relations-show-ego "Demo" 3)
      (should (= calls 1))
      (with-current-buffer arxana-relations-buffer-name
        (arxana-relations-refresh)
        (should (= calls 2))))))

(ert-deftest arxana-relations-copy-id-to-kill-ring ()
  (let ((arxana-relations-buffer-name "*Arxana Relations Context*"))
    (cl-letf* (((symbol-function 'arxana-store-ego)
                (lambda (&rest _) '((:ego . nil))))
               ((symbol-function 'futon4-lookup-article-id)
                (lambda (_) "id-demo")))
      (arxana-relations-show-ego "Demo" 2)
      (with-current-buffer arxana-relations-buffer-name
        (arxana-relations-copy-id)
        (should (string= (current-kill 0) "id-demo"))))))

(provide 'arxana-relations-test)
;;; arxana-relations-test.el ends here
