;;; arxana-scholium-test.el --- Tests for scholium authoring helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'arxana-scholium)

(defvar new-scholium-name nil)
(defvar new-scholium-mode nil)

(ert-deftest arxana-scholium-compose-invokes-make-scholium ()
  (let ((new-scholium-name "Demo")
        (calls 0))
    (cl-letf (((symbol-function 'make-scholium)
               (lambda () (setq calls (1+ calls)))))
      (arxana-scholium-compose)
      (should (= calls 1)))))

(ert-deftest arxana-scholium-compose-region-delegates ()
  (let (captured)
    (cl-letf (((symbol-function 'make-scholium-about-part-of-current-article)
               (lambda (beg end) (setq captured (list beg end)))))
      (arxana-scholium-compose-from-region 10 20)
      (should (equal captured '(10 20))))))

(ert-deftest arxana-scholium-ensure-support-errors ()
  (should-error (arxana-scholium--ensure-support 'nonexistent-fn)
                :type 'user-error))

(ert-deftest arxana-scholium-authoring-mode-proxies ()
  (let ((calls '()))
    (cl-letf (((symbol-function 'new-scholium-mode)
               (lambda (arg) (push arg calls))))
      (arxana-scholium-authoring-mode 1)
      (arxana-scholium-authoring-mode 0)
      (should (equal calls '(-1 1))))))

(ert-deftest arxana-scholium-composes-about-target ()
  (let ((new-scholium-name "demo")
        (new-scholium-about nil)
        (calls nil))
    (cl-letf (((symbol-function 'make-scholium)
               (lambda () (push new-scholium-about calls))))
      (arxana-scholium--compose-about "Target")
      (should (equal calls '((("Target"))))))))

(ert-deftest arxana-scholium-redisplays-after-save ()
  (let ((new-scholium-about '(("Focus")))
        (shown nil))
    (cl-letf (((symbol-function 'display-article)
               (lambda (target) (setq shown target)))
              ((symbol-function 'escape-scholium-creation)
               (lambda (&rest _)
                 (setq new-scholium-about nil))))
      (arxana-scholium--escape-and-display #'escape-scholium-creation)
      (should (equal shown "Focus")))))

(provide 'arxana-scholium-test)
;;; arxana-scholium-test.el ends here
