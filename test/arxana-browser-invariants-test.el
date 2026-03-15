;;; arxana-browser-invariants-test.el --- Tests for invariants browser -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(load-file "/home/joe/code/futon4/dev/arxana-browser-core.el")
(load-file "/home/joe/code/futon4/dev/arxana-browser-lab.el")

(ert-deftest arxana-browser-menu-includes-invariants ()
  (should
   (member "Invariants"
           (mapcar (lambda (item) (plist-get item :label))
                   (arxana-browser--menu-items)))))

(ert-deftest arxana-browser-candidate-family-forms-reads-watchlist ()
  (let* ((inventory
          '(structural-law-inventory
            (handoff-1-2-working-set
             (candidate-family-watchlist
              ((family :id cross-store-agreement
                       :best-current-exemplar session-continuity-alignment
                       :repo futon3c
                       :strength candidate-with-live-exemplar
                       :note "Pressure is real."))))))
         (forms (arxana-browser--candidate-family-forms inventory)))
    (should (= 1 (length forms)))
    (should (eq 'family (caar forms)))
    (should (eq 'cross-store-agreement (plist-get (cdar forms) :id)))))

(ert-deftest arxana-browser-candidate-item-builds-browser-shape ()
  (let ((item (arxana-browser--candidate-invariant-entry-item
               "repl-turns-emit-evidence"
               "human-visible-inspectability"
               "repo/futon3c"
               "candidate"
               "Claude and Codex REPL turns should emit evidence."
               'repo-seed)))
    (should (eq 'candidate-invariant-entry (plist-get item :type)))
    (should (equal "repl-turns-emit-evidence" (plist-get item :invariant)))
    (should (equal "human-visible-inspectability" (plist-get item :family)))
    (should (equal "repo/futon3c" (plist-get item :source)))
    (should (equal "candidate" (plist-get item :status)))))

(ert-deftest arxana-browser-candidate-items-from-devmap-form ()
  (let* ((items (arxana-browser--candidate-invariant-items-from-devmap-form
                 '(devmap :id futon3c
                          :candidate-invariants
                          ((invariant :id repl-turns-emit-evidence
                                      :family human-visible-inspectability
                                      :status candidate
                                      :summary "Turns emit evidence.")))))
         (item (car items)))
    (should (= 1 (length items)))
    (should (equal "repl-turns-emit-evidence" (plist-get item :invariant)))
    (should (equal "human-visible-inspectability" (plist-get item :family)))
    (should (equal "repo/futon3c" (plist-get item :source)))))

;;; arxana-browser-invariants-test.el ends here
