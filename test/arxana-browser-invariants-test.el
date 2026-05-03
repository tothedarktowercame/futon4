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

(ert-deftest arxana-browser-invariants-menu-leads-with-live-invariants ()
  (let* ((root-item (seq-find (lambda (item)
                                (equal (plist-get item :label) "Invariants"))
                              (arxana-browser--menu-items)))
         (sub-items (arxana-browser--invariants-menu-items))
         (first-item (car sub-items)))
    (should (equal (plist-get root-item :description)
                   "Live invariant families first, with violations and candidates as secondary views."))
    (should (equal (plist-get first-item :label) "Live Invariants"))
    (should (equal (plist-get first-item :view) 'operational-families))))

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

(ert-deftest arxana-browser-candidate-section-items-insert-lanes-and-layers ()
  (let* ((items (list
                 '(:type candidate-invariant-entry
                   :invariant "task-work-must-traverse-gates"
                   :family "atomic-inspectable-units"
                   :lane "promote-next"
                   :lane-rank 0
                   :layer ":I3"
                   :source "repo/futon3b"
                   :note "Bypassable live pressure.")
                 '(:type candidate-invariant-entry
                   :invariant "checkout-before-work"
                   :family "atomic-inspectable-units"
                   :lane "family-watchlist"
                   :lane-rank 1
                   :layer ":I3"
                   :source "family/atomic-inspectable-units"
                   :note "Canonical family pressure.")))
         (sectioned (arxana-browser--candidate-section-items items))
         (labels (mapcar (lambda (item) (plist-get item :label))
                         (seq-filter (lambda (item)
                                       (eq (plist-get item :type) 'info))
                                     sectioned))))
    (should (member "PROMOTE NEXT" labels))
    (should (member "FAMILY WATCHLIST" labels))
    (should (member "  :I3: Work is structured" labels))))

(ert-deftest arxana-browser-candidate-row-renders-info-sections ()
  (let* ((row (arxana-browser--candidate-invariants-row
               '(:type info
                 :label "PROMOTE NEXT"
                 :description "Section header."))))
    (should (equal "PROMOTE NEXT" (aref row 0)))
    (should (equal "Section header." (aref row 7)))))

(ert-deftest arxana-browser-candidate-priority-map-reads-json-queue ()
  (let* ((path (make-temp-file "candidate-queue" nil ".json"))
         (json-payload
          "{\n  \"runs\": [\n    {\n      \"run/source\": \"family-definition\",\n      \"family/id\": \"human-visible-inspectability\",\n      \"invariant/id\": \"repl-turns-emit-evidence\",\n      \"priority/score\": 550\n    },\n    {\n      \"run/source\": \"repo-seed\",\n      \"family/id\": \"atomic-inspectable-units\",\n      \"invariant/id\": \"checkout-before-work\",\n      \"priority/score\": 420\n    }\n  ]\n}\n"))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert json-payload))
          (let* ((arxana-browser-stack-priority-queue-path path)
                 (priority-map (arxana-browser--candidate-priority-map))
                 (first (gethash "human-visible-inspectability|repl-turns-emit-evidence|family-definition"
                                 priority-map))
                 (second (gethash "atomic-inspectable-units|checkout-before-work|repo-seed"
                                  priority-map)))
            (should (= 1 (plist-get first :priority-rank)))
            (should (= 550 (plist-get first :priority-score)))
            (should (= 2 (plist-get second :priority-rank)))
            (should (= 420 (plist-get second :priority-score)))))
      (delete-file path))))

(ert-deftest arxana-browser-operational-family-location-is-copyable ()
  (let ((item '(:type operational-family-entry
                :id ":F-startup-contracts"
                :name "Startup contracts")))
    (should (equal (arxana-browser-operational-family-location item)
                   "arxana://invariants/family/startup-contracts"))))

(ert-deftest arxana-browser-format-repos-display-is-compact ()
  (should (equal (arxana-browser--format-repos-display ":R-futon1a :R-futon3b")
                 "1a, 3b")))

(ert-deftest arxana-browser-operational-family-definition-form-finds-implemented-in ()
  (let* ((inventory
          '(structural-law-inventory
            (operational-families
             ((family :id startup-contracts
                      :status operational
                      :implemented-in ("futon1a/system.clj"))))))
         (family-form
          (arxana-browser--operational-family-definition-form
           inventory
           ":F-startup-contracts")))
    (should family-form)
    (should (equal (plist-get (cdr family-form) :implemented-in)
                   '("futon1a/system.clj")))))

(ert-deftest arxana-browser-implemented-roots-display-detects-futon3c-shorthand ()
  (should (equal (arxana-browser--implemented-roots-display
                  '("agents/tickle_logic.clj" "peripheral/proof_logic.clj"))
                 "futon3c")))

(ert-deftest arxana-browser-implemented-root-detects-repo-prefixed-src-path ()
  (should (equal (arxana-browser--implemented-root
                  "futon3b/src/futon3/gate/pipeline.clj")
                 "futon3b")))

(ert-deftest arxana-browser-invariant-family-doc-explains-existence ()
  (let* ((item '(:type operational-family-entry
                 :id ":F-existence"
                 :name "Existence"))
         (doc (arxana-browser--invariant-family-doc item)))
    (should doc)
    (should (string-match-p "structural references point at real entities"
                            (plist-get doc :what)))
    (should (equal (arxana-browser--invariant-code-doc item "peripheral/mission_backend.clj")
                   "Reject mission transitions or blocker claims that point at missing obligations."))))

(ert-deftest arxana-browser-invariant-code-symbol-resolves-curated-target ()
  (let ((item '(:type operational-family-entry
                :id ":F-phase-ordering"
                :name "Phase ordering")))
    (should (equal (arxana-browser--invariant-code-symbol item "peripheral/proof_logic.clj")
                   "query-mode-violations"))
    (should (equal (arxana-browser--invariant-code-symbol item "agents/tickle_logic.clj")
                   "query-watchdog-escalations-without-pages"))))

(ert-deftest arxana-browser-invariant-code-symbol-covers-startup-and-errors ()
  (should (equal (arxana-browser--invariant-code-symbol
                  '(:type operational-family-entry
                    :id ":F-startup-contracts"
                    :name "Startup contracts")
                  "futon1a/system.clj")
                 "validate-start-policy!"))
  (should (equal (arxana-browser--invariant-code-symbol
                  '(:type operational-family-entry
                    :id ":F-layered-error-hierarchy"
                    :name "Layered error hierarchy")
                  "futon1a/api/errors.clj")
                 "error->response")))

(ert-deftest arxana-browser-invariant-code-symbol-covers-graph-symmetry-targets ()
  (should (equal (arxana-browser--invariant-code-symbol
                  '(:type operational-family-entry
                    :id ":F-graph-symmetry"
                    :name "Graph symmetry")
                  "portfolio/logic.clj")
                 "build-db"))
  (should (equal (arxana-browser--invariant-code-symbol
                  '(:type operational-family-entry
                    :id ":F-graph-symmetry"
                    :name "Graph symmetry")
                  "agents/tickle_logic.clj")
                 "query-orphan-escalations")))

(ert-deftest arxana-browser-insert-invariant-code-pointer-block-records-target ()
  (let ((item '(:type operational-family-entry
                :id ":F-gate-pipeline-phase-ordering"
                :name "Gate pipeline phase ordering")))
    (with-temp-buffer
      (let ((target (arxana-browser--insert-invariant-code-pointer-block
                     item
                     "futon3b/src/futon3/gate/pipeline.clj")))
        (should (equal (plist-get target :symbol) "run"))
        (should (string-match-p "futon3b/src/futon3/gate/pipeline.clj"
                                (buffer-string)))
        (should (equal (get-text-property (point-min) 'arxana-invariant-target)
                       target))))))

(ert-deftest arxana-browser-current-code-def-symbol-finds-defn ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun sample-thing ()\n  (message \"x\"))\n")
    (goto-char (point-max))
    (should (equal (arxana-browser--current-code-def-symbol)
                   "sample-thing"))))

(ert-deftest arxana-browser-live-invariant-family-p-excludes-candidates ()
  (should (arxana-browser--live-invariant-family-p '(:status ":operational")))
  (should (arxana-browser--live-invariant-family-p '(:status ":operational-but-bypassable")))
  (should-not (arxana-browser--live-invariant-family-p '(:status ":candidate"))))

(ert-deftest arxana-browser-operational-families-items-only-show-live-families ()
  (cl-letf (((symbol-function 'arxana-browser--read-invariant-model)
             (lambda ()
               "{:id :F-live :name \"Live\" :layer :I0 :status :operational :question \"Q\" :repos #{:R-futon1a}}\n\
{:id :F-candidate :name \"Candidate\" :layer :I4 :status :candidate :question \"Q\" :repos #{:R-futon4}}")))
    (let* ((items (arxana-browser--operational-families-items))
           (labels (mapcar (lambda (item) (plist-get item :label)) items)))
      (should (member "Live" labels))
      (should-not (member "Candidate" labels)))))

(ert-deftest arxana-browser-mi0-docs-code-report-is-currently-green ()
  (let ((report (arxana-browser--mi0-docs-code-report)))
    (should (> (plist-get report :total) 0))
    (should (= (plist-get report :total)
               (plist-get report :complete)))
    (should-not (plist-get report :incomplete))))

(ert-deftest arxana-browser-mi0-live-violation-actionability-report-detects-gaps ()
  (cl-letf (((symbol-function 'arxana-browser--violations-items)
             (lambda ()
               (list
                '((:type . violation-entry)
                  (:hx/endpoints "inv:existence/dangling-refs" "proof-item:a")
                  (:hx/props (:family . "existence")
                             (:rule . "dangling-refs")
                             (:actionability . "auto-fixable")
                             (:obligation-id . "obl-1")
                             (:detected-at . "2026-04-26T12:00:00Z")
                             (:emitted-by . "proof-bridge")
                             (:last-checked . "2026-04-26T12:05:00Z")))
                '((:type . violation-entry)
                  (:hx/endpoints "inv:phase-ordering/mode-violations")
                  (:hx/props (:family . "phase-ordering")
                             (:rule . "mode-violations")
                             (:detected-at . "2026-04-26T12:00:00Z")))))))
    (let ((report (arxana-browser--mi0-live-violation-actionability-report)))
      (should (= 2 (plist-get report :total)))
      (should (= 1 (plist-get report :complete)))
      (should (= 1 (length (plist-get report :incomplete)))))))

(ert-deftest arxana-browser-invariant-guide-includes-mi0-section ()
  (let ((labels (mapcar (lambda (item) (plist-get item :label))
                        (arxana-browser--invariant-guide-items))))
    (should (member "MI0: CORE METAINVARIANTS" labels))
    (should (member "  Docs->Code correspondence" labels))
    (should (member "  Live-violation actionability" labels))))

;;; arxana-browser-invariants-test.el ends here
