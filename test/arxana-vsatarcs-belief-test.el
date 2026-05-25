;;; arxana-vsatarcs-belief-test.el --- Tests for VSATARCS per-entity belief -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the VSATARCS reader's per-entity belief state apparatus
;; (`arxana-vsatarcs-belief.el').
;;
;; Validation properties checked here mirror `futon2/test/futon2/aif/
;; belief_test.clj' (claude-2, 2026-05-17):
;;   - Posterior is a valid probability distribution (sum = 1.0, all
;;     values in [0, 1])
;;   - Update is deterministic under known input (R1 baseline test
;;     required by M-stack-essay-code-alignment / standard contract R1)
;;   - Multiplicative-likelihood updates commute under reordering
;;   - Most-likely-status reports argmax correctly
;;   - Entropy is maximal for uniform posterior, decreasing under
;;     accumulating evidence (V-shrink shape — full V-shrink is R9-class)

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-belief)

(defun arxana-vsatarcs-belief-test--valid-posterior-p (p)
  (arxana-vsatarcs-belief-valid-posterior-p p))

(defun arxana-vsatarcs-belief-test--get (posterior status)
  "Return the probability assigned to STATUS in POSTERIOR."
  (cdr (assoc status posterior)))

(ert-deftest arxana-vsatarcs-belief-uniform-prior-is-valid ()
  (let ((p (arxana-vsatarcs-belief-uniform-prior)))
    (should (arxana-vsatarcs-belief-test--valid-posterior-p p))))

(ert-deftest arxana-vsatarcs-belief-uniform-prior-is-equal-probabilities ()
  (let* ((p (arxana-vsatarcs-belief-uniform-prior))
         (n (length arxana-vsatarcs-belief-status-set))
         (expected (/ 1.0 n)))
    (should (cl-every (lambda (kv) (< (abs (- expected (cdr kv))) 1e-9))
                      p))))

(ert-deftest arxana-vsatarcs-belief-initial-state-carries-uniform-priors ()
  (let* ((b (arxana-vsatarcs-belief-initial-state '(:e1 :e2 :e3))))
    (should (equal (sort (mapcar #'car b) (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                   '(:e1 :e2 :e3)))
    (should (cl-every (lambda (kv)
                        (arxana-vsatarcs-belief-test--valid-posterior-p (cdr kv)))
                      b))))

(ert-deftest arxana-vsatarcs-belief-in-set-event-raises-matched-status ()
  (let* ((p0 (arxana-vsatarcs-belief-uniform-prior))
         (p1 (arxana-vsatarcs-belief-update-entity
              p0 '(:type :strengthened :weight 1.0))))
    (should (arxana-vsatarcs-belief-test--valid-posterior-p p1))
    (should (> (arxana-vsatarcs-belief-test--get p1 'strengthened)
               (arxana-vsatarcs-belief-test--get p0 'strengthened)))
    (should (< (arxana-vsatarcs-belief-test--get p1 'spawned)
               (arxana-vsatarcs-belief-test--get p0 'spawned)))))

(ert-deftest arxana-vsatarcs-belief-namespaced-event-type-is-recognised ()
  (let* ((p0 (arxana-vsatarcs-belief-uniform-prior))
         (p-bare (arxana-vsatarcs-belief-update-entity
                  p0 '(:type :strengthened :weight 1.0)))
         (p-ns (arxana-vsatarcs-belief-update-entity
                p0 '(:type :state/strengthened :weight 1.0))))
    (should (equal p-bare p-ns))))

(ert-deftest arxana-vsatarcs-belief-unknown-event-type-leaves-posterior-unchanged ()
  (let* ((p0 (arxana-vsatarcs-belief-uniform-prior))
         (p1 (arxana-vsatarcs-belief-update-entity
              p0 '(:type :not-a-status :weight 1.0))))
    (should (equal p0 p1))))

(ert-deftest arxana-vsatarcs-belief-link-event-leaves-posterior-unchanged ()
  ;; `link/asserted' events (relational) must be silently ignored by the
  ;; per-entity belief update — verified to keep the update step total
  ;; over a wider event stream.
  (let* ((p0 (arxana-vsatarcs-belief-uniform-prior))
         (p1 (arxana-vsatarcs-belief-update-entity
              p0 '(:type :link/asserted :weight 1.0))))
    (should (equal p0 p1))))

(ert-deftest arxana-vsatarcs-belief-missing-weight-defaults-to-1 ()
  (let ((p-default (arxana-vsatarcs-belief-update-entity
                   (arxana-vsatarcs-belief-uniform-prior) '(:type :spawned)))
        (p-explicit (arxana-vsatarcs-belief-update-entity
                    (arxana-vsatarcs-belief-uniform-prior)
                    '(:type :spawned :weight 1.0))))
    (should (equal p-default p-explicit))))

(ert-deftest arxana-vsatarcs-belief-repeated-updates-remain-valid ()
  (let ((final (cl-reduce #'arxana-vsatarcs-belief-update-entity
                          '((:type :spawned :weight 0.5)
                            (:type :refined :weight 1.0)
                            (:type :addressed :weight 2.0))
                          :initial-value (arxana-vsatarcs-belief-uniform-prior))))
    (should (arxana-vsatarcs-belief-test--valid-posterior-p final))))

(ert-deftest arxana-vsatarcs-belief-update-is-deterministic ()
  (let* ((b0 (arxana-vsatarcs-belief-initial-state '(:m1 :m2)))
         (events '((:entity-id :m1 :type :strengthened :weight 1.5)
                   (:entity-id :m2 :type :foreclosed :weight 0.8)
                   (:entity-id :m1 :type :addressed :weight 0.5)))
         (b1 (arxana-vsatarcs-belief-update-batch b0 events))
         (b2 (arxana-vsatarcs-belief-update-batch b0 events)))
    ;; Determinism asserted per-entity (alist order is incidental in
    ;; elisp; equality must hold on each posterior).
    (dolist (eid '(:m1 :m2))
      (should (equal (cdr (assoc eid b1)) (cdr (assoc eid b2)))))))

(ert-deftest arxana-vsatarcs-belief-update-is-commutative-per-entity ()
  (let* ((b0 (arxana-vsatarcs-belief-initial-state '(:m1)))
         (e1 '(:entity-id :m1 :type :strengthened :weight 1.0))
         (e2 '(:entity-id :m1 :type :refined :weight 0.5))
         (e3 '(:entity-id :m1 :type :addressed :weight 2.0))
         (b-forward (arxana-vsatarcs-belief-update-batch b0 (list e1 e2 e3)))
         (b-reverse (arxana-vsatarcs-belief-update-batch b0 (list e3 e2 e1)))
         (pf (cdr (assoc :m1 b-forward)))
         (pr (cdr (assoc :m1 b-reverse))))
    (dolist (s arxana-vsatarcs-belief-status-set)
      (should (< (abs (- (arxana-vsatarcs-belief-test--get pf s)
                         (arxana-vsatarcs-belief-test--get pr s)))
                 1e-9)))))

(ert-deftest arxana-vsatarcs-belief-untracked-entity-initialised-with-prior ()
  (let* ((b0 '())
         (b1 (arxana-vsatarcs-belief-update
              b0 '(:entity-id :new-thing :type :spawned :weight 1.0))))
    (should (assoc :new-thing b1))
    (should (arxana-vsatarcs-belief-test--valid-posterior-p
             (cdr (assoc :new-thing b1))))
    (should (> (arxana-vsatarcs-belief-test--get (cdr (assoc :new-thing b1))
                                                 'spawned)
               (/ 1.0 (length arxana-vsatarcs-belief-status-set))))))

(ert-deftest arxana-vsatarcs-belief-most-likely-status-nil-for-empty ()
  (should (null (arxana-vsatarcs-belief-most-likely-status nil))))

(ert-deftest arxana-vsatarcs-belief-most-likely-status-returns-argmax ()
  (let ((p (arxana-vsatarcs-belief-update-entity
            (arxana-vsatarcs-belief-uniform-prior)
            '(:type :strengthened :weight 5.0))))
    (should (eq 'strengthened
                (arxana-vsatarcs-belief-most-likely-status p)))))

(ert-deftest arxana-vsatarcs-belief-most-likely-status-uniform-in-set ()
  (should (memq (arxana-vsatarcs-belief-most-likely-status
                 (arxana-vsatarcs-belief-uniform-prior))
                arxana-vsatarcs-belief-status-set)))

(ert-deftest arxana-vsatarcs-belief-entropy-uniform-is-log-n ()
  (let* ((h (arxana-vsatarcs-belief-entropy
             (arxana-vsatarcs-belief-uniform-prior)))
         (n (length arxana-vsatarcs-belief-status-set))
         (max-h (log n)))
    (should (< (abs (- h max-h)) 1e-9))))

(ert-deftest arxana-vsatarcs-belief-entropy-peaked-below-uniform ()
  ;; V-shrink shape — entropy decreases as evidence accumulates.
  (let* ((uniform (arxana-vsatarcs-belief-uniform-prior))
         (peaked (cl-reduce #'arxana-vsatarcs-belief-update-entity
                            (make-list 10 '(:type :strengthened :weight 2.0))
                            :initial-value uniform)))
    (should (< (arxana-vsatarcs-belief-entropy peaked)
               (arxana-vsatarcs-belief-entropy uniform)))))

(ert-deftest arxana-vsatarcs-belief-entropy-non-negative ()
  (should (>= (arxana-vsatarcs-belief-entropy
               (arxana-vsatarcs-belief-uniform-prior))
              0.0)))

;; ---------------------------------------------------------------------
;; v0.2 — persistence + snapshot
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-belief-save-load-round-trip ()
  (let* ((tmp (make-temp-file "vsatarcs-belief-" nil ".eld"))
         (initial (arxana-vsatarcs-belief-initial-state '(:m1 :m2))))
    (unwind-protect
        (progn
          (arxana-vsatarcs-belief-reset)
          (setq arxana-vsatarcs-belief--current initial)
          (arxana-vsatarcs-belief-save tmp)
          (arxana-vsatarcs-belief-reset)
          (should (null (arxana-vsatarcs-belief-current)))
          (arxana-vsatarcs-belief-load tmp)
          (dolist (eid '(:m1 :m2))
            (should (equal (cdr (assoc eid initial))
                           (cdr (assoc eid (arxana-vsatarcs-belief-current)))))))
      (when (file-exists-p tmp) (delete-file tmp))
      (arxana-vsatarcs-belief-reset))))

(ert-deftest arxana-vsatarcs-belief-load-missing-file-resets-to-nil ()
  (arxana-vsatarcs-belief-reset)
  (setq arxana-vsatarcs-belief--current
        (arxana-vsatarcs-belief-initial-state '(:m1)))
  (arxana-vsatarcs-belief-load
   (expand-file-name "definitely-not-present-vsatarcs-belief.eld"
                     temporary-file-directory))
  (should (null (arxana-vsatarcs-belief-current)))
  (arxana-vsatarcs-belief-reset))

(ert-deftest arxana-vsatarcs-belief-ingest-events-applies-batch ()
  (arxana-vsatarcs-belief-reset)
  (let ((post (arxana-vsatarcs-belief-ingest-events
               '((:entity-id :m1 :type :strengthened :weight 2.0)
                 (:entity-id :m2 :type :spawned :weight 1.0)))))
    (should (= 2 (length post)))
    (should (assoc :m1 post))
    (should (assoc :m2 post))
    (should (eq 'strengthened
                (arxana-vsatarcs-belief-most-likely-status
                 (cdr (assoc :m1 post))))))
  (arxana-vsatarcs-belief-reset))

(ert-deftest arxana-vsatarcs-belief-ingest-events-save-flag-persists ()
  (let ((tmp (make-temp-file "vsatarcs-belief-" nil ".eld"))
        (arxana-vsatarcs-belief-store-file nil))
    (unwind-protect
        (progn
          (setq arxana-vsatarcs-belief-store-file tmp)
          (arxana-vsatarcs-belief-reset)
          (arxana-vsatarcs-belief-ingest-events
           '((:entity-id :m1 :type :spawned :weight 1.0))
           t)
          (arxana-vsatarcs-belief-reset)
          (arxana-vsatarcs-belief-load)
          (should (assoc :m1 (arxana-vsatarcs-belief-current))))
      (when (file-exists-p tmp) (delete-file tmp))
      (arxana-vsatarcs-belief-reset))))

(ert-deftest arxana-vsatarcs-belief-snapshot-empty-when-no-state ()
  (arxana-vsatarcs-belief-reset)
  (should (null (arxana-vsatarcs-belief-snapshot)))
  (arxana-vsatarcs-belief-reset))

;; ---------------------------------------------------------------------
;; v0.2.2 — prior bootstrap from stack-annotations.edn (canonical source)
;; ---------------------------------------------------------------------

(defconst arxana-vsatarcs-belief-test--stack-fixture
  ;; Includes both #inst and #{} so the EDN reader's tag/set tolerance
  ;; is exercised in the test path.
  "{:schema-version 1
    :stack \"futon\"
    :system {:arms #{:arm/arxiv :arm/superpod}}
    :sections
    [{:id \"arxana/test/leaf/1\" :kind :leaf
      :source-mtime #inst \"2026-04-19T16:06:44.612-00:00\"}
     {:id \"arxana/test/leaf/2\" :kind :leaf
      :source-mtime #inst \"2026-04-19T17:00:00.000-00:00\"}
     {:id \"arxana/test/devmap/X\" :kind :devmap}]}")

(defmacro arxana-vsatarcs-belief-test--with-fixture (var-symbol &rest body)
  "Bind VAR-SYMBOL to a temp file containing the stack fixture; run BODY."
  (declare (indent 1))
  `(let ((,var-symbol (make-temp-file "vsatarcs-stack-" nil ".edn"
                                      arxana-vsatarcs-belief-test--stack-fixture)))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,var-symbol) (delete-file ,var-symbol)))))

(ert-deftest arxana-vsatarcs-belief-section-ids-tolerates-tags-and-sets ()
  "Extended EDN reader handles #inst tagged literals and #{} sets."
  (arxana-vsatarcs-belief-test--with-fixture fix
    (let ((ids (arxana-vsatarcs-belief--section-ids-from-stack-annotations fix)))
      (should (equal ids
                     '("arxana/test/leaf/1"
                       "arxana/test/leaf/2"
                       "arxana/test/devmap/X"))))))

(ert-deftest arxana-vsatarcs-belief-bootstrap-adds-uniform-priors-for-new-ids ()
  "Bootstrap seeds uniform priors for every section id not yet tracked."
  (arxana-vsatarcs-belief-test--with-fixture fix
    (arxana-vsatarcs-belief-reset)
    (let ((added (arxana-vsatarcs-belief-bootstrap-from-stack-annotations fix)))
      (should (= 3 added))
      (dolist (id '("arxana/test/leaf/1" "arxana/test/leaf/2" "arxana/test/devmap/X"))
        (let ((post (cdr (assoc id (arxana-vsatarcs-belief-current)))))
          (should post)
          (should (arxana-vsatarcs-belief-valid-posterior-p post))
          ;; uniform prior assigns equal probability to every status
          (should (< (abs (- (cdr (assoc 'spawned post))
                             (/ 1.0 (length arxana-vsatarcs-belief-status-set))))
                     1e-9)))))
    (arxana-vsatarcs-belief-reset)))

(ert-deftest arxana-vsatarcs-belief-bootstrap-preserves-existing-evidence ()
  "Bootstrap does NOT clobber entities that already carry accumulated evidence."
  (arxana-vsatarcs-belief-test--with-fixture fix
    (arxana-vsatarcs-belief-reset)
    ;; Pre-existing entity with strong evidence
    (arxana-vsatarcs-belief-ingest-events
     '((:entity-id "arxana/test/leaf/1" :type :strengthened :weight 5.0)))
    (let* ((pre-bootstrap (cdr (assoc "arxana/test/leaf/1"
                                      (arxana-vsatarcs-belief-current))))
           (pre-most-likely (arxana-vsatarcs-belief-most-likely-status
                             pre-bootstrap))
           (added (arxana-vsatarcs-belief-bootstrap-from-stack-annotations fix))
           (post-bootstrap (cdr (assoc "arxana/test/leaf/1"
                                       (arxana-vsatarcs-belief-current)))))
      (should (= 2 added)) ; only the two other ids were added
      (should (equal pre-bootstrap post-bootstrap))
      (should (eq 'strengthened pre-most-likely))
      (should (eq 'strengthened
                  (arxana-vsatarcs-belief-most-likely-status post-bootstrap))))
    (arxana-vsatarcs-belief-reset)))

(ert-deftest arxana-vsatarcs-belief-bootstrap-save-flag-persists ()
  "Bootstrap with SAVE? non-nil writes to the configured store path."
  (let ((tmp-store (make-temp-file "vsatarcs-belief-" nil ".eld")))
    (arxana-vsatarcs-belief-test--with-fixture fix
      (let ((arxana-vsatarcs-belief-store-file tmp-store))
        (unwind-protect
            (progn
              (arxana-vsatarcs-belief-reset)
              (arxana-vsatarcs-belief-bootstrap-from-stack-annotations fix t)
              (arxana-vsatarcs-belief-reset)
              (arxana-vsatarcs-belief-load)
              (dolist (id '("arxana/test/leaf/1" "arxana/test/leaf/2" "arxana/test/devmap/X"))
                (should (assoc id (arxana-vsatarcs-belief-current)))))
          (when (file-exists-p tmp-store) (delete-file tmp-store))
          (arxana-vsatarcs-belief-reset))))))

;; ---------------------------------------------------------------------
;; v0.2.4 — bilateral comparison primitive
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-belief-compare-empty-vs-empty ()
  (let ((rep (arxana-vsatarcs-belief-compare nil nil)))
    (should (null (plist-get rep :only-in-a)))
    (should (null (plist-get rep :only-in-b)))
    (should (null (plist-get rep :posterior-diffs)))
    (should (= 0 (plist-get rep :equal-count)))))

(ert-deftest arxana-vsatarcs-belief-compare-identical-beliefs ()
  (let* ((b (arxana-vsatarcs-belief-initial-state '(:m1 :m2 :m3)))
         (rep (arxana-vsatarcs-belief-compare b b)))
    (should (null (plist-get rep :only-in-a)))
    (should (null (plist-get rep :only-in-b)))
    (should (null (plist-get rep :posterior-diffs)))
    (should (= 3 (plist-get rep :equal-count)))))

(ert-deftest arxana-vsatarcs-belief-compare-only-in-a ()
  (let* ((a (arxana-vsatarcs-belief-initial-state '(:m1 :m2)))
         (b (arxana-vsatarcs-belief-initial-state '(:m1)))
         (rep (arxana-vsatarcs-belief-compare a b)))
    (should (equal '(:m2) (plist-get rep :only-in-a)))
    (should (null (plist-get rep :only-in-b)))
    (should (= 1 (plist-get rep :equal-count)))))

(ert-deftest arxana-vsatarcs-belief-compare-only-in-b ()
  (let* ((a (arxana-vsatarcs-belief-initial-state '(:m1)))
         (b (arxana-vsatarcs-belief-initial-state '(:m1 :m2 :m3)))
         (rep (arxana-vsatarcs-belief-compare a b)))
    (should (null (plist-get rep :only-in-a)))
    (should (equal '(:m2 :m3) (sort (copy-sequence (plist-get rep :only-in-b))
                                    (lambda (x y) (string< (symbol-name x)
                                                            (symbol-name y))))))
    (should (= 1 (plist-get rep :equal-count)))))

(ert-deftest arxana-vsatarcs-belief-compare-posterior-diff-detected ()
  (let* ((a (arxana-vsatarcs-belief-initial-state '(:m1)))
         (b-evidence (arxana-vsatarcs-belief-update-batch
                      (arxana-vsatarcs-belief-initial-state '(:m1))
                      '((:entity-id :m1 :type :strengthened :weight 5.0))))
         (rep (arxana-vsatarcs-belief-compare a b-evidence)))
    (should (null (plist-get rep :only-in-a)))
    (should (null (plist-get rep :only-in-b)))
    (should (= 1 (length (plist-get rep :posterior-diffs))))
    (should (equal :m1 (car (car (plist-get rep :posterior-diffs)))))
    (should (> (cdr (car (plist-get rep :posterior-diffs))) 0))
    (should (= 0 (plist-get rep :equal-count)))))

(ert-deftest arxana-vsatarcs-belief-compare-respects-epsilon ()
  ;; Two beliefs that differ by less than epsilon should be treated as equal.
  (let* ((p1 (arxana-vsatarcs-belief-uniform-prior))
         (p2 (mapcar (lambda (kv) (cons (car kv) (+ (cdr kv) 1.0e-10))) p1))
         ;; renormalise p2 so it sums to 1 (cheating — we want a tiny diff per status)
         (a (list (cons :m1 p1)))
         (b (list (cons :m1 p2)))
         (rep-strict (arxana-vsatarcs-belief-compare a b 1.0e-12))
         (rep-loose  (arxana-vsatarcs-belief-compare a b 1.0e-6)))
    ;; Strict epsilon catches the 1e-10 diff
    (should (= 1 (length (plist-get rep-strict :posterior-diffs))))
    ;; Loose epsilon does not
    (should (null (plist-get rep-loose :posterior-diffs)))
    (should (= 1 (plist-get rep-loose :equal-count)))))

(ert-deftest arxana-vsatarcs-belief-snapshot-orders-by-entropy-desc ()
  (arxana-vsatarcs-belief-reset)
  ;; :high is uniform (max entropy); :low has accumulated heavy evidence.
  (arxana-vsatarcs-belief-ingest-events
   (append
    (list '(:entity-id :high :type :spawned :weight 0.0))  ; uniform-equivalent
    (make-list 10 '(:entity-id :low :type :strengthened :weight 3.0))))
  (let* ((snap (arxana-vsatarcs-belief-snapshot))
         (eids (mapcar #'car snap)))
    (should (= 2 (length snap)))
    (should (equal :high (car eids)))
    (should (equal :low (cadr eids))))
  (arxana-vsatarcs-belief-reset))

(provide 'arxana-vsatarcs-belief-test)
;;; arxana-vsatarcs-belief-test.el ends here
