;;; arxana-links-test.el --- Tests for arxana-links -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the arxana-links module.
;; Run with: bash dev/run-tests.sh test/arxana-links-test.el

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(require 'arxana-links)

;;;; =========================================================================
;;;; Unit Tests - Data Structure Creation
;;;; =========================================================================

(ert-deftest arxana-links-test-make-strategy ()
  "Test strategy creation with required fields."
  (let* ((scope '(:repo "test-repo" :code-roots ("src/") :docbook "test-docs"))
         (finders '((:type :symbol-as-term :auto-link? t)))
         (strategy (arxana-links-make-strategy :scope scope :finders finders)))
    (should (stringp (plist-get strategy :xt/id)))
    (should (string-prefix-p "strategy:" (plist-get strategy :xt/id)))
    (should (equal (plist-get strategy :type) "arxana/link-strategy"))
    (should (equal (plist-get strategy :scope) scope))
    (should (equal (plist-get strategy :finders) finders))
    (should (stringp (plist-get strategy :created-at)))))

(ert-deftest arxana-links-test-make-voiced-link ()
  "Test voiced link creation."
  (let* ((source '(:type :code-symbol :file "test.el" :symbol "test-fn"))
         (target '(:type :doc-paragraph :docbook "docs" :doc-id "doc-123"))
         (link (arxana-links-make-voiced-link
                :source source
                :target target
                :found-by "strategy:test"
                :status :confirmed)))
    (should (stringp (plist-get link :xt/id)))
    (should (string-prefix-p "link:" (plist-get link :xt/id)))
    (should (equal (plist-get link :type) "arxana/voiced-link"))
    (should (equal (plist-get link :source) source))
    (should (equal (plist-get link :target) target))
    (should (equal (plist-get link :status) :confirmed))
    (should (null (plist-get link :annotations)))))

(ert-deftest arxana-links-test-make-surface-form ()
  "Test surface form creation."
  (let* ((context '(:doc "notes" :snippet "the thing is"))
         (form (arxana-links-make-surface-form
                :concept-id "concept-123"
                :surface "the thing"
                :context context
                :source :explicit)))
    (should (stringp (plist-get form :xt/id)))
    (should (string-prefix-p "surface:" (plist-get form :xt/id)))
    (should (equal (plist-get form :type) "arxana/surface-form"))
    (should (equal (plist-get form :concept-id) "concept-123"))
    (should (equal (plist-get form :surface) "the thing"))
    (should (equal (plist-get form :source) :explicit))))

(ert-deftest arxana-links-test-make-scholium ()
  "Test scholium creation with anchor."
  (with-temp-buffer
    (insert "prefix TARGET REGION suffix")
    (let* ((anchor (arxana-links-make-anchor (current-buffer) 8 21))
           (scholium (arxana-links-make-scholium
                      :target-doc "test-doc"
                      :anchor anchor
                      :content "My annotation")))
      (should (stringp (plist-get scholium :xt/id)))
      (should (string-prefix-p "scholium:" (plist-get scholium :xt/id)))
      (should (equal (plist-get scholium :type) "arxana/scholium"))
      (should (equal (plist-get scholium :target-doc) "test-doc"))
      (should (equal (plist-get scholium :content) "My annotation"))
      (should (equal (plist-get scholium :status) :anchored)))))

;;;; =========================================================================
;;;; Unit Tests - Anchor Creation and Finding
;;;; =========================================================================

(ert-deftest arxana-links-test-make-anchor ()
  "Test anchor creation captures correct data."
  (with-temp-buffer
    (insert "prefix text TARGET REGION suffix text")
    (let ((anchor (arxana-links-make-anchor (current-buffer) 13 26)))
      (should (eq (plist-get anchor :strategy) :content-hash))
      (should (string-prefix-p "sha256:" (plist-get anchor :content-hash)))
      (should (equal (plist-get anchor :content-length) 13))
      (should (stringp (plist-get anchor :context-before)))
      (should (stringp (plist-get anchor :context-after)))
      (should (equal (plist-get anchor :hint-offset) 13))
      (should (numberp (plist-get anchor :hint-line))))))

(ert-deftest arxana-links-test-find-anchor-exact ()
  "Test anchor finding at exact offset."
  (with-temp-buffer
    (insert "prefix text TARGET REGION suffix text")
    (let* ((anchor (arxana-links-make-anchor (current-buffer) 13 26))
           (found (arxana-links-find-anchor (current-buffer) anchor)))
      (should found)
      (should (equal (car found) 13))
      (should (equal (cdr found) 26))
      (should (equal (buffer-substring-no-properties (car found) (cdr found))
                     "TARGET REGION")))))

(ert-deftest arxana-links-test-find-anchor-after-insert ()
  "Test anchor finding after text is inserted before region."
  (with-temp-buffer
    (insert "prefix text TARGET REGION suffix text")
    (let ((anchor (arxana-links-make-anchor (current-buffer) 13 26)))
      ;; Insert text at beginning
      (goto-char (point-min))
      (insert "NEW STUFF ")
      ;; Anchor should still be findable via context
      (let ((found (arxana-links-find-anchor (current-buffer) anchor)))
        (should found)
        (should (equal (buffer-substring-no-properties (car found) (cdr found))
                       "TARGET REGION"))))))

(ert-deftest arxana-links-test-find-anchor-orphaned ()
  "Test anchor marked orphaned when content changes completely."
  (with-temp-buffer
    (insert "prefix text TARGET REGION suffix text")
    (let ((anchor (arxana-links-make-anchor (current-buffer) 13 26)))
      ;; Replace the content entirely
      (erase-buffer)
      (insert "completely different content")
      ;; Anchor should not be found
      (let ((found (arxana-links-find-anchor (current-buffer) anchor)))
        (should (null found))))))

;;;; =========================================================================
;;;; Unit Tests - Scholium Verification
;;;; =========================================================================

(ert-deftest arxana-links-test-verify-scholium-success ()
  "Test scholium verification updates status correctly."
  (with-temp-buffer
    (insert "prefix TARGET REGION suffix")
    (let* ((anchor (arxana-links-make-anchor (current-buffer) 8 21))
           (scholium (arxana-links-make-scholium
                      :target-doc "test"
                      :anchor anchor
                      :content "note")))
      (let ((found (arxana-links-verify-scholium (current-buffer) scholium)))
        (should found)
        (should (equal (plist-get scholium :status) :anchored))
        (should (stringp (plist-get scholium :last-verified)))))))

(ert-deftest arxana-links-test-verify-scholium-orphaned ()
  "Test scholium marked orphaned when anchor not found."
  (with-temp-buffer
    (insert "prefix TARGET REGION suffix")
    (let* ((anchor (arxana-links-make-anchor (current-buffer) 8 21))
           (scholium (arxana-links-make-scholium
                      :target-doc "test"
                      :anchor anchor
                      :content "note")))
      ;; Change buffer content
      (erase-buffer)
      (insert "different content")
      (let ((found (arxana-links-verify-scholium (current-buffer) scholium)))
        (should (null found))
        (should (equal (plist-get scholium :status) :orphaned))))))

;;;; =========================================================================
;;;; Unit Tests - Embedding Helpers
;;;; =========================================================================

(ert-deftest arxana-links-test-cosine-similarity ()
  "Test cosine similarity computation."
  ;; Identical vectors should have similarity 1.0
  (should (= (arxana-links--cosine-similarity [1 0 0] [1 0 0]) 1.0))
  ;; Orthogonal vectors should have similarity 0.0
  (should (= (arxana-links--cosine-similarity [1 0 0] [0 1 0]) 0.0))
  ;; Opposite vectors should have similarity -1.0
  (should (= (arxana-links--cosine-similarity [1 0 0] [-1 0 0]) -1.0))
  ;; Check a known case: 45 degrees
  (let ((sim (arxana-links--cosine-similarity [1 0] [1 1])))
    (should (< (abs (- sim (/ 1.0 (sqrt 2.0)))) 0.001))))

(ert-deftest arxana-links-test-make-embedding-cache ()
  "Test embedding cache creation."
  (let ((cache (arxana-links-make-embedding-cache
                :space "test-space"
                :model "test-model"
                :dimensions 50
                :neighbors '(("a" . ((:id "b" :score 0.9))))
                :k 5
                :threshold 0.7)))
    (should (equal (plist-get cache :xt/id) "embedding-cache:test-space"))
    (should (equal (plist-get cache :type) "arxana/embedding-cache"))
    (should (equal (plist-get cache :space) "test-space"))
    (should (equal (plist-get cache :k) 5))
    (should (equal (plist-get cache :threshold) 0.7))))

(ert-deftest arxana-links-test-compute-neighbors ()
  "Test neighbor computation from embeddings."
  (let* ((embeddings (make-hash-table :test 'equal)))
    (puthash "a" [1.0 0.0 0.0] embeddings)
    (puthash "b" [0.9 0.1 0.0] embeddings)  ; close to a
    (puthash "c" [0.0 1.0 0.0] embeddings)  ; orthogonal to a
    (let ((neighbors (arxana-links-compute-neighbors embeddings 2 0.5)))
      ;; Should find b as neighbor of a (sim > 0.5)
      (let ((a-neighbors (cdr (assoc "a" neighbors))))
        (should a-neighbors)
        (should (equal (plist-get (car a-neighbors) :id) "b")))
      ;; c should not be neighbor of a (sim = 0)
      (let ((a-neighbors (cdr (assoc "a" neighbors))))
        (should (not (cl-find "c" a-neighbors
                              :key (lambda (x) (plist-get x :id))
                              :test 'equal)))))))

;;;; =========================================================================
;;;; Unit Tests - Utility Functions
;;;; =========================================================================

(ert-deftest arxana-links-test-content-hash ()
  "Test content hash is deterministic."
  (let ((hash1 (arxana-links--content-hash "test content"))
        (hash2 (arxana-links--content-hash "test content"))
        (hash3 (arxana-links--content-hash "different")))
    (should (string-prefix-p "sha256:" hash1))
    (should (equal hash1 hash2))
    (should (not (equal hash1 hash3)))))

(ert-deftest arxana-links-test-generate-id ()
  "Test ID generation is deterministic."
  (let ((id1 (arxana-links--generate-id "prefix" "a" "b"))
        (id2 (arxana-links--generate-id "prefix" "a" "b"))
        (id3 (arxana-links--generate-id "prefix" "a" "c")))
    (should (string-prefix-p "prefix:" id1))
    (should (equal id1 id2))
    (should (not (equal id1 id3)))))

;;;; =========================================================================
;;;; Integration Test Stubs (require Futon)
;;;; =========================================================================

;; These tests are skipped when Futon sync is disabled.
;; Run with a live Futon server to exercise persistence.

(ert-deftest arxana-links-test-strategy-roundtrip ()
  "Test strategy persist and reload (requires Futon)."
  :tags '(:integration)
  (skip-unless (arxana-store-sync-enabled-p))
  (let* ((scope '(:repo "test-integration" :code-roots ("src/") :docbook "test"))
         (finders '((:type :symbol-as-term :auto-link? t)))
         (strategy (arxana-links-make-strategy :scope scope :finders finders))
         (strategy-id (plist-get strategy :xt/id)))
    ;; Persist
    (should (arxana-links-persist-strategy strategy))
    ;; Reload and verify
    (let ((loaded (arxana-links-load-strategies "test-integration")))
      (should loaded)
      (should (cl-find strategy-id loaded
                       :key (lambda (s) (plist-get s :xt/id))
                       :test 'equal)))))

(provide 'arxana-links-test)

;;; arxana-links-test.el ends here
