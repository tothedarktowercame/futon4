;;; arxana-links.el --- Link persistence for Arxana -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module implements Ted Nelson-style hyperlinks for Arxana with multiple
;; link tiers:
;;
;; - Ephemeral (Tier 0): Computed on-the-fly from heuristics, no persistence
;; - Strategy-bound (Tier 1): Persist the finder rules, links are recomputable
;; - Anchored (Tier 2): Persist exact regions for immutable/historical content
;;
;; Key data types:
;;
;; - Link Strategy: Declares finders that generate links within a scope
;; - Voiced Link: A promoted link that can have annotations attached
;; - Surface Form: Alternative phrasings for concepts (feeds finders)
;; - Resilient Anchor: Region reference that survives edits (content hash + context)
;; - Embedding Cache: Cached nearest-neighbors from vector spaces
;;
;; All entities persist to Futon1 via the /entity endpoint.
;;
;; Demo usage:
;;   ;; Create and persist a link strategy
;;   (arxana-links-demo-create-strategy)
;;
;;   ;; Promote a computed link to voiced
;;   (arxana-links-demo-promote-link)
;;
;;   ;; Create a resilient scholium on selected region
;;   (arxana-links-demo-create-scholium)  ; with region selected
;;
;;   ;; Capture a surface form
;;   (arxana-links-capture-surface-form "commutative thingy" "abelian-group")

;;; Code:

(require 'cl-lib)
(require 'url-util)

;; Avoid autoload issues - these are loaded via bootstrap.el
(declare-function arxana-store--request "arxana-store" (method path &optional payload query))
(declare-function arxana-store-sync-enabled-p "arxana-store" ())

(defgroup arxana-links nil
  "Link persistence settings for Arxana.
This module provides Ted Nelson-style hyperlinks with multiple persistence
tiers, from ephemeral computed links to fully anchored archival references."
  :group 'arxana
  :prefix "arxana-links-")

;;;; =========================================================================
;;;; Configuration
;;;; =========================================================================

(defcustom arxana-links-context-chars 30
  "Number of characters of context to capture before/after anchored regions."
  :type 'integer
  :group 'arxana-links)

(defcustom arxana-links-default-neighbor-k 10
  "Default number of neighbors to store per item in embedding caches."
  :type 'integer
  :group 'arxana-links)

(defcustom arxana-links-default-similarity-threshold 0.8
  "Default minimum similarity score for embedding neighbors."
  :type 'number
  :group 'arxana-links)

;;;; =========================================================================
;;;; Utility Functions
;;;; =========================================================================

(defun arxana-links--timestamp ()
  "Return current UTC timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun arxana-links--content-hash (text)
  "Compute SHA256 hash of TEXT, prefixed with 'sha256:'."
  (concat "sha256:" (secure-hash 'sha256 text)))

(defun arxana-links--generate-id (prefix &rest components)
  "Generate a deterministic ID from PREFIX and COMPONENTS."
  (format "%s:%s" prefix (md5 (format "%S" components))))

;;;; =========================================================================
;;;; Link Strategy (Tier 1 Foundation)
;;;; =========================================================================

(defconst arxana-links-finder-types
  '(:symbol-as-term :filename-mention :embedding-proximity :explicit)
  "Known finder types for link strategies.")

(cl-defun arxana-links-make-strategy (&key id scope finders created-by)
  "Create a link strategy plist.

SCOPE is a plist with keys:
  :repo        - Repository name (string)
  :code-roots  - List of directories to scan (list of strings)
  :docbook     - Target docbook name (string)

FINDERS is a list of finder plists, each with:
  :type        - One of `arxana-links-finder-types'
  :auto-link?  - Whether to show links without user action (boolean)
  Additional keys depend on finder type.

Example:
  (arxana-links-make-strategy
   :scope \\='(:repo \"futon4\" :code-roots (\"dev/\" \"test/\") :docbook \"futon4\")
   :finders \\='((:type :symbol-as-term :auto-link? t)
              (:type :filename-mention :auto-link? t)))"
  (let* ((repo (plist-get scope :repo))
         (id (or id (arxana-links--generate-id "strategy" repo finders))))
    (list :xt/id id
          :type "arxana/link-strategy"
          :scope scope
          :finders finders
          :created-at (arxana-links--timestamp)
          :created-by (or created-by user-login-name))))

(defun arxana-links-persist-strategy (strategy)
  "Persist STRATEGY to Futon1.
Returns the server response or nil on failure."
  (if (not (arxana-store-sync-enabled-p))
      (progn
        (message "[arxana-links] Cannot persist strategy: Futon sync disabled")
        nil)
    (arxana-store--request "POST" "/entity" strategy)))

(defun arxana-links-load-strategies (&optional type-filter)
  "Load all link strategies from Futon1.
Optional TYPE-FILTER limits results to strategies matching that repo."
  (when (arxana-store-sync-enabled-p)
    (let* ((query "type=arxana/link-strategy")
           (response (arxana-store--request "GET" "/entities" nil query)))
      (when response
        (let ((entities (cdr (assq :entities response))))
          (if type-filter
              (cl-remove-if-not
               (lambda (s)
                 (equal (plist-get (plist-get s :scope) :repo) type-filter))
               entities)
            entities))))))

(defun arxana-links-find-strategy (repo)
  "Find the active strategy for REPO, or nil if none exists."
  (car (arxana-links-load-strategies repo)))

;;;; =========================================================================
;;;; Voiced Link (Promoted Links)
;;;; =========================================================================

(defconst arxana-links-status-values
  '(:confirmed :suppressed :orphaned :fuzzy-matched)
  "Valid status values for voiced links and scholia.")

(cl-defun arxana-links-make-voiced-link (&key source target found-by status promoted-by)
  "Create a voiced link plist.

SOURCE is a plist describing the source endpoint:
  :type    - Endpoint type (e.g., :code-symbol)
  :file    - File path (for code)
  :symbol  - Symbol name (for code)
  :def-type - Definition type (defun, defvar, etc.)

TARGET is a plist describing the target endpoint:
  :type           - Endpoint type (e.g., :doc-paragraph)
  :docbook        - Docbook name
  :doc-id         - Document ID
  :paragraph-anchor - Optional section anchor

FOUND-BY is the strategy ID that discovered this link.
STATUS is one of `arxana-links-status-values' (default :confirmed)."
  (list :xt/id (arxana-links--generate-id "link" source target)
        :type "arxana/voiced-link"
        :source source
        :target target
        :found-by found-by
        :promoted-at (arxana-links--timestamp)
        :promoted-by (or promoted-by user-login-name)
        :status (or status :confirmed)
        :annotations nil))

(defun arxana-links-persist-voiced-link (link)
  "Persist voiced LINK to Futon1."
  (if (not (arxana-store-sync-enabled-p))
      (progn
        (message "[arxana-links] Cannot persist link: Futon sync disabled")
        nil)
    (arxana-store--request "POST" "/entity" link)))

(defun arxana-links-suppress-link (link-id)
  "Mark link LINK-ID as suppressed (false positive)."
  (when (arxana-store-sync-enabled-p)
    (arxana-store--request "POST" "/entity"
                           (list :xt/id link-id
                                 :status :suppressed
                                 :suppressed-at (arxana-links--timestamp)
                                 :suppressed-by user-login-name))))

(defun arxana-links-load-voiced-links (&optional strategy-id)
  "Load voiced links from Futon1.
Optional STRATEGY-ID filters to links found by that strategy."
  (when (arxana-store-sync-enabled-p)
    (let* ((query "type=arxana/voiced-link")
           (response (arxana-store--request "GET" "/entities" nil query)))
      (when response
        (let ((entities (cdr (assq :entities response))))
          (if strategy-id
              (cl-remove-if-not
               (lambda (link)
                 (equal (plist-get link :found-by) strategy-id))
               entities)
            entities))))))

;;;; =========================================================================
;;;; Surface Forms (Open World Ingest)
;;;; =========================================================================

(cl-defun arxana-links-make-surface-form (&key concept-id surface context source)
  "Create a surface form plist.

CONCEPT-ID is the canonical concept identifier (string).
SURFACE is the alternative text used to refer to the concept (string).
CONTEXT is a plist with:
  :doc     - Document name where this usage occurred
  :snippet - Surrounding text snippet
  :offset  - Optional character offset

SOURCE is :explicit (user marked) or :inferred (NLP detected)."
  (list :xt/id (arxana-links--generate-id "surface" concept-id surface)
        :type "arxana/surface-form"
        :concept-id concept-id
        :surface surface
        :context context
        :source (or source :explicit)
        :created-at (arxana-links--timestamp)
        :created-by user-login-name))

(defun arxana-links-persist-surface-form (form)
  "Persist surface FORM to Futon1."
  (if (not (arxana-store-sync-enabled-p))
      (progn
        (message "[arxana-links] Cannot persist surface form: Futon sync disabled")
        nil)
    (arxana-store--request "POST" "/entity" form)))

;;;###autoload
(defun arxana-links-capture-surface-form (surface concept-id)
  "Capture that SURFACE was used to refer to CONCEPT-ID.
When called interactively, prompts for both values.
If a region is selected, uses the region text as SURFACE."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Surface form: "))
         (read-string "Concept ID: ")))
  (let* ((context (list :doc (buffer-name)
                        :snippet (when (use-region-p)
                                   (buffer-substring-no-properties
                                    (max (point-min) (- (region-beginning) 20))
                                    (min (point-max) (+ (region-end) 20))))
                        :offset (when (use-region-p) (region-beginning))))
         (form (arxana-links-make-surface-form
                :concept-id concept-id
                :surface surface
                :context context
                :source :explicit)))
    (if (arxana-links-persist-surface-form form)
        (message "Captured surface form: '%s' -> %s" surface concept-id)
      (message "Failed to persist surface form (check Futon sync)"))))

(defun arxana-links-surface-forms-for-concept (concept-id)
  "Fetch all surface forms for CONCEPT-ID from Futon1."
  (when (arxana-store-sync-enabled-p)
    (let* ((query (format "type=arxana/surface-form&concept-id=%s"
                          (url-hexify-string concept-id)))
           (response (arxana-store--request "GET" "/entities" nil query)))
      (when response
        (cdr (assq :entities response))))))

;;;; =========================================================================
;;;; Resilient Anchors (Tier 2 - Region References)
;;;; =========================================================================

(defun arxana-links--extract-context (buffer start end)
  "Extract context around region START to END in BUFFER.
Uses `arxana-links-context-chars' for context length."
  (let ((chars arxana-links-context-chars))
    (with-current-buffer buffer
      (list :context-before (buffer-substring-no-properties
                             (max (point-min) (- start chars))
                             start)
            :context-after (buffer-substring-no-properties
                            end
                            (min (point-max) (+ end chars)))))))

(defun arxana-links-make-anchor (buffer start end)
  "Create a resilient anchor for region START to END in BUFFER.
The anchor uses content hash + context for resilient re-finding."
  (with-current-buffer buffer
    (let* ((content (buffer-substring-no-properties start end))
           (context (arxana-links--extract-context buffer start end)))
      (list :strategy :content-hash
            :content-hash (arxana-links--content-hash content)
            :content-length (length content)
            :context-before (plist-get context :context-before)
            :context-after (plist-get context :context-after)
            :hint-offset start
            :hint-line (line-number-at-pos start)))))

(cl-defun arxana-links-make-scholium (&key target-doc target-doc-version anchor content content-type)
  "Create a scholium (region annotation) plist.

TARGET-DOC is the document name/id being annotated.
TARGET-DOC-VERSION is optional version identifier (for pinning).
ANCHOR is created by `arxana-links-make-anchor'.
CONTENT is the annotation text.
CONTENT-TYPE defaults to \"text/plain\"."
  (list :xt/id (arxana-links--generate-id "scholium" target-doc anchor content)
        :type "arxana/scholium"
        :target-doc target-doc
        :target-doc-version target-doc-version
        :anchor anchor
        :content content
        :content-type (or content-type "text/plain")
        :status :anchored
        :created-at (arxana-links--timestamp)
        :created-by user-login-name
        :last-verified (arxana-links--timestamp)))

(defun arxana-links-persist-scholium (scholium)
  "Persist SCHOLIUM to Futon1."
  (if (not (arxana-store-sync-enabled-p))
      (progn
        (message "[arxana-links] Cannot persist scholium: Futon sync disabled")
        nil)
    (arxana-store--request "POST" "/entity" scholium)))

;;;; =========================================================================
;;;; Anchor Re-finding (Resilience)
;;;; =========================================================================

(defun arxana-links-find-anchor (buffer anchor)
  "Try to find ANCHOR in BUFFER.
Returns (start . end) on success, nil on failure.
Tries three strategies: exact offset, context pattern, fuzzy context."
  (with-current-buffer buffer
    (let* ((hash (plist-get anchor :content-hash))
           (len (plist-get anchor :content-length))
           (ctx-before (plist-get anchor :context-before))
           (ctx-after (plist-get anchor :context-after))
           (hint-offset (plist-get anchor :hint-offset))
           (result nil))

      ;; Strategy 1: Try hint offset first (fastest)
      (when (and (not result)
                 hint-offset
                 (>= (point-max) (+ hint-offset len)))
        (let* ((candidate-start hint-offset)
               (candidate-end (+ hint-offset len))
               (candidate (ignore-errors
                            (buffer-substring-no-properties candidate-start candidate-end)))
               (candidate-hash (and candidate
                                    (arxana-links--content-hash candidate))))
          (when (equal candidate-hash hash)
            (setq result (cons candidate-start candidate-end)))))

      ;; Strategy 2: Search for context pattern (medium)
      (when (and (not result)
                 ctx-before ctx-after
                 (> (length ctx-before) 0)
                 (> (length ctx-after) 0))
        (save-excursion
          (goto-char (point-min))
          (let ((pattern (concat (regexp-quote ctx-before)
                                 "\\(.\\{" (number-to-string len) "\\}\\)"
                                 (regexp-quote ctx-after))))
            (when (re-search-forward pattern nil t)
              (let* ((match-start (match-beginning 1))
                     (match-end (match-end 1))
                     (match-text (match-string 1))
                     (match-hash (arxana-links--content-hash match-text)))
                (when (equal match-hash hash)
                  (setq result (cons match-start match-end))))))))

      ;; Strategy 3: Fuzzy search using context only (slowest, may not verify hash)
      (when (and (not result)
                 ctx-before ctx-after
                 (> (length ctx-before) 5))
        (save-excursion
          (goto-char (point-min))
          (when (search-forward ctx-before nil t)
            (let ((start (point)))
              (when (search-forward ctx-after nil t)
                (let ((end (match-beginning 0)))
                  (when (and (> end start)
                             (<= (- end start) (* len 2))) ; sanity check
                    (setq result (cons start end)))))))))

      result)))

(defun arxana-links-verify-scholium (buffer scholium)
  "Verify SCHOLIUM's anchor in BUFFER.
Updates the scholium's :status and :last-verified fields.
Returns the found bounds (start . end) or nil if orphaned."
  (let* ((anchor (plist-get scholium :anchor))
         (found (arxana-links-find-anchor buffer anchor)))
    (if found
        (progn
          (plist-put scholium :status :anchored)
          (plist-put scholium :last-verified (arxana-links--timestamp))
          found)
      (plist-put scholium :status :orphaned)
      nil)))

(defun arxana-links-load-scholia-for-doc (doc-name)
  "Load all scholia for DOC-NAME from Futon1."
  (when (arxana-store-sync-enabled-p)
    (let* ((query (format "type=arxana/scholium&target-doc=%s"
                          (url-hexify-string doc-name)))
           (response (arxana-store--request "GET" "/entities" nil query)))
      (when response
        (cdr (assq :entities response))))))

;;;; =========================================================================
;;;; Embedding Cache (Vector Space Neighbors)
;;;; =========================================================================

(cl-defun arxana-links-make-embedding-cache (&key space model dimensions neighbors k threshold)
  "Create an embedding cache plist.

SPACE is the embedding space identifier (string).
MODEL is the model name (e.g., \"glove\").
DIMENSIONS is the vector dimension count.
NEIGHBORS is an alist mapping item IDs to neighbor lists.
  Each neighbor is a plist with :id and :score.
K is the number of neighbors stored per item.
THRESHOLD is the minimum similarity score."
  (list :xt/id (format "embedding-cache:%s" space)
        :type "arxana/embedding-cache"
        :space space
        :model model
        :dimensions dimensions
        :neighbors neighbors
        :k (or k arxana-links-default-neighbor-k)
        :threshold (or threshold arxana-links-default-similarity-threshold)
        :computed-at (arxana-links--timestamp)))

(defun arxana-links-persist-embedding-cache (cache)
  "Persist embedding CACHE to Futon1."
  (if (not (arxana-store-sync-enabled-p))
      (progn
        (message "[arxana-links] Cannot persist embedding cache: Futon sync disabled")
        nil)
    (arxana-store--request "POST" "/entity" cache)))

(defun arxana-links-load-embedding-cache (space)
  "Load embedding cache for SPACE from Futon1."
  (when (arxana-store-sync-enabled-p)
    (arxana-store--request "GET"
                           (format "/entity/%s"
                                   (url-hexify-string
                                    (format "embedding-cache:%s" space))))))

(defun arxana-links-get-neighbors (space item-id &optional k)
  "Get K nearest neighbors for ITEM-ID from SPACE's embedding cache.
K defaults to 5. Returns list of plists with :id and :score."
  (let* ((k (or k 5))
         (cache (arxana-links-load-embedding-cache space)))
    (when cache
      (let* ((neighbors-alist (plist-get cache :neighbors))
             (item-neighbors (cdr (assoc item-id neighbors-alist))))
        (cl-subseq item-neighbors 0 (min k (length item-neighbors)))))))

;;;; =========================================================================
;;;; Embedding Computation Helpers
;;;; =========================================================================

(defun arxana-links--cosine-similarity (vec1 vec2)
  "Compute cosine similarity between vectors VEC1 and VEC2.
Both should be sequences of numbers of the same length."
  (let ((dot 0.0) (norm1 0.0) (norm2 0.0)
        (len (min (length vec1) (length vec2))))
    (dotimes (i len)
      (let ((v1 (elt vec1 i))
            (v2 (elt vec2 i)))
        (setq dot (+ dot (* v1 v2))
              norm1 (+ norm1 (* v1 v1))
              norm2 (+ norm2 (* v2 v2)))))
    (if (or (zerop norm1) (zerop norm2))
        0.0
      (/ dot (* (sqrt norm1) (sqrt norm2))))))

(defun arxana-links-compute-neighbors (embeddings &optional k threshold)
  "Compute nearest neighbors from EMBEDDINGS hash table.
EMBEDDINGS maps item IDs to vectors.
K is neighbors per item (default `arxana-links-default-neighbor-k').
THRESHOLD is minimum similarity (default `arxana-links-default-similarity-threshold').
Returns an alist suitable for embedding cache :neighbors field."
  (let ((k (or k arxana-links-default-neighbor-k))
        (threshold (or threshold arxana-links-default-similarity-threshold))
        (result '()))
    (maphash
     (lambda (id vec)
       (let ((scores '()))
         (maphash
          (lambda (other-id other-vec)
            (unless (equal id other-id)
              (let ((sim (arxana-links--cosine-similarity vec other-vec)))
                (when (>= sim threshold)
                  (push (list :id other-id :score sim) scores)))))
          embeddings)
         ;; Sort by score descending and take top K
         (setq scores (sort scores (lambda (a b)
                                     (> (plist-get a :score)
                                        (plist-get b :score)))))
         (push (cons id (cl-subseq scores 0 (min k (length scores)))) result)))
     embeddings)
    (nreverse result)))

;;;; =========================================================================
;;;; Interactive Demo Functions
;;;; =========================================================================

;;;###autoload
(defun arxana-links-demo-create-strategy ()
  "Demo: Create and persist a link strategy for futon4 code-docs.
This demonstrates Tier 1 persistence - storing finder rules, not individual links."
  (interactive)
  (let* ((scope (list :repo "futon4"
                      :code-roots '("dev/" "test/")
                      :docbook "futon4"))
         (finders (list (list :type :symbol-as-term
                              :def-patterns '("defun" "defmacro" "defvar" "defcustom")
                              :auto-link? t)
                        (list :type :filename-mention
                              :auto-link? t)))
         (strategy (arxana-links-make-strategy :scope scope :finders finders)))
    (message "Creating strategy: %s" (plist-get strategy :xt/id))
    (if (arxana-links-persist-strategy strategy)
        (message "Strategy persisted successfully!")
      (message "Failed to persist strategy"))))

;;;###autoload
(defun arxana-links-demo-promote-link ()
  "Demo: Promote an ad-hoc link to a voiced (persisted) link.
Prompts for source and target information."
  (interactive)
  (let* ((source-file (read-file-name "Source file: " nil nil t))
         (source-symbol (read-string "Source symbol: "))
         (target-docbook (read-string "Target docbook: " "futon4"))
         (target-doc-id (read-string "Target doc ID: "))
         (source (list :type :code-symbol
                       :file (file-relative-name source-file)
                       :symbol source-symbol))
         (target (list :type :doc-paragraph
                       :docbook target-docbook
                       :doc-id target-doc-id))
         (link (arxana-links-make-voiced-link
                :source source
                :target target
                :status :confirmed)))
    (if (arxana-links-persist-voiced-link link)
        (message "Link promoted: %s -> %s" source-symbol target-doc-id)
      (message "Failed to persist link"))))

;;;###autoload
(defun arxana-links-demo-create-scholium ()
  "Demo: Create a resilient scholium on the selected region.
The scholium uses content hash + context for resilient anchoring."
  (interactive)
  (unless (use-region-p)
    (user-error "Select a region first"))
  (let* ((content (read-string "Annotation: "))
         (anchor (arxana-links-make-anchor
                  (current-buffer)
                  (region-beginning)
                  (region-end)))
         (scholium (arxana-links-make-scholium
                    :target-doc (buffer-name)
                    :anchor anchor
                    :content content)))
    (if (arxana-links-persist-scholium scholium)
        (message "Scholium created with resilient anchor")
      (message "Failed to persist scholium"))))

;;;###autoload
(defun arxana-links-demo-verify-anchor ()
  "Demo: Verify that an anchor can be re-found after edits.
Creates a temporary buffer, makes an anchor, edits the buffer,
then verifies the anchor still resolves."
  (interactive)
  (with-temp-buffer
    (insert "prefix text TARGET REGION suffix text more content here")
    (let* ((anchor (arxana-links-make-anchor (current-buffer) 13 26)))
      (message "Created anchor for 'TARGET REGION'")
      (message "  Hash: %s" (plist-get anchor :content-hash))

      ;; Verify initial find
      (let ((found (arxana-links-find-anchor (current-buffer) anchor)))
        (if found
            (message "  Initial find: positions %d-%d = '%s'"
                     (car found) (cdr found)
                     (buffer-substring-no-properties (car found) (cdr found)))
          (message "  Initial find: FAILED")))

      ;; Insert text before region
      (goto-char (point-min))
      (insert "NEW STUFF INSERTED HERE ")
      (message "  After inserting text at start...")

      ;; Verify still findable
      (let ((found (arxana-links-find-anchor (current-buffer) anchor)))
        (if found
            (message "  Re-find: positions %d-%d = '%s'"
                     (car found) (cdr found)
                     (buffer-substring-no-properties (car found) (cdr found)))
          (message "  Re-find: FAILED (anchor orphaned)"))))))

;;;###autoload
(defun arxana-links-status ()
  "Display the current status of arxana-links subsystem."
  (interactive)
  (let ((buf (get-buffer-create "*Arxana Links Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Arxana Links Status\n")
        (insert "===================\n\n")
        (insert (format "Futon sync enabled: %s\n"
                        (if (arxana-store-sync-enabled-p) "yes" "no")))
        (insert (format "Context chars: %d\n" arxana-links-context-chars))
        (insert (format "Default neighbor K: %d\n" arxana-links-default-neighbor-k))
        (insert (format "Default similarity threshold: %.2f\n"
                        arxana-links-default-similarity-threshold))

        (insert "\nPersisted Strategies:\n")
        (let ((strategies (arxana-links-load-strategies)))
          (if strategies
              (dolist (s strategies)
                (insert (format "  - %s (repo: %s)\n"
                                (plist-get s :xt/id)
                                (plist-get (plist-get s :scope) :repo))))
            (insert "  (none, or Futon unavailable)\n")))

        (insert "\nDemo Commands:\n")
        (insert "  M-x arxana-links-demo-create-strategy\n")
        (insert "  M-x arxana-links-demo-promote-link\n")
        (insert "  M-x arxana-links-demo-create-scholium\n")
        (insert "  M-x arxana-links-demo-verify-anchor\n")
        (insert "  M-x arxana-links-capture-surface-form\n")

        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))

(provide 'arxana-links)

;;; arxana-links.el ends here
