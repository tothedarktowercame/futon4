;;; arxana-store-qa.el --- Non-interactive QA for arxana-store writes -*- lexical-binding: t; -*-

;; Run with:
;;   emacs -Q --batch \
;;     -L /home/joe/code/futon4/dev \
;;     --eval "(setq futon4-base-url \"http://localhost:8080/api/alpha\" futon4-enable-sync t)" \
;;     -l arxana-store-qa.el

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-store)

(defun arxana-store-qa--fail (fmt &rest args)
  (princ (apply #'format (concat "QA FAIL: " fmt "\n") args))
  (kill-emacs 1))

(defun arxana-store-qa--ok (fmt &rest args)
  (princ (apply #'format (concat "QA OK: " fmt "\n") args)))

(defun arxana-store-qa--assert (pred fmt &rest args)
  (unless pred
    (apply #'arxana-store-qa--fail fmt args)))

(defun arxana-store-qa--fetch-entity-or-nil (id)
  (let* ((resp (ignore-errors (arxana-store-fetch-entity id)))
         (entity (and (listp resp) (alist-get :entity resp))))
    entity))

(defun arxana-store-qa--entity-source (entity)
  (let ((entity-source (and (listp entity)
                            (or (alist-get :entity/source entity)
                                (alist-get 'entity/source entity))))
        (source (and (listp entity)
                     (or (alist-get :source entity)
                         (alist-get 'source entity)))))
    (or (and (stringp entity-source) (not (string-empty-p entity-source)) entity-source)
        (and (stringp source) (not (string-empty-p source)) source)
        "")))

(defun arxana-store-qa--rand-suffix ()
  (substring (md5 (format "%s:%s" (float-time) (random))) 0 10))

(defun arxana-store-qa-run ()
  "Exercise arxana-store write paths against the configured futon4-base-url."
  (arxana-store-qa--assert (arxana-store-sync-enabled-p)
                           "Sync disabled (futon4-enable-sync must be non-nil)")
  (arxana-store-qa--assert (and (boundp 'futon4-base-url) (stringp futon4-base-url)
                                (not (string-empty-p futon4-base-url)))
                           "futon4-base-url not set")
  (arxana-store-clear-error)
  (let* ((suffix (arxana-store-qa--rand-suffix))
         (a-name (format "qa/entity-a/%s" suffix))
         (b-name (format "qa/entity-b/%s" suffix))
         (a (arxana-store-ensure-entity :name a-name :type "thing" :source "qa"))
         (b (arxana-store-ensure-entity :name b-name :type "thing" :source "qa"))
         (a-entity (and (listp a) (alist-get :entity a)))
         (b-entity (and (listp b) (alist-get :entity b)))
         (a-id (and (listp a-entity) (alist-get :id a-entity)))
         (b-id (and (listp b-entity) (alist-get :id b-entity))))
    (arxana-store-qa--assert (and (stringp a-id) (stringp b-id))
                             "ensure-entity did not return ids: %S %S" a b)
    (arxana-store-qa--ok "ensure-entity: %s %s" a-id b-id)

    ;; Single relation write (label in props -> provenance note).
    (let* ((rel (arxana-store-create-relation :src a-id :dst b-id
                                              :type "arxana/scholium"
                                              :label (format "qa-label-%s" suffix)
                                              :props (list (cons 'label (format "qa-label-%s" suffix))
                                                           (cons 'qa t)))))
      (arxana-store-qa--assert rel "create-relation returned nil (see arxana-store-last-error)")
      (arxana-store-qa--ok "create-relation"))

    ;; Batch relation write (exercises /relations/batch).
    (let* ((c-name (format "qa/entity-c/%s" suffix))
           (c (arxana-store-ensure-entity :name c-name :type "thing" :source "qa"))
           (c-entity (and (listp c) (alist-get :entity c)))
           (c-id (and (listp c-entity) (alist-get :id c-entity)))
           (rels (list (list (cons 'type "arxana/scholium")
                             (cons 'src a-id)
                             (cons 'dst c-id)
                             (cons 'props (list (cons 'label "qa-batch-1"))))
                       (list (cons 'type "arxana/scholium")
                             (cons 'src b-id)
                             (cons 'dst c-id)
                             (cons 'props (list (cons 'label "qa-batch-2"))))))
           (resp (arxana-store-create-relations-batch rels)))
      (arxana-store-qa--assert resp "relations-batch returned nil (see arxana-store-last-error)")
      (arxana-store-qa--ok "relations-batch"))

    ;; Media lyrics upsert + verify read-back durability contract.
    ;; Use a synthetic misc sha so we don't depend on local files.
    (let* ((sha (make-string 64 ?a))
           (track-id (format "arxana/media/misc/%s" sha))
           (lyrics-id (format "arxana/media-lyrics/misc/%s" sha))
           (lyrics (format "qa-lyrics-%s\nline2" suffix))
           (track (list (cons 'id track-id)
                        (cons 'name (format "qa-track-%s" suffix))
                        (cons 'type "arxana/media-track")
                        (cons 'external-id track-id)
                        (cons 'media/sha256 sha)))
           (lyrics-payload (list (cons 'id lyrics-id)
                                 (cons 'name (format "qa-track-%s (lyrics)" suffix))
                                 (cons 'type "arxana/media-lyrics")
                                 (cons 'external-id lyrics-id)
                                 (cons 'media/sha256 sha)
                                 (cons 'source lyrics)
                                 (cons 'entity/source lyrics)))
           (_resp (arxana-store-upsert-media-lyrics
                   :track track
                   :lyrics lyrics-payload
                   :relation (list (cons 'type ":media/lyrics")
                                   (cons 'src track-id)
                                   (cons 'dst lyrics-id))))
           (entity (arxana-store-qa--fetch-entity-or-nil lyrics-id)))
      (arxana-store-qa--assert entity "lyrics entity not readable after write: %s" lyrics-id)
      (let ((stored-sha (alist-get :media/sha256 entity))
            (stored-source (arxana-store-qa--entity-source entity)))
        (arxana-store-qa--assert (and (stringp stored-sha) (string= stored-sha sha))
                                 "lyrics :media/sha256 mismatch: got=%S want=%S" stored-sha sha)
        (arxana-store-qa--assert (string= stored-source lyrics)
                                 "lyrics :source mismatch (len %d vs %d)" (length stored-source) (length lyrics)))
      (arxana-store-qa--ok "media/lyrics upsert + readback"))

    (arxana-store-qa--ok "all checks passed")
    (kill-emacs 0)))

(arxana-store-qa-run)

;;; arxana-store-qa.el ends here
