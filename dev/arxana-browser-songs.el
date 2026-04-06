;;; arxana-browser-songs.el --- Songs browser for Arxana -*- lexical-binding: t; -*-

;;; Commentary:
;; XTDB-backed browser helpers for song and lyrics entities.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'seq)
(require 'subr-x)
(require 'view)

(require 'arxana-store)
(require 'arxana-ui)

(declare-function arxana-store-fetch-entities-latest "arxana-store" (&rest args))
(declare-function arxana-store-fetch-entity "arxana-store" (id &optional version as-of))
(declare-function arxana-store-fetch-hyperedges "arxana-store" (&rest args))
(declare-function arxana-store-create-hyperedge "arxana-store" (&rest args))
(declare-function arxana-store-ensure-sync "arxana-store" (&optional prompt))
(declare-function arxana-store-ensure-entity "arxana-store" (&rest args))

(defgroup arxana-browser-songs nil
  "Songs browser."
  :group 'arxana)

(defcustom arxana-browser-songs-latest-limit 200
  "Maximum number of song-like entities to request per catalog."
  :type 'integer
  :group 'arxana-browser-songs)

(defcustom arxana-browser-songs-catalogs
  '((:id chapbook
     :label "Chapbook lyrics"
     :description "XTDB-backed lyric entities already imported."
     :view songs-chapbook
     :entity-types ("arxana/media-lyrics"))
    (:id suite
     :label "Suite songs"
     :description "New suite-song entrypoint; ready for imported score texts."
     :view songs-suite
     :entity-types ("arxana/song" "arxana/song-lyrics")))
  "Catalog definitions used by the songs browser."
  :type 'sexp
  :group 'arxana-browser-songs)

(defcustom arxana-browser-songs-suite-default-file
  "/home/joe/code/music/nightmarish-suite.tex"
  "Default suite source imported into `arxana/song' entities."
  :type 'file
  :group 'arxana-browser-songs)

(defcustom arxana-browser-songs-annotations-limit 200
  "Maximum number of annotation hyperedges to request per entity."
  :type 'integer
  :group 'arxana-browser-songs)

(defcustom arxana-browser-songs-text-buffer "*Arxana Song*"
  "Buffer name used for song/chapbook text in the songs browser."
  :type 'string
  :group 'arxana-browser-songs)

(defcustom arxana-browser-songs-notes-buffer "*Arxana Song Notes*"
  "Buffer name used for annotation notes in the songs browser."
  :type 'string
  :group 'arxana-browser-songs)

(defcustom arxana-browser-songs-notes-side 'right
  "Side for the annotation notes window."
  :type '(choice (const left) (const right))
  :group 'arxana-browser-songs)

(defcustom arxana-browser-songs-notes-width 0.45
  "Width for the annotation notes side window."
  :type 'number
  :group 'arxana-browser-songs)

(defcustom arxana-browser-songs-highlight-scope-default 'all
  "Default highlight scope for songs text buffers.
`all' keeps all annotated spans tinted.
`point' only highlights the annotated span at point."
  :type '(choice (const :tag "All annotations" all)
                 (const :tag "Only current annotation" point))
  :group 'arxana-browser-songs)

(defface arxana-browser-songs-annotation-face
  '((t :background "#eef6ff"))
  "Face for positively annotated passages."
  :group 'arxana-browser-songs)

(defface arxana-browser-songs-question-face
  '((t :background "#fff4e5"))
  "Face for open-question passages."
  :group 'arxana-browser-songs)

(defface arxana-browser-songs-marker-face
  '((t :foreground "#2563eb" :weight bold))
  "Face for inline annotation markers."
  :group 'arxana-browser-songs)

(defface arxana-browser-songs-active-face
  '((t :background "#dbeafe"))
  "Face for the actively focused songs annotation."
  :group 'arxana-browser-songs)

(defface arxana-browser-songs-note-link-face
  '((t :inherit link))
  "Face for actionable note lines in the songs browser."
  :group 'arxana-browser-songs)

(defface arxana-browser-songs-note-quote-face
  '((t :inherit bold))
  "Face for cited passage text in the songs notes pane."
  :group 'arxana-browser-songs)

(defconst arxana-browser-songs-base-overlay-priority 10
  "Priority for passive song annotation overlays.")

(defconst arxana-browser-songs-active-overlay-priority 20
  "Priority for active song annotation overlays.")

(defun arxana-browser-songs--catalog-spec (id-or-view)
  (seq-find
   (lambda (spec)
     (or (eq (plist-get spec :id) id-or-view)
         (eq (plist-get spec :view) id-or-view)))
   arxana-browser-songs-catalogs))

(defun arxana-browser-songs--entity-name (entity)
  (or (alist-get :entity/name entity)
      (alist-get :name entity)
      (alist-get :entity/id entity)
      (alist-get :id entity)
      "<untitled>"))

(defun arxana-browser-songs--entity-id (entity)
  (or (alist-get :entity/id entity)
      (alist-get :xt/id entity)
      (alist-get :entity/external-id entity)
      (alist-get :id entity)))

(defun arxana-browser-songs--entity-type (entity)
  (or (alist-get :entity/type entity)
      (alist-get :type entity)))

(defun arxana-browser-songs--entity-source (entity)
  (or (alist-get :entity/source entity)
      (alist-get :source entity)
      ""))

(defun arxana-browser-songs--entity-lines (entity)
  (let* ((text (string-trim-right (or (arxana-browser-songs--entity-source entity) ""))))
    (if (string-empty-p text)
        0
      (length (split-string text "\n" nil)))))

(defun arxana-browser-songs--entity-preview (entity)
  (let* ((text (string-trim (or (arxana-browser-songs--entity-source entity) ""))))
    (if (string-empty-p text)
        "No text stored yet."
      (truncate-string-to-width
       (replace-regexp-in-string "[\n\r\t ]+" " " text)
       90 nil nil t))))

(defun arxana-browser-songs--unwrap-entities (response)
  (let ((entities (and (listp response)
                       (or (ignore-errors (alist-get :entities response))
                           (ignore-errors (alist-get :items response))
                           response))))
    (cond
     ((null entities) nil)
     ((vectorp entities) (append entities nil))
     ((and (listp entities)
           (listp (car entities))
           (ignore-errors (alist-get :entity (car entities))))
      (mapcar (lambda (item) (or (alist-get :entity item) item))
              entities))
     ((and (listp entities)
           (cl-every (lambda (item)
                       (and (listp item)
                            (or (ignore-errors (arxana-browser-songs--entity-id item))
                                (ignore-errors (alist-get :entity item)))))
                     entities))
      entities)
     (t nil))))

(defun arxana-browser-songs--slugify (text)
  (let* ((slug (downcase (or text ""))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (setq slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug))
    (if (string-empty-p slug) "untitled" slug)))

(defun arxana-browser-songs--clean-inline-tex (text)
  (let ((value (or text "")))
    (setq value (replace-regexp-in-string "\\\\label{[^}]*}" "" value))
    (setq value (replace-regexp-in-string "\\\\footnote{[^}]*}" "" value))
    (while (string-match "\\\\[A-Za-z@]+\\(?:\\[[^]]*\\]\\)?{\\([^{}]*\\)}" value)
      (setq value (replace-match "\\1" t nil value)))
    (setq value (replace-regexp-in-string "\\\\[A-Za-z@]+\\(?:\\[[^]]*\\]\\)?" "" value))
    (setq value (replace-regexp-in-string "[{}]" "" value))
    (string-trim value)))

(defun arxana-browser-songs--section-title-from-line (line)
  (when (string-match "^\\\\section\\(?:\\[\\([^]]+\\)\\]\\)?{\\(.+?\\)}\\(?:\\\\label{[^}]*}\\)?$" line)
    (let ((short (match-string 1 line))
          (long (match-string 2 line)))
      (arxana-browser-songs--clean-inline-tex (or short long)))))

(defun arxana-browser-songs--movement-title-from-line (line)
  (when (string-match "^\\\\section\\*{+.*\\\\book\\s-+\\([^}]+\\)}+\\s-*$" line)
    (string-trim (match-string 1 line))))

(defun arxana-browser-songs--preferred-section-body (body)
  (cond
   ((string-match "\\\\begin{itpar}\\([[:ascii:][:nonascii:]\n\r]*?\\)\\\\end{itpar}" body)
    (match-string 1 body))
   ((string-match "\\\\begin{song}[^}]*}\n?\\([[:ascii:][:nonascii:]\n\r]*?\\)\\\\end{song}" body)
    (match-string 1 body))
   (t body)))

(defun arxana-browser-songs--strip-leading-comment (line)
  (if (string-match-p "^\\s-*%" line)
      ""
    line))

(defun arxana-browser-songs--filter-lines (lines)
  (let (result)
    (dolist (line lines (nreverse result))
      (let ((s (string-trim line)))
        (when (and (not (string-empty-p s))
                   (not (string-match-p "^\\[[^]]+\\]$" s))
                   (not (arxana-browser-songs--chord-line-p s))
                   (not (string-match-p "\\`[[:punct:]•·]+\\'" s)))
          (push s result))))))

(defun arxana-browser-songs--chord-token-p (token)
  (or (member token '("~" "..." "•"))
      (string-match-p
       "\\`[A-G][#♯♭b]?\\(?:maj\\|min\\|sus\\|dim\\|aug\\|add\\|m\\|o\\)?[0-9#♯♭b/+()A-Ga-g-]*\\'"
       token)))

(defun arxana-browser-songs--chord-line-p (line)
  (let ((tokens (split-string (replace-regexp-in-string "[,;]" " " line) "[[:space:]]+" t)))
    (and tokens
         (cl-every #'arxana-browser-songs--chord-token-p tokens))))

(defun arxana-browser-songs--clean-tex-text (text)
  (let ((value (or text "")))
    (setq value
          (mapconcat #'arxana-browser-songs--strip-leading-comment
                     (split-string value "\n")
                     "\n"))
    (setq value (replace-regexp-in-string "\\\\clearpage\\|\\\\newpage\\|\\\\medskip\\|\\\\smallskip\\|\\\\bigskip" "\n" value))
    (setq value (replace-regexp-in-string "\\\\\\\\" "\n" value))
    (setq value (replace-regexp-in-string "\\\\hspace{[^}]*}" "" value))
    (setq value (replace-regexp-in-string "\\\\vspace{[^}]*}" "" value))
    (setq value (replace-regexp-in-string "\\\\label{[^}]*}" "" value))
    (setq value (replace-regexp-in-string "\\\\ldots\\(?:{}\\)?" "..." value))
    (setq value (replace-regexp-in-string "\\\\\\([#$%&_{}]\\)" "\\1" value))
    (setq value (replace-regexp-in-string "\\\\," "" value))
    (setq value (replace-regexp-in-string "\\\\/" "" value))
    (setq value (replace-regexp-in-string "\\\\begin{[^}]+}\\|\\\\end{[^}]+}" "\n" value))
    (setq value (replace-regexp-in-string "\\\\footnote{[^}]*}" "" value))
    (while (string-match "\\\\[A-Za-z@]+\\(?:\\[[^]]*\\]\\)?{\\([^{}]*\\)}" value)
      (setq value (replace-match "\\1" t nil value)))
    (setq value (replace-regexp-in-string "\\\\[A-Za-z@]+\\(?:\\[[^]]*\\]\\)?" "" value))
    (setq value (replace-regexp-in-string "\\^[{][^}]*}" "" value))
    (setq value (replace-regexp-in-string "\\\\[()\\[\\]]" "" value))
    (setq value (replace-regexp-in-string "[ \t]+" " " value))
    (mapconcat #'identity
               (arxana-browser-songs--filter-lines (split-string value "\n"))
               "\n")))

(defun arxana-browser-songs-parse-suite-file (&optional file)
  "Parse FILE into song plists suitable for XTDB import."
  (let ((file (expand-file-name (or file arxana-browser-songs-suite-default-file))))
    (unless (file-readable-p file)
      (user-error "Suite file is not readable: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (split-string (buffer-string) "\n"))
            (movement nil)
            (current nil)
            (after-appendix nil)
            songs)
        (dolist (line lines)
          (unless after-appendix
            (let ((trimmed (string-trim line)))
              (cond
               ((string-prefix-p "\\appendix" trimmed)
                (when current
                  (push current songs)
                  (setq current nil))
                (setq after-appendix t))
               ((arxana-browser-songs--movement-title-from-line trimmed)
                (setq movement (arxana-browser-songs--movement-title-from-line trimmed)))
               ((and (string-prefix-p "\\section*" trimmed)
                     (not (arxana-browser-songs--movement-title-from-line trimmed)))
                nil)
               ((arxana-browser-songs--section-title-from-line trimmed)
                (when current
                  (push current songs))
                (setq current (list :title (arxana-browser-songs--section-title-from-line trimmed)
                                    :movement movement
                                    :lines nil)))
               (current
                (setf (plist-get current :lines)
                      (append (plist-get current :lines) (list line))))))))
        (when current
          (push current songs))
        (setq songs (nreverse songs))
        (delq nil
              (mapcar
               (lambda (song)
                 (let* ((body (mapconcat #'identity (plist-get song :lines) "\n"))
                        (preferred (arxana-browser-songs--preferred-section-body body))
                        (source (arxana-browser-songs--clean-tex-text preferred))
                        (title (plist-get song :title))
                        (movement-title (plist-get song :movement))
                        (id (format "arxana/song/nightmarish-suite/%s/%s"
                                    (arxana-browser-songs--slugify (or movement-title "suite"))
                                    (arxana-browser-songs--slugify title))))
                   (when (and (stringp title) (not (string-empty-p title))
                              (stringp source) (not (string-empty-p source)))
                     (list :id id
                           :external-id id
                           :name title
                           :type "arxana/song"
                           :movement movement-title
                           :source-file file
                           :source source))))
               songs))))))

(defun arxana-browser-songs-import-suite-file (&optional file dry-run)
  "Import FILE into XTDB as `arxana/song' entities.
When DRY-RUN is non-nil, return parsed entities without writing them."
  (interactive
   (list (read-file-name "Suite file: "
                         (file-name-directory arxana-browser-songs-suite-default-file)
                         arxana-browser-songs-suite-default-file
                         t)
         current-prefix-arg))
  (let ((songs (arxana-browser-songs-parse-suite-file file))
        imported)
    (unless dry-run
      (unless (arxana-store-ensure-sync)
        (user-error "Futon sync is disabled; enable futon4-enable-sync first")))
    (dolist (song songs)
      (if dry-run
          (push song imported)
        (let ((response (arxana-store-ensure-entity
                         :id (plist-get song :id)
                         :external-id (plist-get song :external-id)
                         :name (plist-get song :name)
                         :type (plist-get song :type)
                         :source (plist-get song :source))))
          (push (list :name (plist-get song :name)
                      :id (plist-get song :id)
                      :response response)
                imported))))
    (setq imported (nreverse imported))
    (when (called-interactively-p 'interactive)
      (message "%s %d suite song%s"
               (if dry-run "Parsed" "Imported")
               (length imported)
               (if (= (length imported) 1) "" "s")))
    imported))

(defun arxana-browser-songs--demo-support
    (id song-id song-passage source-id source-passage note)
  (list :id id
        :hx-type "annotation/supports"
        :endpoints `(((:role . "annotated")
                      (:entity-id . ,song-id)
                      (:passage . ,song-passage))
                     ((:role . "source")
                      (:entity-id . ,source-id)
                      (:passage . ,source-passage)))
        :props `((note . ,note)
                 (demo . "t"))
        :labels '("annotation/demo" "annotation/positive")))

(defun arxana-browser-songs--demo-open-question
    (id song-id song-passage question)
  (list :id id
        :hx-type "annotation/open-question"
        :endpoints `(((:role . "annotated")
                      (:entity-id . ,song-id)
                      (:passage . ,song-passage)))
        :props `((question . ,question)
                 (demo . "t"))
        :labels '("annotation/demo" "annotation/open-question")))

(defconst arxana-browser-songs-demo-annotations
  (list
   (arxana-browser-songs--demo-support
    "hx:demo:celebration-city"
    "arxana/song/nightmarish-suite/a-house/celebration-of-the-lizard"
    "lines 1-3: Lions in the street and roaming"
    "arxana/media-lyrics/misc/2a5e36a8319aa6c03e22a0348f8952b5034419f3ddbcbd60d20d12d8498c6fa4"
    "Generally alone and the stars shone brighter, except in the city, the city, the city the red glow takes over"
    "The city as site of wildness. Morrison's lions and the chapbook's red glow are the same feral urbanism.")
   (arxana-browser-songs--demo-support
    "hx:demo:celebration-mother"
    "arxana/song/nightmarish-suite/a-house/celebration-of-the-lizard"
    "lines 4-6: The body of his mother / Rotting in the summer ground."
    "arxana/media-lyrics/misc/2e813e73bb2887023df2da8d95139cdf3658917a48f2a65a24edebd22dbf9a2e"
    "This is my mother slogging her way through snowstorms and Haldol to bring food home from the store"
    "Both begin from the mother's body. The prologue's dead mother and the chapbook's living one are mirrors.")
   (arxana-browser-songs--demo-support
    "hx:demo:celebration-leave"
    "arxana/song/nightmarish-suite/a-house/celebration-of-the-lizard"
    "line 7: He went down South"
    "arxana/media-lyrics/misc/1888e96053f790646d400fda9fd7579f340ca29180bcd3cfd2620a94fa16d19b"
    "wanting to leave"
    "The simplest warrant: wanting to leave. Both voices share a departure that is not yet an arrival.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:celebration-snake-question"
    "arxana/song/nightmarish-suite/a-house/celebration-of-the-lizard"
    "line 20: The snake was pale gold glazed and shrunken."
    "Why is the snake diminished here rather than triumphant? The chapbook has reptilian energy in abundance, but not yet this image of a spent or desiccated emblem.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:celebration-silence-question"
    "arxana/song/nightmarish-suite/a-house/celebration-of-the-lizard"
    "line 108: Listening for a fistful of silence"
    "What can actually be gathered or held from silence? This feels like an ars poetica for the whole suite, and the chapbook could still answer it more directly.")
   (arxana-browser-songs--demo-support
    "hx:demo:salty-moonrise"
    "arxana/song/nightmarish-suite/the-sea/salty-moon"
    "line 1: When the sky throws a blanket over you"
    "arxana/media-lyrics/misc/082c67e8bbe29ab46eef2fa1522a58cf2f1f8721e8d66205cb020a509dca5a50"
    "The moon rising over the hill"
    "Both open with sky descending onto a figure below. The blanket and the moonrise are the same sheltering or smothering gesture.")
   (arxana-browser-songs--demo-support
    "hx:demo:salty-salt"
    "arxana/song/nightmarish-suite/the-sea/salty-moon"
    "line 2: We'll cry salt; these moments are gone"
    "arxana/media-lyrics/misc/7fc71c499ac2b857959d23a802c3513820f6a32127f66b0cfebba3359c7e5532"
    "On a quiet morning, as I went to record these words a glass canister of salt fell from the cupboard and smashed on the floor."
    "The salt spills involuntarily. Neither the tears nor the canister were planned.")
   (arxana-browser-songs--demo-support
    "hx:demo:salty-skin"
    "arxana/song/nightmarish-suite/the-sea/salty-moon"
    "line 7: And the moon is salty sometimes"
    "arxana/media-lyrics/misc/69d3fc8c7afdf9ac9e80e9a5b5ae081ab16ef04d881ca39d6e8ad863611f7a32"
    "I felt very tight in my own skin"
    "The salty moon and the tight skin both name the body as a container for something that stings from within.")
   (arxana-browser-songs--demo-support
    "hx:demo:salty-inhabited-skin"
    "arxana/song/nightmarish-suite/the-sea/salty-moon"
    "line 8: Isn't your skin eating you alive?"
    "arxana/media-lyrics/misc/ad03aefe4fe6010941863e227716e707e6a8506ea974024052ec4cdabf978b0a"
    "think about the fact that there are other things living in your skin"
    "Direct echo: skin as inhabited, consuming, and not fully one's own.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:salty-secrets-question"
    "arxana/song/nightmarish-suite/the-sea/salty-moon"
    "line 3: Your forehead is a valley of secrets"
    "What are the secrets stored in the body here? The chapbook gives salt, skin, and weather, but not yet a facial landscape or a secret history written on it.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:salty-hell-question"
    "arxana/song/nightmarish-suite/the-sea/salty-moon"
    "line 19: And her eyes flow with light it has to be like hell"
    "Who is the her in this moment, and why does radiance become infernal? This could open a new passage on beauty that overwhelms rather than consoles.")
   (arxana-browser-songs--demo-support
    "hx:demo:memoire-foreign-water"
    "arxana/song/nightmarish-suite/the-sea/la-mémoire-et-la-mer"
    "lines 1-4: La marée je l'ai dans le coeur"
    "arxana/media-lyrics/misc/ad03aefe4fe6010941863e227716e707e6a8506ea974024052ec4cdabf978b0a"
    "Like the cold water that flowed in the creek I only listened to the ways it sounded when it was speaking in a foreign language, this is when I was a kid"
    "The tide rising like a sign and the creek speaking in a foreign language both treat water as a voice one can feel but not fully understand.")
   (arxana-browser-songs--demo-support
    "hx:demo:memoire-ghost"
    "arxana/song/nightmarish-suite/the-sea/la-mémoire-et-la-mer"
    "lines 9-12: Je suis le fantôme Jersey"
    "arxana/media-lyrics/misc/9612c8d8529c786baa1837acde228b9ab295138058c22401288fc7d77980c8fa"
    "All these old ghosts gathered around telling me they don't like what I'm doing"
    "Ferré names himself a ghost; the chapbook's old ghosts gather and complain. Ghosts recognizing each other.")
   (arxana-browser-songs--demo-support
    "hx:demo:memoire-desert"
    "arxana/song/nightmarish-suite/the-sea/la-mémoire-et-la-mer"
    "lines 17-20: Rappelle-toi ce chien de mer"
    "arxana/media-lyrics/misc/daee033bbc5f267e69dcabf3e8ec2217a71a339eee3ef16fba4b78c08fa67c2b"
    "Across the desert vista"
    "Ferré's desert of seaweed necropolis meets the chapbook's desert vista. Both are landscapes of the dead.")
   (arxana-browser-songs--demo-support
    "hx:demo:memoire-horses"
    "arxana/song/nightmarish-suite/the-sea/la-mémoire-et-la-mer"
    "lines 27-28: Cette bave des chevaux ras"
    "arxana/media-lyrics/misc/36b2ec0749d7640cfbf9311ca1a13ee6f803f2a6f08ee097a054d0560c15da84"
    "in the morning there are horses chomping and stamping on the beach"
    "Ferré's horses and the chapbook's beach horses are the same animals at the edge of land and sea.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:memoire-jersey-question"
    "arxana/song/nightmarish-suite/the-sea/la-mémoire-et-la-mer"
    "line 9: Je suis le fantôme Jersey"
    "Why Jersey in particular, and what kind of ghost is local enough to name a place? The chapbook has old ghosts, but not yet a geography of haunting tied to a coast or port.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:memoire-blue-math-question"
    "arxana/song/nightmarish-suite/the-sea/la-mémoire-et-la-mer"
    "line 61: Une mathématique bleue"
    "What would a blue mathematics actually count or measure? This image feels central to the suite's sea-logic but still largely undescribed in prose.")
   (arxana-browser-songs--demo-support
    "hx:demo:oldman-dying"
    "arxana/song/nightmarish-suite/the-sea/old-man"
    "lines 1-4: Old Man, this hole at your knees"
    "arxana/media-lyrics/misc/9612c8d8529c786baa1837acde228b9ab295138058c22401288fc7d77980c8fa"
    "who goes to talk to an old man who's probably dying"
    "The chapbook asks the question this aria stages outright. The old man is addressed directly, but too late, with pity rather than remedy.")
   (arxana-browser-songs--demo-support
    "hx:demo:oldman-door"
    "arxana/song/nightmarish-suite/the-sea/old-man"
    "lines 5-9: Your love, one day walked through the door"
    "arxana/media-lyrics/misc/098cbe949f6e87b9850d88a45d115ee372e9b261acf37a59135310407e713edf"
    "the second stroke, walked out the door"
    "The love who walks through the door is already crossing into absence. The door is a threshold into burial, not domestic return.")
   (arxana-browser-songs--demo-support
    "hx:demo:oldman-aging"
    "arxana/song/nightmarish-suite/the-sea/old-man"
    "lines 10-13: Old man, there's not much on your clock"
    "arxana/media-lyrics/misc/69d3fc8c7afdf9ac9e80e9a5b5ae081ab16ef04d881ca39d6e8ad863611f7a32"
    "a chill wind nipping at your cheeks or at your toes this is what it must be like to grow old"
    "The lock, the clock, and the drowned voice all belong to the same apprehension of age as exposure, dwindling time, and helpless witness.")
   (arxana-browser-songs--demo-support
    "hx:demo:oldman-castaway"
    "arxana/song/nightmarish-suite/the-sea/old-man"
    "lines 14-17: She now is resting forever in her ocean dress"
    "arxana/media-lyrics/misc/20a7a52932b74b3ffd30e43ea967d1ec9afcc2a5f5bcb42fb43d23b24b35caf1"
    "I feel like a castaway washed up on the shore"
    "The daughter goes to the shore and returns only as wreckage. The ocean dress makes her into the castaway the chapbook only imagines.")
   (arxana-browser-songs--demo-support
    "hx:demo:oldman-ghosts"
    "arxana/song/nightmarish-suite/the-sea/old-man"
    "lines 20-21: Old man, the ghosts come when you pray"
    "arxana/media-lyrics/misc/9612c8d8529c786baa1837acde228b9ab295138058c22401288fc7d77980c8fa"
    "All these old ghosts gathered around telling me they don't like what I'm doing"
    "By the end the ghosts are no longer metaphorical. Prayer summons visitation, complaint, and judgement.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:oldman-mimes-question"
    "arxana/song/nightmarish-suite/the-sea/old-man"
    "line 4: May the sound of mimes end your tragedy"
    "What kind of silence or failed performance is this? The aria introduces mimes as if muteness itself could intervene, and the chapbook does not yet answer that.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:oldman-ocean-dress-question"
    "arxana/song/nightmarish-suite/the-sea/old-man"
    "line 17: She now is resting forever in her ocean dress"
    "Is the ocean dress literal burial imagery, a mythic costume, or a way of making shipwreck intimate? A new chapbook fragment could decide how the sea dresses the dead.")
   (arxana-browser-songs--demo-support
    "hx:demo:night-mother"
    "arxana/song/nightmarish-suite/the-sea/night-is-beautiful"
    "lines 1-4: Dear Mother, I dreamed of you again"
    "arxana/media-lyrics/misc/2e813e73bb2887023df2da8d95139cdf3658917a48f2a65a24edebd22dbf9a2e"
    "This is my mother slogging her way through snowstorms and Haldol to bring food home from the store"
    "Both texts address the mother through ordeal rather than sentiment. The dream is full of labor, danger, and belated tenderness.")
   (arxana-browser-songs--demo-support
    "hx:demo:night-blue"
    "arxana/song/nightmarish-suite/the-sea/night-is-beautiful"
    "lines 5-10: And the night is beautiful"
    "arxana/media-lyrics/misc/0e3f436ebd52c6c024c4d656827d243d93f7a6ccb0dabf3d17df16e351b5dd43"
    "so little I can do as evening really settles in for the night and the city turns blue"
    "Here too the night is not peaceful so much as saturated. The song insists on beauty at the point where the chapbook stresses helplessness.")
   (arxana-browser-songs--demo-support
    "hx:demo:night-door"
    "arxana/song/nightmarish-suite/the-sea/night-is-beautiful"
    "lines 11-14: Dear Mother, when you open the door"
    "arxana/media-lyrics/misc/afda6877f813f59c40c20513dd9374c107b5760e1bf14a271e7c34e6e3ce8222"
    "He left the door propped open. You see here in the building there is a private economy based on words"
    "The open door matters because speech fails on the threshold. The plea for forgiveness and the plea for a lie both come from a speaker who cannot cross cleanly into truth.")
   (arxana-browser-songs--demo-support
    "hx:demo:night-mother-tongue"
    "arxana/song/nightmarish-suite/the-sea/night-is-beautiful"
    "lines 21-24: Of your love; of your story"
    "arxana/media-lyrics/misc/640510fdef89a816b30b398230e6dfa078d1e0e552443013f252d5400a7161af"
    "our mothers who teach us to speak; our fathers who teach us to write"
    "The song ends by collapsing mother, story, and sea-colour into one origin. The chapbook's mother tongue becomes literal again.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:night-danger-question"
    "arxana/song/nightmarish-suite/the-sea/night-is-beautiful"
    "line 3: Your empty mouth, the danger in your head"
    "What exactly is the danger here: illness, language, inheritance, or memory? The mother figure is vivid elsewhere, but this specific threat remains productively undefined.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:night-lie-question"
    "arxana/song/nightmarish-suite/the-sea/night-is-beautiful"
    "line 13: Give me a lie, oh to swallow them all"
    "Why are lies imagined as something ingestible? This could lead to a new prose passage on feeding, medicine, or consolation as forms of untruth.")
   (arxana-browser-songs--demo-support
    "hx:demo:abi-blue"
    "arxana/song/nightmarish-suite/the-sea/abi"
    "lines 1-4: Blue carnations, in the soil"
    "arxana/media-lyrics/misc/a613ca431dd0beebfda44cf65172a80bfee2d492c167d521fb405b27f12a6705"
    "So go to the orange of purple or the blue of green, the blue of red"
    "Blue is treated as somewhere one can travel inside. The chapbook's sliding colours and the song's flowers and moonlight both dissolve fixed borders.")
   (arxana-browser-songs--demo-support
    "hx:demo:abi-nocturne"
    "arxana/song/nightmarish-suite/the-sea/abi"
    "lines 5-8: The mist is wrapped around your shoulders"
    "arxana/media-lyrics/misc/082c67e8bbe29ab46eef2fa1522a58cf2f1f8721e8d66205cb020a509dca5a50"
    "The moon rising over the hill"
    "Mist, stars, shoulders, and hair turn the stanza into a nocturne. The chapbook's moonrise supplies the visual field for this embrace.")
   (arxana-browser-songs--demo-support
    "hx:demo:abi-butterfly"
    "arxana/song/nightmarish-suite/the-sea/abi"
    "line 7: Heavy, the sleep of butterflies"
    "arxana/media-lyrics/misc/a613ca431dd0beebfda44cf65172a80bfee2d492c167d521fb405b27f12a6705"
    "the butterfly craves release to the open sky"
    "Against the song's heavy sleeping butterflies, the chapbook offers the contrary impulse: metamorphosis as pressure toward release.")
   (arxana-browser-songs--demo-support
    "hx:demo:abi-shore"
    "arxana/song/nightmarish-suite/the-sea/abi"
    "lines 9-12: Blue, blue, the waves of the beach"
    "arxana/media-lyrics/misc/20a7a52932b74b3ffd30e43ea967d1ec9afcc2a5f5bcb42fb43d23b24b35caf1"
    "I feel like a castaway washed up on the shore"
    "The beach and the request to be held make this a shore-song. Arrival, rescue, and desire become the same gesture.")
   (arxana-browser-songs--demo-support
    "hx:demo:abi-wind"
    "arxana/song/nightmarish-suite/the-sea/abi"
    "lines 13-14: Tomorrow when the sun shines, the light"
    "arxana/media-lyrics/misc/ad03aefe4fe6010941863e227716e707e6a8506ea974024052ec4cdabf978b0a"
    "And a reed is bent by the wind"
    "Tomorrow's light is paired with wind already moving through the scene. The journey begins not with decision but with weather.")
   (arxana-browser-songs--demo-support
    "hx:demo:abi-memory"
    "arxana/song/nightmarish-suite/the-sea/abi"
    "lines 15-18: One day the sea will carry us away from our memories"
    "arxana/media-lyrics/misc/098cbe949f6e87b9850d88a45d115ee372e9b261acf37a59135310407e713edf"
    "a roving fluid without repurcussions, without any memory"
    "The sea gets the last word: water as the force that strips memory away. The chapbook states the logic abstractly; the song makes it tender.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:abi-heavy-question"
    "arxana/song/nightmarish-suite/the-sea/abi"
    "line 7: Heavy, the sleep of butterflies"
    "Why are butterflies heavy here when they usually signify lightness or release? The image suggests a burdened metamorphosis the chapbook has not yet pursued.")
   (arxana-browser-songs--demo-open-question
    "hx:demo:abi-memory-question"
    "arxana/song/nightmarish-suite/the-sea/abi"
    "line 16: One day the sea will carry us away from our memories"
    "Is this erasure a promise, a threat, or a mercy? The chapbook repeatedly circles memory, but not yet the wish to be taken beyond it altogether."))
  "Demo annotation links imported from the two-column mockup.")

(defun arxana-browser-songs-import-demo-annotations (&optional dry-run)
  "Import demo song annotations into XTDB.
When DRY-RUN is non-nil, return the payload specs without writing them."
  (interactive "P")
  (unless dry-run
    (unless (arxana-store-ensure-sync)
      (user-error "Futon sync is disabled; enable futon4-enable-sync first")))
  (let (imported)
    (dolist (spec arxana-browser-songs-demo-annotations)
      (if dry-run
          (push spec imported)
        (push (arxana-store-create-hyperedge
               :id (plist-get spec :id)
               :type "arxana/annotation"
               :hx-type (plist-get spec :hx-type)
               :endpoints (plist-get spec :endpoints)
               :props (plist-get spec :props)
               :labels (plist-get spec :labels))
              imported)))
    (setq imported (nreverse imported))
    (when (called-interactively-p 'interactive)
      (message "%s %d demo annotation%s"
               (if dry-run "Prepared" "Imported")
               (length imported)
               (if (= (length imported) 1) "" "s")))
    imported))

(defun arxana-browser-songs--catalog-entities (spec)
  (let ((seen (make-hash-table :test 'equal))
        items)
    (dolist (entity-type (plist-get spec :entity-types))
      (let* ((response (and (arxana-store-sync-enabled-p)
                            (arxana-store-fetch-entities-latest
                             :type entity-type
                             :limit arxana-browser-songs-latest-limit)))
             (entities (arxana-browser-songs--unwrap-entities response)))
        (dolist (entity entities)
          (let ((entity-id (arxana-browser-songs--entity-id entity)))
            (when (and entity-id (not (gethash entity-id seen)))
              (puthash entity-id t seen)
              (push entity items))))))
    (seq-sort-by #'arxana-browser-songs--entity-name #'string-lessp items)))

(defun arxana-browser-songs-menu-items ()
  (mapcar
   (lambda (spec)
     (let* ((entities (arxana-browser-songs--catalog-entities spec))
            (count (length entities))
            (summary (if (arxana-store-sync-enabled-p)
                         (format "%d entr%s. %s"
                                 count
                                 (if (= count 1) "y" "ies")
                                 (plist-get spec :description))
                       (format "Sync disabled. %s" (plist-get spec :description)))))
       (list :type 'songs-catalog
             :label (plist-get spec :label)
             :description summary
             :view (plist-get spec :view)
             :catalog-id (plist-get spec :id)
             :count count)))
   arxana-browser-songs-catalogs))

(defun arxana-browser-songs-items (context)
  (let* ((spec (arxana-browser-songs--catalog-spec (plist-get context :view)))
         (entities (and spec (arxana-browser-songs--catalog-entities spec))))
    (cond
     ((not (arxana-store-sync-enabled-p))
      (list (list :type 'info
                  :label "Songs browser unavailable"
                  :description "Enable Futon sync to browse XTDB-backed songs and lyrics.")))
     ((not spec)
      (list (list :type 'info
                  :label "Unknown songs catalog"
                  :description "No songs catalog is associated with this view.")))
     ((null entities)
      (list (list :type 'info
                  :label "No entries yet"
                  :description (format "No %s entities are stored in XTDB yet."
                                       (downcase (plist-get spec :label))))))
     (t
      (mapcar
       (lambda (entity)
         (list :type 'song-entity
               :label (arxana-browser-songs--entity-name entity)
               :description (arxana-browser-songs--entity-preview entity)
               :entity-id (arxana-browser-songs--entity-id entity)
               :entity-type (arxana-browser-songs--entity-type entity)
               :line-count (arxana-browser-songs--entity-lines entity)
               :catalog-id (plist-get spec :id)
               :entity entity))
       entities)))))

(defun arxana-browser-songs--flatten-section-items (sections)
  (apply #'append (mapcar (lambda (section) (plist-get section :items)) sections)))

(defun arxana-browser-songs--entry-passage (entry)
  (or (plist-get entry :current-passage)
      (plist-get entry :passage)
      ""))

(defun arxana-browser-songs--entry-sort-key (entry)
  (let* ((passage (arxana-browser-songs--entry-passage entry))
         (line-bounds (arxana-browser-songs--line-bounds-from-passage passage))
         (start (or (car-safe line-bounds) most-positive-fixnum))
         (end (or (cdr-safe line-bounds) most-positive-fixnum)))
    (list end start (downcase (string-trim passage)))))

(defun arxana-browser-songs--entry-sort-lessp (left right)
  (let ((left-key (arxana-browser-songs--entry-sort-key left))
        (right-key (arxana-browser-songs--entry-sort-key right)))
    (or (< (nth 0 left-key) (nth 0 right-key))
        (and (= (nth 0 left-key) (nth 0 right-key))
             (or (< (nth 1 left-key) (nth 1 right-key))
                 (and (= (nth 1 left-key) (nth 1 right-key))
                      (string-lessp (nth 2 left-key) (nth 2 right-key))))))))

(defun arxana-browser-songs--sort-sections (sections)
  (mapcar
   (lambda (section)
     (list :title (plist-get section :title)
           :items (sort (copy-sequence (plist-get section :items))
                        #'arxana-browser-songs--entry-sort-lessp)))
   sections))

(defun arxana-browser-songs--label-sections (sections)
  (let ((counter 0))
    (mapcar
     (lambda (section)
       (let ((items
              (mapcar (lambda (entry)
                        (setq counter (1+ counter))
                        (plist-put (copy-sequence entry) :marker-label
                                   (format "[%d]" counter)))
                      (plist-get section :items))))
         (list :title (plist-get section :title)
               :items items)))
     sections)))

(defun arxana-browser-songs-format ()
  [("Title" 36 t)
   ("Type" 22 t)
   ("Lines" 7 nil)
   ("Entity" 42 t)])

(defun arxana-browser-songs-row (item)
  (vector (or (plist-get item :label) "")
          (format "%s" (or (plist-get item :entity-type) ""))
          (number-to-string (or (plist-get item :line-count) 0))
          (or (plist-get item :entity-id) "")))

(defvar-local arxana-browser-songs--line-map nil)
(defvar-local arxana-browser-songs--source-index nil)
(defvar-local arxana-browser-songs--note-index nil)
(defvar-local arxana-browser-songs--active-overlay nil)
(defvar-local arxana-browser-songs--note-highlight-overlay nil)
(defvar-local arxana-browser-songs--entry-overlays nil)
(defvar-local arxana-browser-songs--last-active-hyperedge nil)
(defvar-local arxana-browser-songs--highlight-scope 'all)
(defvar-local arxana-browser-songs--peer-buffer nil)
(defvar-local arxana-browser-songs--current-entity-id nil)
(defvar-local arxana-browser-songs--return-item nil)

(defvar arxana-browser-songs-text-link-map nil
  "Keymap for clickable annotation markers inside song text.")
(setq arxana-browser-songs-text-link-map (make-sparse-keymap))
(define-key arxana-browser-songs-text-link-map [mouse-1] #'arxana-browser-songs-text-activate)
(define-key arxana-browser-songs-text-link-map (kbd "RET") #'arxana-browser-songs-text-activate)

(defvar arxana-browser-songs-text-mode-map nil
  "Keymap for `arxana-browser-songs-text-mode'.")
(setq arxana-browser-songs-text-mode-map (make-sparse-keymap))
(set-keymap-parent arxana-browser-songs-text-mode-map view-mode-map)
(define-key arxana-browser-songs-text-mode-map (kbd "<left>") #'arxana-browser-songs-left-or-return)
(define-key arxana-browser-songs-text-mode-map (kbd "RET") #'arxana-browser-songs-text-activate)
(define-key arxana-browser-songs-text-mode-map (kbd "h") #'arxana-browser-songs-toggle-highlight-scope)

(define-minor-mode arxana-browser-songs-text-mode
  "Minor mode for the songs text buffer."
  :lighter " Song-Text"
  :keymap arxana-browser-songs-text-mode-map
  :group 'arxana-browser-songs
  (if arxana-browser-songs-text-mode
      (add-hook 'post-command-hook #'arxana-browser-songs--sync-note-from-point nil t)
    (remove-hook 'post-command-hook #'arxana-browser-songs--sync-note-from-point t)))

(defun arxana-browser-songs--scope-label (scope)
  (pcase scope
    ('point "Current")
    (_ "All")))

(defun arxana-browser-songs--text-header-line ()
  (format "Songs  RET/mouse-1 follow annotation  h toggle highlight [%s]  point syncs notes"
          (arxana-browser-songs--scope-label arxana-browser-songs--highlight-scope)))

(defun arxana-browser-songs--notes-header-line ()
  (let* ((text-buf (and (buffer-live-p arxana-browser-songs--peer-buffer)
                        arxana-browser-songs--peer-buffer))
         (scope (if text-buf
                    (buffer-local-value 'arxana-browser-songs--highlight-scope text-buf)
                  arxana-browser-songs--highlight-scope)))
    (format "Song Notes  RET/mouse-1 follow link  h toggle highlight [%s]  point syncs source"
            (arxana-browser-songs--scope-label scope))))

(defun arxana-browser-songs--refresh-header-lines ()
  (dolist (buf (delq nil (list (current-buffer)
                               (and (buffer-live-p arxana-browser-songs--peer-buffer)
                                    arxana-browser-songs--peer-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (force-mode-line-update t)))))

(defun arxana-browser-songs-left-or-return ()
  "Move left or return to the prior songs context when at point-min."
  (interactive)
  (if (> (point) (point-min))
      (backward-char)
    (if arxana-browser-songs--return-item
        (arxana-browser-songs-open arxana-browser-songs--return-item)
      (arxana-ui-left-or-return))))

(defun arxana-browser-songs--note-source-passage-at-point ()
  (or (get-text-property (point) 'arxana-song-source-passage)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'arxana-song-source-passage))))

(defun arxana-browser-songs--make-navigation-item (buffer focus-passage)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when arxana-browser-songs--current-entity-id
        (list :entity-id arxana-browser-songs--current-entity-id
              :focus-passage focus-passage
              :return-buffer arxana-ui-return-buffer
              :return-window-config arxana-ui-return-window-config
              :return-item arxana-browser-songs--return-item)))))

(defun arxana-browser-songs--value-name (value)
  (cond
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   ((null value) nil)
   (t (format "%s" value))))

(defun arxana-browser-songs--alist-value (alist &rest keys)
  (seq-some (lambda (key)
              (and (listp alist)
                   (ignore-errors (alist-get key alist))))
            keys))

(defun arxana-browser-songs--unwrap-hyperedges (response)
  (let ((items (and (listp response)
                    (or (ignore-errors (alist-get :hyperedges response))
                        response))))
    (cond
     ((null items) nil)
     ((vectorp items) (append items nil))
     ((listp items) items)
     (t nil))))

(defun arxana-browser-songs--line-bounds-from-passage (passage)
  (cond
   ((and (stringp passage)
         (string-match "\\`lines? \\([0-9]+\\)-\\([0-9]+\\)\\(?::\\|\\'\\)" passage))
    (cons (string-to-number (match-string 1 passage))
          (string-to-number (match-string 2 passage))))
   ((and (stringp passage)
         (string-match "\\`line \\([0-9]+\\)\\(?::\\|\\'\\)" passage))
    (let ((line (string-to-number (match-string 1 passage))))
      (cons line line)))
   (t nil)))

(defun arxana-browser-songs--passage-search-text (passage)
  (cond
   ((not (stringp passage)) nil)
   ((string-match "\\`lines? [0-9]+\\(?:-[0-9]+\\)?:\\s-*\\(.+\\)\\'" passage)
    (string-trim (match-string 1 passage)))
   (t (string-trim passage))))

(defun arxana-browser-songs--locate-passage-bounds (passage)
  (or
   (let ((line-bounds (arxana-browser-songs--line-bounds-from-passage passage)))
     (when (and line-bounds (vectorp arxana-browser-songs--line-map))
       (let* ((start-line (car line-bounds))
              (end-line (cdr line-bounds))
              (start-entry (and (< 0 start-line (length arxana-browser-songs--line-map))
                                (aref arxana-browser-songs--line-map start-line)))
              (end-entry (and (< 0 end-line (length arxana-browser-songs--line-map))
                              (aref arxana-browser-songs--line-map end-line))))
         (when (and start-entry end-entry)
           (cons (car start-entry) (cdr end-entry))))))
   (let ((text (arxana-browser-songs--passage-search-text passage)))
     (when (and (stringp text) (not (string-empty-p text)))
       (save-excursion
         (goto-char (point-min))
         (when (search-forward text nil t)
           (cons (match-beginning 0) (match-end 0))))))))

(defun arxana-browser-songs--focus-bounds (bounds)
  (when (and bounds (consp bounds))
    (unless (overlayp arxana-browser-songs--active-overlay)
      (setq arxana-browser-songs--active-overlay
            (make-overlay (car bounds) (cdr bounds)))
      (overlay-put arxana-browser-songs--active-overlay
                   'face 'arxana-browser-songs-active-face)
      (overlay-put arxana-browser-songs--active-overlay
                   'priority arxana-browser-songs-active-overlay-priority))
    (move-overlay arxana-browser-songs--active-overlay
                  (car bounds) (cdr bounds))
    (goto-char (car bounds))
    (recenter)))

(defun arxana-browser-songs--highlight-source-bounds (bounds)
  (when (and bounds (consp bounds))
    (unless (overlayp arxana-browser-songs--active-overlay)
      (setq arxana-browser-songs--active-overlay
            (make-overlay (car bounds) (cdr bounds)))
      (overlay-put arxana-browser-songs--active-overlay
                   'face 'arxana-browser-songs-active-face)
      (overlay-put arxana-browser-songs--active-overlay
                   'priority arxana-browser-songs-active-overlay-priority))
    (move-overlay arxana-browser-songs--active-overlay
                  (car bounds) (cdr bounds))))

(defun arxana-browser-songs--refresh-entry-highlights (&optional active-hyperedge)
  (when (hash-table-p arxana-browser-songs--entry-overlays)
    (maphash
     (lambda (_hyperedge-id overlay)
       (when (overlayp overlay)
         (overlay-put
          overlay
          'face
          (pcase arxana-browser-songs--highlight-scope
            ('point nil)
            (_ (overlay-get overlay 'arxana-song-base-face))))))
     arxana-browser-songs--entry-overlays))
  (when (and (eq arxana-browser-songs--highlight-scope 'point)
             (overlayp arxana-browser-songs--active-overlay))
    (if active-hyperedge
        (overlay-put arxana-browser-songs--active-overlay
                     'face 'arxana-browser-songs-active-face)
      (delete-overlay arxana-browser-songs--active-overlay)
      (setq arxana-browser-songs--active-overlay nil))))

(defun arxana-browser-songs-toggle-highlight-scope ()
  "Toggle whether all annotations are tinted or only the current one."
  (interactive)
  (let ((text-buf (if (hash-table-p arxana-browser-songs--entry-overlays)
                      (current-buffer)
                    arxana-browser-songs--peer-buffer)))
    (unless (buffer-live-p text-buf)
      (user-error "No songs text buffer is associated with this view"))
    (with-current-buffer text-buf
      (setq-local arxana-browser-songs--highlight-scope
                  (if (eq arxana-browser-songs--highlight-scope 'all) 'point 'all))
      (arxana-browser-songs--refresh-entry-highlights
       (arxana-browser-songs--hyperedge-id-at-point))
      (arxana-browser-songs--sync-note-from-point)
      (message "Songs highlight scope: %s"
               (downcase (arxana-browser-songs--scope-label
                          arxana-browser-songs--highlight-scope))))
    (when (buffer-live-p text-buf)
      (with-current-buffer text-buf
        (arxana-browser-songs--refresh-header-lines)))))

(defun arxana-browser-songs--fetch-entity-record (entity-id &optional cache)
  (let ((cached (and cache (gethash entity-id cache))))
    (or cached
        (let* ((response (and entity-id (arxana-store-fetch-entity entity-id)))
               (entity (and (listp response) (alist-get :entity response))))
          (when (and cache entity-id entity)
            (puthash entity-id entity cache))
          entity))))

(defun arxana-browser-songs--jump-to-note (hyperedge-id)
  (let ((buf (get-buffer arxana-browser-songs-notes-buffer)))
    (when (buffer-live-p buf)
      (let ((win (display-buffer-in-side-window
                  buf
                  (list (cons 'side arxana-browser-songs-notes-side)
                        (cons 'window-width arxana-browser-songs-notes-width)))))
        (with-current-buffer buf
          (when-let* ((bounds (and (hash-table-p arxana-browser-songs--note-index)
                                   (gethash hyperedge-id arxana-browser-songs--note-index)))
                      (start (car bounds)))
            (arxana-browser-songs--highlight-note-bounds bounds)
            (goto-char start)
            (when (window-live-p win)
              (with-selected-window win
                (goto-char start)
                (recenter)))))))))

(defun arxana-browser-songs--jump-to-source (hyperedge-id)
  (let ((buf (get-buffer arxana-browser-songs-text-buffer)))
    (when (buffer-live-p buf)
      (let ((win (display-buffer buf)))
        (with-current-buffer buf
          (let ((bounds (and (hash-table-p arxana-browser-songs--source-index)
                             (gethash hyperedge-id arxana-browser-songs--source-index))))
            (when bounds
              (arxana-browser-songs--focus-bounds bounds)
              (when (window-live-p win)
                (with-selected-window win
                  (arxana-browser-songs--focus-bounds bounds))))))))))

(defun arxana-browser-songs-text-activate ()
  "Jump from an inline annotation marker to its note entry."
  (interactive)
  (let ((hyperedge-id (arxana-browser-songs--hyperedge-id-at-point)))
    (unless hyperedge-id
      (user-error "No annotation marker at point"))
    (arxana-browser-songs--jump-to-note hyperedge-id)))

(defun arxana-browser-songs--hyperedge-id-at-point ()
  (or (get-text-property (point) 'arxana-song-hyperedge-id)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'arxana-song-hyperedge-id))
      (arxana-browser-songs--overlay-hyperedge-id-at (point))
      (and (> (point) (point-min))
           (save-excursion
             (goto-char (1- (point)))
             (arxana-browser-songs--overlay-hyperedge-id-at (point))))))

(defun arxana-browser-songs--overlay-hyperedge-id-at (pos)
  (when pos
    (let* ((candidates
            (seq-filter
             (lambda (ov) (overlay-get ov 'arxana-song-hyperedge-id))
             (overlays-at pos)))
           (best
            (car
             (sort candidates
                   (lambda (a b)
                     (< (- (overlay-end a) (overlay-start a))
                        (- (overlay-end b) (overlay-start b))))))))
      (and best (overlay-get best 'arxana-song-hyperedge-id)))))

(defun arxana-browser-songs--highlight-note-bounds (bounds)
  (when (and bounds (consp bounds))
    (unless (overlayp arxana-browser-songs--note-highlight-overlay)
      (setq arxana-browser-songs--note-highlight-overlay
            (make-overlay (car bounds) (cdr bounds)))
      (overlay-put arxana-browser-songs--note-highlight-overlay
                   'face 'arxana-browser-songs-active-face)
      (overlay-put arxana-browser-songs--note-highlight-overlay
                   'priority arxana-browser-songs-active-overlay-priority))
    (move-overlay arxana-browser-songs--note-highlight-overlay
                  (car bounds) (cdr bounds))))

(defun arxana-browser-songs--sync-note-from-point ()
  (let ((hyperedge-id (arxana-browser-songs--hyperedge-id-at-point)))
    (unless (equal hyperedge-id arxana-browser-songs--last-active-hyperedge)
      (setq arxana-browser-songs--last-active-hyperedge hyperedge-id)
      (if (and hyperedge-id (hash-table-p arxana-browser-songs--source-index))
          (when-let ((bounds (gethash hyperedge-id arxana-browser-songs--source-index)))
            (arxana-browser-songs--highlight-source-bounds bounds)
            (arxana-browser-songs--refresh-entry-highlights hyperedge-id))
        (when (overlayp arxana-browser-songs--active-overlay)
          (delete-overlay arxana-browser-songs--active-overlay)
          (setq arxana-browser-songs--active-overlay nil))
        (arxana-browser-songs--refresh-entry-highlights nil))
      (let ((notes-buf (get-buffer arxana-browser-songs-notes-buffer)))
        (when (buffer-live-p notes-buf)
          (with-current-buffer notes-buf
            (if (and hyperedge-id (hash-table-p arxana-browser-songs--note-index))
                (when-let ((bounds (gethash hyperedge-id arxana-browser-songs--note-index)))
                  (arxana-browser-songs--highlight-note-bounds bounds)
                  (let ((win (get-buffer-window notes-buf t)))
                    (when (window-live-p win)
                      (with-selected-window win
                        (goto-char (car bounds))
                        (recenter)))))
              (when (overlayp arxana-browser-songs--note-highlight-overlay)
                (delete-overlay arxana-browser-songs--note-highlight-overlay)
                (setq arxana-browser-songs--note-highlight-overlay nil)))))))))

(defun arxana-browser-songs--sync-source-from-point ()
  (let ((hyperedge-id (arxana-browser-songs--hyperedge-id-at-point)))
    (unless (equal hyperedge-id arxana-browser-songs--last-active-hyperedge)
      (setq arxana-browser-songs--last-active-hyperedge hyperedge-id)
      (let ((text-buf (get-buffer arxana-browser-songs-text-buffer)))
        (when (buffer-live-p text-buf)
          (with-current-buffer text-buf
            (if (and hyperedge-id (hash-table-p arxana-browser-songs--source-index))
                (when-let ((bounds (gethash hyperedge-id arxana-browser-songs--source-index)))
                  (arxana-browser-songs--focus-bounds bounds)
                  (let ((win (get-buffer-window text-buf t)))
                    (when (window-live-p win)
                      (with-selected-window win
                        (goto-char (car bounds))
                        (recenter)))))
              (when (overlayp arxana-browser-songs--active-overlay)
                (delete-overlay arxana-browser-songs--active-overlay)
                (setq arxana-browser-songs--active-overlay nil)))))))))

(defun arxana-browser-songs--endpoint-role (endpoint)
  (arxana-browser-songs--value-name
   (arxana-browser-songs--alist-value endpoint :role :hx/role)))

(defun arxana-browser-songs--endpoint-entity-id (endpoint)
  (arxana-browser-songs--alist-value endpoint :entity-id :id))

(defun arxana-browser-songs--endpoint-passage (endpoint)
  (arxana-browser-songs--alist-value endpoint :passage :hx/passage))

(defun arxana-browser-songs--annotation-props (hyperedge)
  (or (alist-get :hx/props hyperedge)
      (alist-get :props hyperedge)
      '()))

(defun arxana-browser-songs--hyperedge-kind (hyperedge)
  (arxana-browser-songs--value-name
   (arxana-browser-songs--alist-value hyperedge :hx/type :type)))

(defun arxana-browser-songs--annotation-sections (entity)
  (let* ((entity-id (arxana-browser-songs--entity-id entity))
         (response (and entity-id
                        (arxana-store-fetch-hyperedges
                         :end entity-id
                         :limit arxana-browser-songs-annotations-limit)))
         (hyperedges (arxana-browser-songs--unwrap-hyperedges response))
         (entity-cache (make-hash-table :test 'equal))
         supports backlinks questions)
    (when entity-id
      (puthash entity-id entity entity-cache))
    (dolist (hyperedge hyperedges)
      (let* ((kind (arxana-browser-songs--hyperedge-kind hyperedge))
             (ends (or (alist-get :hx/ends hyperedge) '()))
             (current-end (seq-find (lambda (ep)
                                      (equal entity-id
                                             (arxana-browser-songs--endpoint-entity-id ep)))
                                    ends))
             (props (arxana-browser-songs--annotation-props hyperedge))
             (annotated (seq-find (lambda (ep)
                                    (equal (arxana-browser-songs--endpoint-role ep) "annotated"))
                                  ends))
             (source (seq-find (lambda (ep)
                                 (equal (arxana-browser-songs--endpoint-role ep) "source"))
                               ends)))
        (cond
         ((and (equal kind "annotation/open-question") current-end)
          (push (list :kind 'open-question
                      :hyperedge-id (alist-get :hx/id hyperedge)
                      :passage (or (arxana-browser-songs--endpoint-passage current-end) "")
                      :question (or (arxana-browser-songs--alist-value props :question)
                                    "Open question"))
                questions))
         ((and (equal kind "annotation/supports") current-end)
          (cond
           ((and annotated
                 (equal entity-id (arxana-browser-songs--endpoint-entity-id annotated))
                 source)
            (let* ((other-id (arxana-browser-songs--endpoint-entity-id source))
                   (other-entity (arxana-browser-songs--fetch-entity-record other-id entity-cache)))
              (push (list :kind 'support
                          :hyperedge-id (alist-get :hx/id hyperedge)
                          :target-id other-id
                          :target-name (or (and other-entity (arxana-browser-songs--entity-name other-entity))
                                           other-id)
                          :target-type (and other-entity (arxana-browser-songs--entity-type other-entity))
                          :current-passage (or (arxana-browser-songs--endpoint-passage annotated) "")
                          :target-passage (or (arxana-browser-songs--endpoint-passage source) "")
                          :note (or (arxana-browser-songs--alist-value props :note :gloss) ""))
                    supports)))
           ((and source
                 (equal entity-id (arxana-browser-songs--endpoint-entity-id source))
                 annotated)
            (let* ((other-id (arxana-browser-songs--endpoint-entity-id annotated))
                   (other-entity (arxana-browser-songs--fetch-entity-record other-id entity-cache)))
              (push (list :kind 'backlink
                          :hyperedge-id (alist-get :hx/id hyperedge)
                          :target-id other-id
                          :target-name (or (and other-entity (arxana-browser-songs--entity-name other-entity))
                                           other-id)
                          :target-type (and other-entity (arxana-browser-songs--entity-type other-entity))
                          :current-passage (or (arxana-browser-songs--endpoint-passage source) "")
                          :target-passage (or (arxana-browser-songs--endpoint-passage annotated) "")
                          :note (or (arxana-browser-songs--alist-value props :note :gloss) ""))
                    backlinks))))))))
    (delq nil
          (list (when supports
                  (list :title "Supported By Chapbook"
                        :items (nreverse supports)))
                (when backlinks
                  (list :title "Backlinks To Suite Songs"
                        :items (nreverse backlinks)))
                (when questions
                  (list :title "Open Questions"
                        :items (nreverse questions)))))))

(defvar arxana-browser-songs-notes-link-map nil
  "Keymap for note links in the songs browser notes pane.")
(setq arxana-browser-songs-notes-link-map (make-sparse-keymap))
(define-key arxana-browser-songs-notes-link-map [mouse-1] #'arxana-browser-songs-notes-activate)
(define-key arxana-browser-songs-notes-link-map (kbd "RET") #'arxana-browser-songs-notes-activate)

(defvar arxana-browser-songs-notes-mode-map nil
  "Keymap for `arxana-browser-songs-notes-mode'.")
(setq arxana-browser-songs-notes-mode-map (make-sparse-keymap))
(set-keymap-parent arxana-browser-songs-notes-mode-map view-mode-map)
(define-key arxana-browser-songs-notes-mode-map (kbd "<left>") #'arxana-browser-songs-left-or-return)
(define-key arxana-browser-songs-notes-mode-map (kbd "RET") #'arxana-browser-songs-notes-activate)
(define-key arxana-browser-songs-notes-mode-map (kbd "h") #'arxana-browser-songs-toggle-highlight-scope)

(define-minor-mode arxana-browser-songs-notes-mode
  "Minor mode for the songs annotation notes buffer."
  :lighter " Song-Notes"
  :keymap arxana-browser-songs-notes-mode-map
  :group 'arxana-browser-songs
  (if arxana-browser-songs-notes-mode
      (add-hook 'post-command-hook #'arxana-browser-songs--sync-source-from-point nil t)
    (remove-hook 'post-command-hook #'arxana-browser-songs--sync-source-from-point t)))

(defun arxana-browser-songs--note-target-id-at-point ()
  (or (get-text-property (point) 'arxana-song-target-id)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'arxana-song-target-id))))

(defun arxana-browser-songs--put-note-entry-properties (start end hyperedge-id &rest properties)
  (add-text-properties
   start end
   (append (list 'arxana-song-hyperedge-id hyperedge-id)
           properties)))

(defun arxana-browser-songs-notes-activate ()
  "Open the related entity at point from the songs notes pane."
  (interactive)
  (let* ((entity-id (arxana-browser-songs--note-target-id-at-point))
         (passage (or (get-text-property (point) 'arxana-song-target-passage)
                      (and (> (point) (point-min))
                           (get-text-property (1- (point)) 'arxana-song-target-passage))))
         (source-passage (arxana-browser-songs--note-source-passage-at-point))
         (hyperedge-id (or (get-text-property (point) 'arxana-song-hyperedge-id)
                           (and (> (point) (point-min))
                                (get-text-property (1- (point)) 'arxana-song-hyperedge-id))))
         (jump-kind (or (get-text-property (point) 'arxana-song-jump-kind)
                        (and (> (point) (point-min))
                             (get-text-property (1- (point)) 'arxana-song-jump-kind))))
         (source-buffer (or (and (buffer-live-p arxana-browser-songs--peer-buffer)
                                 arxana-browser-songs--peer-buffer)
                            (current-buffer)))
         (return-item (arxana-browser-songs--make-navigation-item
                       source-buffer source-passage)))
    (pcase jump-kind
      ('source
       (unless hyperedge-id
         (user-error "No source annotation at point"))
       (arxana-browser-songs--jump-to-source hyperedge-id))
      (_
       (unless entity-id
         (user-error "No related entity at point"))
       (arxana-browser-songs-open
        (list :entity-id entity-id
              :focus-passage passage
              :return-item return-item))))))

(defun arxana-browser-songs--insert-note-link (title entity-id passage hyperedge-id jump-kind)
  (let ((start (point)))
    (insert (format "- %s\n" title))
    (add-text-properties
     start (point)
     (list 'arxana-song-target-id entity-id
           'arxana-song-target-passage passage
           'arxana-song-hyperedge-id hyperedge-id
           'arxana-song-jump-kind jump-kind
           'face 'arxana-browser-songs-note-link-face
           'font-lock-face 'arxana-browser-songs-note-link-face
           'mouse-face 'highlight
           'follow-link t
           'keymap arxana-browser-songs-notes-link-map
           'help-echo "Open related entity"))))

(defun arxana-browser-songs--insert-action-line (label properties &optional face)
  (let ((start (point)))
    (insert label)
    (add-text-properties
     start (point)
     (append
      properties
      (list 'face (or face 'arxana-browser-songs-note-link-face)
            'font-lock-face (or face 'arxana-browser-songs-note-link-face)
            'mouse-face 'highlight
            'follow-link t
            'keymap arxana-browser-songs-notes-link-map)))))

(defun arxana-browser-songs--render-notes (entity sections)
  (let ((buf (get-buffer-create arxana-browser-songs-notes-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max))
        (setq-local arxana-browser-songs--note-highlight-overlay nil)
        (setq-local arxana-browser-songs--last-active-hyperedge nil)
        (setq-local arxana-browser-songs--current-entity-id
                    (arxana-browser-songs--entity-id entity))
        (erase-buffer)
        (org-mode)
        (setq-local arxana-browser-songs--note-index (make-hash-table :test 'equal))
        (setq-local truncate-lines nil)
        (setq-local header-line-format '(:eval (arxana-browser-songs--notes-header-line)))
        (insert (format "#+TITLE: Notes for %s\n\n"
                        (arxana-browser-songs--entity-name entity)))
        (insert (format "- Entity: %s\n" (or (arxana-browser-songs--entity-id entity) "?")))
        (insert (format "- Type: %s\n\n" (or (arxana-browser-songs--entity-type entity) "?")))
        (if (null sections)
            (progn
              (insert "* Notes\n")
              (insert "- (none yet)\n")
              (insert "  - Import the demo annotations or add new hyperedges to see cross-links here.\n"))
          (dolist (section sections)
            (insert (format "* %s\n" (plist-get section :title)))
            (dolist (entry (plist-get section :items))
              (let ((entry-start (point)))
                (if (eq (plist-get entry :kind) 'open-question)
                    (progn
                      (arxana-browser-songs--insert-action-line
                       (format "- %s %s\n"
                               (or (plist-get entry :marker-label) "")
                               (or (plist-get entry :question) ""))
                       (list 'arxana-song-hyperedge-id (plist-get entry :hyperedge-id)
                             'arxana-song-jump-kind 'source
                             'help-echo "Jump to the marked passage in the current text"))
                      (when-let ((passage (string-trim (or (plist-get entry :passage) ""))))
                        (unless (string-empty-p passage)
                          (insert (format "  %s\n" passage)))))
                  (arxana-browser-songs--insert-note-link
                   (format "%s %s"
                           (or (plist-get entry :marker-label) "")
                           (or (plist-get entry :target-name) "(unknown)"))
                   (plist-get entry :target-id)
                   (plist-get entry :target-passage)
                   (plist-get entry :hyperedge-id)
                   'target)
                  (arxana-browser-songs--insert-action-line
                   (format "  %s\n"
                           (or (plist-get entry :target-passage) ""))
                   (list 'arxana-song-target-id (plist-get entry :target-id)
                         'arxana-song-target-passage (plist-get entry :target-passage)
                         'arxana-song-hyperedge-id (plist-get entry :hyperedge-id)
                         'arxana-song-jump-kind 'target
                         'help-echo "Open the cited passage in the related entity")
                   'arxana-browser-songs-note-quote-face)
                  (when-let ((note (string-trim (or (plist-get entry :note) ""))))
                    (unless (string-empty-p note)
                      (insert (format "  - Gloss: %s\n" note)))))
                (insert "\n")
                (arxana-browser-songs--put-note-entry-properties
                 entry-start
                 (point)
                 (plist-get entry :hyperedge-id)
                 'arxana-song-jump-kind (if (eq (plist-get entry :kind) 'open-question)
                                            'source
                                          'target)
                 'arxana-song-source-passage (or (plist-get entry :current-passage)
                                                 (plist-get entry :passage))
                 'arxana-song-target-id (plist-get entry :target-id)
                 'arxana-song-target-passage (plist-get entry :target-passage))
                (puthash (plist-get entry :hyperedge-id)
                         (cons (copy-marker entry-start)
                               (copy-marker (point)))
                         arxana-browser-songs--note-index)))))
        (goto-char (point-min))
        (view-mode 1)
        (arxana-browser-songs-notes-mode 1)))
    buf))

(defun arxana-browser-songs--render-entity (entity sections)
  (let ((buf (get-buffer-create arxana-browser-songs-text-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max))
        (setq-local arxana-browser-songs--line-map nil)
        (setq-local arxana-browser-songs--source-index (make-hash-table :test 'equal))
        (setq-local arxana-browser-songs--entry-overlays (make-hash-table :test 'equal))
        (setq-local arxana-browser-songs--current-entity-id
                    (arxana-browser-songs--entity-id entity))
        (setq-local arxana-browser-songs--highlight-scope
                    arxana-browser-songs-highlight-scope-default)
        (setq-local arxana-browser-songs--active-overlay nil)
        (setq-local arxana-browser-songs--last-active-hyperedge nil)
        (erase-buffer)
        (setq-local header-line-format '(:eval (arxana-browser-songs--text-header-line)))
        (insert (format "Title: %s\n" (arxana-browser-songs--entity-name entity)))
        (insert (format "Entity: %s\n" (or (arxana-browser-songs--entity-id entity) "?")))
        (insert (format "Type: %s\n" (or (arxana-browser-songs--entity-type entity) "?")))
        (let* ((source (or (arxana-browser-songs--entity-source entity) ""))
               (lines (split-string source "\n" nil))
               (line-map (make-vector (1+ (max 1 (length lines))) nil)))
          (insert (format "Lines: %d\n\n" (length lines)))
          (cl-loop for line in lines
                   for idx from 1 do
                   (let ((line-start (point)))
                     (insert (format "%3d  %s\n" idx line))
                     (aset line-map idx
                           (cons (+ line-start 5)
                                 (max (+ line-start 5) (1- (point)))))))
          (setq-local arxana-browser-songs--line-map line-map))
        (dolist (entry (arxana-browser-songs--flatten-section-items sections))
          (let ((bounds (arxana-browser-songs--locate-passage-bounds
                         (plist-get entry :current-passage))))
            (when bounds
              (puthash (plist-get entry :hyperedge-id) bounds
                       arxana-browser-songs--source-index)
              (let ((overlay (make-overlay (car bounds) (cdr bounds)))
                    (marker-label (or (plist-get entry :marker-label) "[*]")))
                (overlay-put overlay 'arxana-song-base-face
                             (if (eq (plist-get entry :kind) 'open-question)
                                 'arxana-browser-songs-question-face
                               'arxana-browser-songs-annotation-face))
                (overlay-put overlay 'face (overlay-get overlay 'arxana-song-base-face))
                (overlay-put overlay 'priority arxana-browser-songs-base-overlay-priority)
                (overlay-put overlay 'arxana-song-hyperedge-id
                             (plist-get entry :hyperedge-id))
                (overlay-put overlay 'mouse-face 'highlight)
                (overlay-put overlay 'help-echo "Jump to annotation note")
                (overlay-put overlay 'keymap arxana-browser-songs-text-link-map)
                (puthash (plist-get entry :hyperedge-id) overlay
                         arxana-browser-songs--entry-overlays)
                (let ((marker-text
                       (propertize
                        (format " %s" marker-label)
                        'face 'arxana-browser-songs-marker-face
                        'mouse-face 'highlight
                        'keymap arxana-browser-songs-text-link-map
                        'help-echo "Jump to annotation note"
                        'arxana-song-hyperedge-id (plist-get entry :hyperedge-id))))
                  (overlay-put (make-overlay (cdr bounds) (cdr bounds))
                               'after-string marker-text))))))
        (goto-char (point-min))
        (view-mode 1)
        (arxana-browser-songs-text-mode 1)
        (arxana-browser-songs--refresh-entry-highlights)))
    buf))

(defun arxana-browser-songs--display-text-buffer (buf)
  (let* ((notes-buf (get-buffer arxana-browser-songs-notes-buffer))
         (notes-win (and notes-buf (get-buffer-window notes-buf t)))
         (main-win (or (seq-find (lambda (win)
                                   (and (window-live-p win)
                                        (not (eq win notes-win))
                                        (not (window-parameter win 'window-side))))
                                 (window-list nil 'no-mini))
                       (selected-window))))
    (set-window-buffer main-win buf)
    (select-window main-win)))

(defun arxana-browser-songs-open (item)
  (let* ((entity-id (plist-get item :entity-id))
         (focus-passage (plist-get item :focus-passage))
         (return-buffer (or (plist-get item :return-buffer)
                            (current-buffer)))
         (return-config (or (plist-get item :return-window-config)
                            (current-window-configuration)))
         (return-item (plist-get item :return-item))
         (entity (arxana-browser-songs--fetch-entity-record entity-id)))
    (unless entity
      (user-error "No song entity found for %s" (or entity-id (plist-get item :label))))
    (let* ((sections (arxana-browser-songs--label-sections
                      (arxana-browser-songs--sort-sections
                       (arxana-browser-songs--annotation-sections entity))))
           (text-buf (arxana-browser-songs--render-entity entity sections))
           (notes-buf (arxana-browser-songs--render-notes entity sections)))
      (with-current-buffer text-buf
        (setq-local arxana-browser-songs--peer-buffer notes-buf)
        (setq-local arxana-browser-songs--return-item return-item)
        (setq-local arxana-ui-return-buffer return-buffer)
        (setq-local arxana-ui-return-window-config return-config))
      (with-current-buffer notes-buf
        (setq-local arxana-browser-songs--peer-buffer text-buf)
        (setq-local arxana-browser-songs--return-item return-item)
        (setq-local arxana-ui-return-buffer return-buffer)
        (setq-local arxana-ui-return-window-config return-config))
      (arxana-browser-songs--display-text-buffer text-buf)
      (display-buffer-in-side-window
       notes-buf
       (list (cons 'side arxana-browser-songs-notes-side)
             (cons 'window-width arxana-browser-songs-notes-width)))
      (when focus-passage
        (with-current-buffer text-buf
          (when-let ((bounds (arxana-browser-songs--locate-passage-bounds focus-passage)))
            (arxana-browser-songs--focus-bounds bounds))))
      (with-current-buffer text-buf
        (arxana-browser-songs--sync-note-from-point)
        (arxana-browser-songs--refresh-header-lines))
      text-buf)))

(defun arxana-browser-songs-location (item)
  (let ((entity-id (plist-get item :entity-id)))
    (when entity-id
      (format "arxana://song/%s" (url-hexify-string entity-id)))))

(provide 'arxana-browser-songs)

;;; arxana-browser-songs.el ends here
