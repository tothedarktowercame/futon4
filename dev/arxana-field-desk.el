;;; arxana-field-desk.el --- Morning Brief feature-acceptance desk -*- lexical-binding: t; -*-

;;; Commentary:
;; Arxana surface over the append-only War Machine Morning Brief store.
;; Items, reviews, and notebook addenda are read directly as EDN.  Append-only
;; records are never written here: submissions go asynchronously through the
;; serving JVM's Morning Brief HTTP API.

;;; Code:

(eval-and-compile
  (let ((dir (or (and load-file-name (file-name-directory load-file-name))
                 (and buffer-file-name (file-name-directory buffer-file-name))
                 default-directory)))
    (add-to-list 'load-path dir)))

(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'json)
(require 'url)
(require 'url-http)
(require 'arxana-browser-rewrites)

(defgroup arxana-field-desk nil
  "Morning Brief feature-acceptance review surface."
  :group 'arxana)

(defcustom arxana-field-desk-root
  "/home/joe/code/futon2/data/wm-morning-brief"
  "Root containing append-only Morning Brief items/, reviews/, and addenda/."
  :type 'directory
  :group 'arxana-field-desk)

(defcustom arxana-field-desk-reviewer "joe"
  "Reviewer identity recorded with Morning Brief feature acceptance."
  :type 'string
  :group 'arxana-field-desk)

(defcustom arxana-field-desk-endpoint "http://127.0.0.1:7070"
  "Serving JVM endpoint used to submit Morning Brief reviews."
  :type 'string
  :group 'arxana-field-desk)

(defcustom arxana-field-desk-auto-refresh-seconds nil
  "Idle seconds between automatic store refreshes, or nil to disable."
  :type '(choice (const :tag "Off" nil) number)
  :group 'arxana-field-desk)

(defconst arxana-field-desk--buffer "*Arxana Field Desk*")
(defconst arxana-field-desk--errors-buffer "*field-desk-errors*")

(defconst arxana-field-desk--objective-specs
  '((:feature-verdict
     :question "Accept the built feature?"
     :answers (:accept-feature :accept-with-follow-ups :reject))
    (:selection-quality
     :question "Was this the best available policy selection?"
     :answers (:yes :no :uncertain))
    (:substantive-achievement
     :question "Did the result substantively advance the selected target?"
     :answers (:yes :partial :no :uncertain))
    (:evidence-sufficiency
     :question "Are the commit, validation, review, and grounding evidence sufficient?"
     :answers (:sufficient :insufficient :uncertain))
    (:machine-response
     :question "If the click failed, did the machine stop, remember, and prescribe an adequate discharge?"
     :answers (:correct :incorrect :uncertain))))

(defconst arxana-field-desk--strata
  '((:pending "Pending" "not yet reviewed")
    (:partial "Partially reviewed" "some applicable objectives answered")
    (:full "Fully reviewed" "all applicable objectives answered")))

(defvar-local arxana-field-desk--refresh-fn nil)
(defvar-local arxana-field-desk--current-item nil)
(defvar-local arxana-field-desk--auto-refresh-timer nil)

(defvar arxana-field-desk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-field-desk-open-at-point)
    (define-key map "a" #'arxana-field-desk-answer-at-point)
    (define-key map "v" #'arxana-field-desk-feature-verdict)
    (define-key map "n" #'arxana-field-desk-compose-addendum)
    (define-key map "o" #'arxana-field-desk-open-mission)
    (define-key map "c" #'arxana-field-desk-copy-commit)
    (define-key map "g" #'arxana-field-desk-refresh)
    (define-key map "q" #'quit-window)
    (define-key map "?" #'arxana-field-desk-help)
    map)
  "Keymap for `arxana-field-desk-mode'.")

(define-derived-mode arxana-field-desk-mode special-mode "Field-Desk"
  "Major mode for Morning Brief feature acceptance."
  (add-hook 'kill-buffer-hook #'arxana-field-desk--cancel-timer nil t))

;; ---------------------------------------------------------------- data ----

(defun arxana-field-desk--directory-files (name)
  (let ((dir (expand-file-name name arxana-field-desk-root)))
    (when (file-directory-p dir)
      (sort (directory-files dir t "\\.edn\\'" t) #'string<))))

(defun arxana-field-desk--read-records (name)
  "Read all EDN records in store subdirectory NAME.
This deliberately reuses the same reader as `arxana-ledger--read'."
  (delq nil
        (mapcar (lambda (path)
                  (when (file-readable-p path)
                    (ignore-errors
                      (arxana-browser-rewrites--read-edn-file path))))
                (arxana-field-desk--directory-files name))))

(defun arxana-field-desk--items () (arxana-field-desk--read-records "items"))
(defun arxana-field-desk--reviews () (arxana-field-desk--read-records "reviews"))
(defun arxana-field-desk--addenda ()
  (sort (arxana-field-desk--read-records "addenda")
        (lambda (left right)
          (string< (or (plist-get left :created-at) "")
                   (or (plist-get right :created-at) "")))))

(defun arxana-field-desk--item-addenda (item)
  (let ((attempt (plist-get item :attempt-id)))
    (cl-remove-if-not
     (lambda (addendum) (equal attempt (plist-get addendum :attempt-id)))
     (arxana-field-desk--addenda))))

(defun arxana-field-desk--get-in (plist keys)
  (let ((value plist))
    (dolist (key keys value)
      (setq value (and (listp value) (plist-get value key))))))

(defun arxana-field-desk--evidence-bearing-p (item)
  (or (plist-get item :commit)
      (arxana-field-desk--get-in item '(:achievement :build))))

(defun arxana-field-desk--item-objectives (item)
  "Mirror `futon2.aif.morning-brief/item-objectives'.
Keep this simple conditional projection visibly aligned with the Clojure source."
  (let ((commit-bearing (plist-get item :commit))
        (evidence-bearing (arxana-field-desk--evidence-bearing-p item))
        (outcome (plist-get item :outcome))
        (failure (plist-get item :failure))
        objectives)
    (when commit-bearing
      (setq objectives (append objectives '(:feature-verdict))))
    (setq objectives (append objectives '(:selection-quality
                                           :substantive-achievement)))
    (when evidence-bearing
      (setq objectives (append objectives '(:evidence-sufficiency))))
    (when (or (and outcome (not (eq outcome :grounded-change))) failure)
      (setq objectives (append objectives '(:machine-response))))
    objectives))

(defun arxana-field-desk--answer-table (reviews)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (review reviews table)
      (puthash (cons (plist-get review :attempt-id)
                     (plist-get review :objective))
               review table))))

(defun arxana-field-desk--review-for (item objective reviews)
  (gethash (cons (plist-get item :attempt-id) objective)
           (arxana-field-desk--answer-table reviews)))

(defun arxana-field-desk--review-state (item reviews)
  (let* ((objectives (arxana-field-desk--item-objectives item))
         (table (arxana-field-desk--answer-table reviews))
         (answered (cl-count-if
                    (lambda (objective)
                      (gethash (cons (plist-get item :attempt-id) objective) table))
                    objectives)))
    (cond ((= answered 0) :pending)
          ((< answered (length objectives)) :partial)
          (t :full))))

(defun arxana-field-desk--items-at (stratum &optional items reviews)
  (let ((all-items (or items (arxana-field-desk--items)))
        (all-reviews (or reviews (arxana-field-desk--reviews))))
    (cl-remove-if-not
     (lambda (item)
       (eq stratum (arxana-field-desk--review-state item all-reviews)))
     all-items)))

(defun arxana-field-desk--objective-spec (objective)
  (cdr (assq objective arxana-field-desk--objective-specs)))

(defun arxana-field-desk--keyword-name (value)
  (cond ((keywordp value) (substring (symbol-name value) 1))
        ((symbolp value) (symbol-name value))
        ((null value) "—")
        (t (format "%s" value))))

(defun arxana-field-desk--as-list (value)
  (cond ((vectorp value) (append value nil))
        ((listp value) value)
        ((null value) nil)
        (t (list value))))

(defun arxana-field-desk--commit (item)
  (or (plist-get item :commit)
      (arxana-field-desk--get-in item '(:achievement :build :validation
                                        :artifact-binding :commit))
      (car (arxana-field-desk--as-list
            (arxana-field-desk--get-in item '(:achievement :build :commits))))
      (arxana-field-desk--get-in item '(:achievement :adjudication :after
                                        :implementation-entity :props
                                        :implementation/commit))))

(defun arxana-field-desk--repository (item)
  (or (arxana-field-desk--get-in item '(:achievement :build :validation
                                        :artifact-binding :repo))
      (arxana-field-desk--get-in item '(:achievement :adjudication :after
                                        :implementation-entity :props
                                        :implementation/repository))))

(defun arxana-field-desk--mission-path (item)
  (or (arxana-field-desk--get-in item '(:qa-targets :selection :policy :mission-path))
      (arxana-field-desk--get-in item '(:selection-review :selected-action
                                        :mission-path))))

(defun arxana-field-desk--review-job (item)
  (or (arxana-field-desk--get-in item '(:achievement :build :validation :review-job))
      (arxana-field-desk--get-in item '(:achievement :adjudication :after
                                        :implementation-entity :props
                                        :implementation/review-job))))

;; -------------------------------------------------------------- render ----

(defun arxana-field-desk--cancel-timer ()
  (when (timerp arxana-field-desk--auto-refresh-timer)
    (cancel-timer arxana-field-desk--auto-refresh-timer))
  (setq arxana-field-desk--auto-refresh-timer nil))

(defun arxana-field-desk--timer-refresh (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'arxana-field-desk-mode)
                 arxana-field-desk--refresh-fn)
        (funcall arxana-field-desk--refresh-fn)))))

(defun arxana-field-desk--configure-timer ()
  (arxana-field-desk--cancel-timer)
  (when (and (numberp arxana-field-desk-auto-refresh-seconds)
             (> arxana-field-desk-auto-refresh-seconds 0))
    (setq arxana-field-desk--auto-refresh-timer
          (run-with-idle-timer arxana-field-desk-auto-refresh-seconds t
                               #'arxana-field-desk--timer-refresh
                               (current-buffer)))))

(defun arxana-field-desk--render-frame (body-fn refresh-fn &optional item)
  (let ((buffer (get-buffer-create arxana-field-desk--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'arxana-field-desk-mode)
        (arxana-field-desk-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall body-fn)
        (goto-char (point-min)))
      (setq arxana-field-desk--refresh-fn refresh-fn
            arxana-field-desk--current-item item)
      (arxana-field-desk--configure-timer))
    (pop-to-buffer buffer)))

(defun arxana-field-desk--insert-value (label value)
  (insert (format "  %-18s %s\n" label
                  (if (null value) "—" (format "%s" value)))))

(defun arxana-field-desk--insert-verbatim (value &optional indent)
  (let ((prefix (or indent "  ")))
    (dolist (line (split-string (format "%s" value) "\n"))
      (insert prefix line "\n"))))

(defun arxana-field-desk--insert-objective (item objective reviews)
  (let* ((spec (arxana-field-desk--objective-spec objective))
         (review (arxana-field-desk--review-for item objective reviews))
         (start (point)))
    (insert (format "[%s] %s\n"
                    (if review (arxana-field-desk--keyword-name
                                (plist-get review :answer))
                      "unanswered")
                    (plist-get spec :question)))
    (when review
      (insert (format "  note: %s\n" (or (plist-get review :note) "—"))))
    (add-text-properties
     start (point)
     (list 'arxana-field-desk-item item
           'arxana-field-desk-objective objective
           'mouse-face 'highlight
           'help-echo "a: answer this objective"))))

(defun arxana-field-desk--commit-subject (item)
  "Return the one-line subject of the item's commit, or nil.
Read-only `git show'; guarded so a missing repo or sha never errors."
  (let ((repo (arxana-field-desk--repository item))
        (sha (arxana-field-desk--commit item)))
    (when (and (stringp repo) (stringp sha) (file-directory-p repo))
      (with-temp-buffer
        (when (zerop (ignore-errors
                       (call-process "git" nil t nil "-C" repo
                                     "show" "--no-patch" "--format=%s" sha)))
          (let ((subject (string-trim (buffer-string))))
            (unless (string-empty-p subject) subject)))))))

(defun arxana-field-desk--insert-selection (item)
  (let* ((policy (or (arxana-field-desk--get-in
                      item '(:selection-review :selected-action))
                     (arxana-field-desk--get-in
                      item '(:qa-targets :selection :policy)))))
    (when policy
      (insert "WHY THIS WORK WAS SELECTED\n==========================\n")
      (arxana-field-desk--insert-value
       "action" (format "%s %s"
                        (arxana-field-desk--keyword-name (plist-get policy :type))
                        (or (plist-get policy :target)
                            (arxana-field-desk--keyword-name
                             (plist-get policy :target-class))
                            "")))
      (when-let ((rationale (plist-get policy :rationale)))
        (insert "  rationale:\n")
        (arxana-field-desk--insert-verbatim rationale "    "))
      (insert "\n"))))

(defun arxana-field-desk--insert-built-record (item)
  "Insert the loop-record facts about what was built (card or no card)."
  (let* ((achievement (plist-get item :achievement))
         (build (plist-get achievement :build))
         (subject (arxana-field-desk--commit-subject item))
         (artifacts (arxana-field-desk--as-list (plist-get build :artifacts)))
         (patterns (arxana-field-desk--as-list (plist-get build :patterns-used))))
    (insert "WHAT WAS BUILT (from the loop record)\n")
    (insert "=====================================\n")
    (when-let ((summary (plist-get achievement :summary)))
      (arxana-field-desk--insert-value "summary" summary))
    (when subject
      (arxana-field-desk--insert-value "commit says" subject))
    (when artifacts
      (insert "  files:\n")
      (dolist (file artifacts) (insert (format "    - %s\n" file))))
    (when patterns
      (insert "  patterns used:\n")
      (dolist (pattern patterns) (insert (format "    - %s\n" pattern))))
    (unless (or subject artifacts patterns (plist-get achievement :summary))
      (insert "  No additional build facts were recorded.\n"))
    (insert "\n")))

(defun arxana-field-desk--insert-notebook (item)
  (let* ((addenda (arxana-field-desk--item-addenda item))
         (things-to-try
          (arxana-field-desk--as-list
           (arxana-field-desk--get-in item '(:feature-card :things-to-try)))))
    (insert "NOTEBOOK — why built / how to reproduce\n")
    (insert "======================================\n")
    (dolist (addendum addenda)
      (let ((created-at (or (plist-get addendum :created-at) "—")))
        (insert (format "── %s · %s · %s · %s\n"
                        (arxana-field-desk--keyword-name
                         (plist-get addendum :kind))
                        (or (plist-get addendum :title) "—")
                        (or (plist-get addendum :author) "—")
                        (if (and (stringp created-at) (>= (length created-at) 10))
                            (substring created-at 0 10)
                          created-at)))
        (arxana-field-desk--insert-verbatim (plist-get addendum :body) "  ")))
    (when (and (null things-to-try) (null addenda))
      (insert "No repro notes yet — press n to add what you tried and observed.\n"))
    (insert "\n")))

(defun arxana-field-desk--insert-validation-review (item)
  (let* ((validation (arxana-field-desk--get-in
                      item '(:achievement :build :validation))))
    (when validation
      (let ((author (plist-get validation :author))
            (reviewer (plist-get validation :reviewer))
            (approved (plist-get validation :approved?)))
        (insert "VALIDATION & INDEPENDENT REVIEW\n")
        (insert "===============================\n")
        (when author
          (arxana-field-desk--insert-value
           "author" (format "executed=%s tool-events=%s"
                            (plist-get author :executed)
                            (plist-get author :tool-events))))
        (when reviewer
          (let ((executed (plist-get reviewer :executed)))
            (arxana-field-desk--insert-value
             "reviewer" (format "executed=%s tool-events=%s%s"
                                executed (plist-get reviewer :tool-events)
                                (if (eq executed t) ""
                                  "  ⟵ READ-ONLY review; no gates were executed")))))
        (arxana-field-desk--insert-value "approved?" approved)
        (insert "\n")))))

(defun arxana-field-desk--insert-failure (item)
  (let ((failure (plist-get item :failure)))
    (when failure
      (insert "WHAT HAPPENED (why this attempt did not ground)\n")
      (insert "===============================================\n")
      (arxana-field-desk--insert-value
       "failure" (arxana-field-desk--keyword-name (plist-get failure :kind)))
      (arxana-field-desk--insert-value
       "at stage" (arxana-field-desk--keyword-name (plist-get failure :stage)))
      (when-let ((error-text (plist-get failure :error)))
        (arxana-field-desk--insert-value "error" error-text))
      (when-let ((repair (plist-get failure :repair-id)))
        (arxana-field-desk--insert-value "repair obligation" repair))
      (when-let ((requires (arxana-field-desk--as-list
                            (arxana-field-desk--get-in
                             failure '(:discharge-contract :requires)))))
        (insert "  discharge requires:\n")
        (dolist (requirement requires)
          (insert (format "    - %s\n"
                          (arxana-field-desk--keyword-name requirement)))))
      (insert "\n"))))

(defun arxana-field-desk--insert-feature (item)
  (let ((card (plist-get item :feature-card)))
    (insert "THE FEATURE\n===========\n")
    (if card
        (progn
          (insert "Built\n")
          (arxana-field-desk--insert-verbatim (plist-get card :built))
          (insert "Matches intent?\n")
          (arxana-field-desk--insert-verbatim (plist-get card :matches-intent?))
          (insert "Want coverage\n")
          (arxana-field-desk--insert-verbatim (plist-get card :want-coverage))
          (insert "Things to try\n")
          (dolist (step (arxana-field-desk--as-list
                         (plist-get card :things-to-try)))
            (insert (format "  - %s\n" step)))
          (when (plist-member card :fold-ref)
            (arxana-field-desk--insert-value "fold-ref" (plist-get card :fold-ref)))
          (when (plist-member card :proof-ref)
            (arxana-field-desk--insert-value "proof-ref" (plist-get card :proof-ref))))
      (insert "No feature card: the author did not record a feature claim\n")
      (insert "(build-time gap).  The loop-record facts below are what there\n")
      (insert "is to judge; a reject verdict here is normal for card-less builds.\n"))
    (insert "\n")))

(defun arxana-field-desk--insert-sheet (item reviews)
  (let* ((achievement (plist-get item :achievement))
         (date (or (plist-get item :queued-at) "—"))
         (objectives (arxana-field-desk--item-objectives item)))
    (insert "ARXANA FIELD DESK — FEATURE ACCEPTANCE\n")
    (insert "========================================\n")
    (arxana-field-desk--insert-value "attempt" (plist-get item :attempt-id))
    (arxana-field-desk--insert-value
     "date" (if (and (stringp date) (>= (length date) 10))
                (substring date 0 10) date))
    (arxana-field-desk--insert-value "outcome" (arxana-field-desk--keyword-name
                                                 (plist-get item :outcome)))
    (arxana-field-desk--insert-value "achievement" (arxana-field-desk--keyword-name
                                                     (plist-get achievement :tier)))
    (arxana-field-desk--insert-value "target" (plist-get item :selected-target))
    (arxana-field-desk--insert-value
     "agents" (format "author %s → reviewer %s"
                      (or (plist-get item :author) "—")
                      (or (plist-get item :reviewer) "—")))
    (insert "\n")
    (insert "How to review: read this sheet, then press v to record your\n")
    (insert "verdict (accept-feature / accept-with-follow-ups / reject) with a\n")
    (insert "note.  The appendix questions are optional; answer them with a.\n\n")
    (arxana-field-desk--insert-selection item)
    (arxana-field-desk--insert-feature item)
    (arxana-field-desk--insert-built-record item)
    (arxana-field-desk--insert-notebook item)
    (arxana-field-desk--insert-validation-review item)
    (arxana-field-desk--insert-failure item)
    (insert "EVIDENCE LINKS\n==============\n")
    (arxana-field-desk--insert-value "commit (c)" (arxana-field-desk--commit item))
    (arxana-field-desk--insert-value "repository" (arxana-field-desk--repository item))
    (arxana-field-desk--insert-value "mission path (o)" (arxana-field-desk--mission-path item))
    (arxana-field-desk--insert-value "reviewer job" (arxana-field-desk--review-job item))
    (insert "\nVERDICT\n=======\n")
    (if (memq :feature-verdict objectives)
        (arxana-field-desk--insert-objective item :feature-verdict reviews)
      (insert "[not applicable] No commit-bearing build was recorded.\n"))
    (insert "  Keys: v feature verdict, a answer row\n\n")
    (insert "APPENDIX — DECISION QA\n======================\n")
    (dolist (objective (delq :feature-verdict (copy-sequence objectives)))
      (arxana-field-desk--insert-objective item objective reviews))
    (insert "\nKeys: v=verdict  a=answer  n=notebook  o=mission  c=copy SHA  g=refresh  ?=help  q=quit\n")))

(defun arxana-field-desk--item-summary (item)
  "One line saying what there is to judge, or why the attempt failed."
  (or (when-let ((kind (arxana-field-desk--get-in item '(:failure :kind))))
        (format "failed: %s" (arxana-field-desk--keyword-name kind)))
      (arxana-field-desk--get-in item '(:feature-card :built))
      (arxana-field-desk--get-in item '(:achievement :summary))
      "—"))

(defun arxana-field-desk--insert-item-row (item reviews)
  (let* ((start (point))
         (date (plist-get item :queued-at))
         (summary (arxana-field-desk--item-summary item)))
    (ignore reviews)
    (insert (format "%-13s %-11s %-16s %-26s %s\n"
                    (or (plist-get item :attempt-id) "?")
                    (if (and (stringp date) (>= (length date) 10))
                        (substring date 0 10) "—")
                    (arxana-field-desk--keyword-name (plist-get item :outcome))
                    (truncate-string-to-width
                     (format "%s" (or (plist-get item :selected-target)
                                      (arxana-field-desk--get-in
                                       item '(:qa-targets :selection :policy :target-class))
                                      "—"))
                     26 nil nil "…")
                    (truncate-string-to-width (format "%s" summary) 46 nil nil "…")))
    (add-text-properties start (point)
                         (list 'arxana-field-desk-item item
                               'mouse-face 'highlight
                               'help-echo "RET: open feature-acceptance sheet"))))

;;;###autoload
(defun arxana-field-desk ()
  "Open the Morning Brief Field Desk."
  (interactive)
  (let ((items (arxana-field-desk--items))
        (reviews (arxana-field-desk--reviews)))
    (arxana-field-desk--render-frame
     (lambda ()
       (insert "Arxana Field Desk — Morning Brief feature acceptance\n")
       (insert (format "Source: %s\n\n" arxana-field-desk-root))
       (dolist (spec arxana-field-desk--strata)
         (let ((count (length (arxana-field-desk--items-at
                               (nth 0 spec) items reviews))))
           (insert-text-button
            (format "▸ %s" (nth 1 spec))
            'follow-link t
            'action (let ((stratum (nth 0 spec)))
                      (lambda (_) (arxana-field-desk-open-stratum
                                   (list :stratum stratum)))))
           (insert (format " — %d item(s), %s\n" count (nth 2 spec)))))
       (insert "\nKeys: RET=open  g=refresh  ?=help  q=quit\n"))
     #'arxana-field-desk)))

(defalias 'arxana-field-desk-browse #'arxana-field-desk)

(defun arxana-field-desk-open-stratum (item)
  "Open the Field Desk stratum named by ITEM."
  (let* ((stratum (plist-get item :stratum))
         (reviews (arxana-field-desk--reviews))
         (items (arxana-field-desk--items-at stratum nil reviews))
         (title (nth 1 (assq stratum arxana-field-desk--strata))))
    (arxana-field-desk--render-frame
     (lambda ()
       (insert (format "Arxana Field Desk — %s\n" title))
       (insert "RET on a row opens its reviewable sheet; press v there to record\n")
       (insert "your accept/reject verdict.  Newest first.\n\n")
       (insert (format "%-13s %-11s %-16s %-26s %s\n"
                       "ATTEMPT" "DATE" "OUTCOME" "TARGET" "WHAT / WHY"))
       (insert (make-string 100 ?-) "\n")
       (dolist (brief-item (sort (copy-sequence items)
                                 (lambda (a b)
                                   (string> (format "%s" (or (plist-get a :queued-at) ""))
                                            (format "%s" (or (plist-get b :queued-at) ""))))))
         (arxana-field-desk--insert-item-row brief-item reviews))
       (unless items (insert "No items in this stratum.\n"))
       (insert "\nRET=open feature-acceptance sheet  g=refresh  q=quit\n"))
     (let ((copy (copy-sequence item)))
       (lambda () (arxana-field-desk-open-stratum copy))))))

(defun arxana-field-desk-open-item (item)
  "Open the feature-acceptance sheet for ITEM."
  (let* ((brief-item (or (plist-get item :item) item))
         (attempt-id (plist-get brief-item :attempt-id)))
    (arxana-field-desk--render-frame
     (lambda () (arxana-field-desk--insert-sheet
                 brief-item (arxana-field-desk--reviews)))
     (lambda ()
       (let ((fresh (cl-find attempt-id (arxana-field-desk--items)
                             :key (lambda (candidate)
                                    (plist-get candidate :attempt-id))
                             :test #'equal)))
         (when fresh (arxana-field-desk-open-item fresh))))
     brief-item)))

;; ------------------------------------------------------------- actions ----

(defun arxana-field-desk--prop-at-point (property)
  (or (get-text-property (point) property)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) property))))

(defun arxana-field-desk-open-at-point ()
  "Open the item at point, or invoke a button."
  (interactive)
  (cond ((button-at (point)) (push-button))
        ((arxana-field-desk--prop-at-point 'arxana-field-desk-item)
         (arxana-field-desk-open-item
          (arxana-field-desk--prop-at-point 'arxana-field-desk-item)))
        (t (user-error "No Field Desk item at point"))))

(defun arxana-field-desk--review-payload
    (attempt objective answer note reviewer)
  "Build the JSON-ready review payload for ATTEMPT."
  `(("attempt-id" . ,attempt)
    ("objective" . ,(arxana-field-desk--keyword-name objective))
    ("answer" . ,(arxana-field-desk--keyword-name answer))
    ("note" . ,note)
    ("reviewer" . ,reviewer)))

(defun arxana-field-desk--review-url ()
  (concat (string-remove-suffix "/" arxana-field-desk-endpoint)
          "/api/alpha/morning-brief/review"))

(defun arxana-field-desk--addendum-payload
    (attempt kind title body author)
  "Build the JSON-ready notebook payload for ATTEMPT."
  `(("attempt-id" . ,attempt)
    ("kind" . ,(arxana-field-desk--keyword-name kind))
    ("title" . ,title)
    ("body" . ,body)
    ("author" . ,author)))

(defun arxana-field-desk--addendum-url ()
  (concat (string-remove-suffix "/" arxana-field-desk-endpoint)
          "/api/alpha/morning-brief/addendum"))

(defun arxana-field-desk--record-submit-error (summary detail)
  (let ((buffer (get-buffer-create arxana-field-desk--errors-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert summary "\n\n" (or detail "No response body was returned.") "\n")))
    (display-buffer buffer)
    (message "%s; the serving JVM may be down (see %s)"
             summary arxana-field-desk--errors-buffer)))

(defun arxana-field-desk--response-body ()
  (save-excursion
    (goto-char (or (and (boundp 'url-http-end-of-headers)
                        url-http-end-of-headers)
                   (point-min)))
    (when (looking-at-p "\r?\n") (forward-line 1))
    (string-trim (buffer-substring-no-properties (point) (point-max)))))

(defun arxana-field-desk--submit-callback (status origin objective attempt)
  (let ((response-buffer (current-buffer)))
    (unwind-protect
        (let ((network-error (plist-get status :error))
              (http-status (and (boundp 'url-http-response-status)
                                url-http-response-status))
              (body (arxana-field-desk--response-body)))
          (if (and (not network-error) http-status
                   (<= 200 http-status) (< http-status 300))
              (progn
                (message "Field Desk recorded %s for %s" objective attempt)
                (when (buffer-live-p origin)
                  (with-current-buffer origin
                    (when arxana-field-desk--refresh-fn
                      (funcall arxana-field-desk--refresh-fn)))))
            (arxana-field-desk--record-submit-error
             (if network-error
                 (format "Field Desk could not reach %s"
                         arxana-field-desk-endpoint)
               (format "Field Desk review failed with HTTP %s"
                       (or http-status "unknown")))
             (if network-error (format "%S" network-error) body))))
      (when (buffer-live-p response-buffer) (kill-buffer response-buffer)))))

(defun arxana-field-desk--submit (item objective answer note)
  (let* ((attempt (plist-get item :attempt-id))
         (origin (current-buffer))
         (payload (arxana-field-desk--review-payload
                   attempt objective answer note arxana-field-desk-reviewer))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
    (condition-case err
        (url-retrieve (arxana-field-desk--review-url)
                      #'arxana-field-desk--submit-callback
                      (list origin objective attempt) t t)
      (error
       (arxana-field-desk--record-submit-error
        (format "Field Desk could not reach %s" arxana-field-desk-endpoint)
        (error-message-string err))))))

(defvar-local arxana-field-desk--compose-item nil)
(defvar-local arxana-field-desk--compose-kind nil)
(defvar-local arxana-field-desk--compose-title nil)
(defvar-local arxana-field-desk--compose-origin nil)

(defun arxana-field-desk--addendum-submit-callback
    (status compose-buffer origin attempt)
  (let ((response-buffer (current-buffer)))
    (unwind-protect
        (let ((network-error (plist-get status :error))
              (http-status (and (boundp 'url-http-response-status)
                                url-http-response-status))
              (body (arxana-field-desk--response-body)))
          (if (and (not network-error) http-status
                   (<= 200 http-status) (< http-status 300))
              (progn
                (when (buffer-live-p compose-buffer) (kill-buffer compose-buffer))
                (when (buffer-live-p origin)
                  (with-current-buffer origin
                    (when arxana-field-desk--refresh-fn
                      (funcall arxana-field-desk--refresh-fn))))
                (message "Field Desk added notebook entry for %s" attempt))
            (arxana-field-desk--record-submit-error
             (if network-error
                 (format "Field Desk could not reach %s"
                         arxana-field-desk-endpoint)
               (format "Field Desk addendum failed with HTTP %s"
                       (or http-status "unknown")))
             (if network-error (format "%S" network-error) body))))
      (when (buffer-live-p response-buffer) (kill-buffer response-buffer)))))

(defun arxana-field-desk-submit-addendum ()
  "Submit the current addendum composition asynchronously."
  (interactive)
  (unless arxana-field-desk--compose-item
    (user-error "This is not a Field Desk addendum buffer"))
  (let* ((compose-buffer (current-buffer))
         (attempt (plist-get arxana-field-desk--compose-item :attempt-id))
         (body (buffer-substring-no-properties (point-min) (point-max)))
         (payload (arxana-field-desk--addendum-payload
                   attempt arxana-field-desk--compose-kind
                   arxana-field-desk--compose-title body
                   arxana-field-desk-reviewer))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
    (when (string-empty-p (string-trim body))
      (user-error "A non-blank body is required"))
    (condition-case err
        (url-retrieve (arxana-field-desk--addendum-url)
                      #'arxana-field-desk--addendum-submit-callback
                      (list compose-buffer arxana-field-desk--compose-origin attempt)
                      t t)
      (error
       (arxana-field-desk--record-submit-error
        (format "Field Desk could not reach %s" arxana-field-desk-endpoint)
        (error-message-string err))))))

(defun arxana-field-desk-cancel-addendum ()
  "Cancel the current addendum composition."
  (interactive)
  (kill-buffer (current-buffer)))

(defun arxana-field-desk-compose-addendum ()
  "Compose a notebook addendum for the current attempt sheet."
  (interactive)
  (unless arxana-field-desk--current-item
    (user-error "Open a Field Desk item first"))
  (let* ((item arxana-field-desk--current-item)
         (attempt (plist-get item :attempt-id))
         (origin (current-buffer))
         (kind-name (completing-read "Kind: " '("repro" "why-built" "note")
                                     nil t))
         (kind (intern (concat ":" kind-name)))
         (title (string-trim (read-string "Title: "))))
    (when (string-empty-p title) (user-error "A non-blank title is required"))
    (let ((buffer (get-buffer-create
                   (format "*Field Desk Addendum: %s*" attempt))))
      (pop-to-buffer buffer)
      (text-mode)
      (erase-buffer)
      (setq-local arxana-field-desk--compose-item item
                  arxana-field-desk--compose-kind kind
                  arxana-field-desk--compose-title title
                  arxana-field-desk--compose-origin origin
                  header-line-format "C-c C-c submit, C-c C-k cancel")
      (local-set-key (kbd "C-c C-c") #'arxana-field-desk-submit-addendum)
      (local-set-key (kbd "C-c C-k") #'arxana-field-desk-cancel-addendum))))

(defun arxana-field-desk--answer (item objective)
  (unless (memq objective (arxana-field-desk--item-objectives item))
    (user-error "%s does not apply to %s" objective (plist-get item :attempt-id)))
  (when (arxana-field-desk--review-for item objective
                                       (arxana-field-desk--reviews))
    (user-error "%s is already answered for %s"
                objective (plist-get item :attempt-id)))
  (let* ((spec (arxana-field-desk--objective-spec objective))
         (answers (mapcar #'arxana-field-desk--keyword-name
                          (plist-get spec :answers)))
         (answer-name (completing-read
                       (concat (plist-get spec :question) " ") answers nil t))
         (note (string-trim (read-string "Required note: "))))
    (when (string-empty-p note) (user-error "A non-blank note is required"))
    (arxana-field-desk--submit item objective (intern (concat ":" answer-name)) note)))

(defun arxana-field-desk-answer-at-point ()
  "Answer the objective row at point."
  (interactive)
  (let ((item (or (arxana-field-desk--prop-at-point 'arxana-field-desk-item)
                  arxana-field-desk--current-item))
        (objective (arxana-field-desk--prop-at-point
                    'arxana-field-desk-objective)))
    (unless (and item objective) (user-error "No objective row at point"))
    (arxana-field-desk--answer item objective)))

(defun arxana-field-desk-feature-verdict ()
  "Answer the feature-verdict objective for the current sheet."
  (interactive)
  (unless arxana-field-desk--current-item
    (user-error "Open a Field Desk item first"))
  (arxana-field-desk--answer arxana-field-desk--current-item :feature-verdict))

(defun arxana-field-desk-open-mission ()
  "Open the current item's mission file."
  (interactive)
  (let ((path (and arxana-field-desk--current-item
                   (arxana-field-desk--mission-path
                    arxana-field-desk--current-item))))
    (unless (and path (file-readable-p path))
      (user-error "No readable mission path on this item"))
    (find-file path)))

(defun arxana-field-desk-copy-commit ()
  "Copy the current item's commit SHA."
  (interactive)
  (let ((commit (and arxana-field-desk--current-item
                     (arxana-field-desk--commit
                      arxana-field-desk--current-item))))
    (unless commit (user-error "No commit SHA on this item"))
    (kill-new commit)
    (message "Copied %s" commit)))

(defun arxana-field-desk-refresh ()
  "Re-read the append-only store and re-render the current frame."
  (interactive)
  (if arxana-field-desk--refresh-fn
      (funcall arxana-field-desk--refresh-fn)
    (arxana-field-desk)))

(defun arxana-field-desk-help ()
  "Show Field Desk key help."
  (interactive)
  (with-help-window "*Arxana Field Desk Help*"
    (princ "Arxana Field Desk — feature acceptance\n\n")
    (princ "RET  open item or stratum\n")
    (princ "a    answer objective at point\n")
    (princ "v    answer feature verdict from anywhere in a sheet\n")
    (princ "n    add a reproducibility or rationale notebook entry\n")
    (princ "o    open mission file\n")
    (princ "c    copy commit SHA\n")
    (princ "g    refresh from append-only EDN store\n")
    (princ "q    quit window\n")
    (princ "?    this help\n")))

;; ---------------------------------------------------- home integration ----

(defun arxana-field-desk--home-items ()
  "Return pending, partial, and full Field Desk stratum rows."
  (let ((items (arxana-field-desk--items))
        (reviews (arxana-field-desk--reviews)))
    (mapcar
     (lambda (spec)
       (let ((count (length (arxana-field-desk--items-at
                             (nth 0 spec) items reviews))))
         (list :type 'field-desk-stratum
               :label (nth 1 spec)
               :stratum (nth 0 spec)
               :description (format "%d item(s) — %s" count (nth 2 spec)))))
     arxana-field-desk--strata)))

(provide 'arxana-field-desk)
;;; arxana-field-desk.el ends here
