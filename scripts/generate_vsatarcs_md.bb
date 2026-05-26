#!/usr/bin/env bb
;; generate_vsatarcs_md.bb — Linearised distillation of the VSATARCS
;; anthology with marginal alignment annotations sourced from the
;; v0.5.x .aif.edn closures + r-criterion-audit + bilateral-evidence.
;;
;; Usage:
;;   bb scripts/generate_vsatarcs_md.bb [--out PATH] [--stories DIR]
;;
;; Defaults:
;;   --out      ~/code/futon4/docs/VSATARCS.md
;;   --stories  ~/code/futon5a/holes/stories/
;;
;; Authored 2026-05-19 per Joe's pivot to reader-facing distillation;
;; recommended as v1 canonical artifact, PDF via pandoc downstream.
;;
;; Story ordering (hybrid):
;;   1. leaf-start-here.md first (per the VSATARCS landing convention)
;;   2. Then any story file referenced by a lifted :sections[] entry in
;;      ~/code/futon5a/holes/stack-annotations.edn, in :sections[] order
;;   3. Then alphabetically-remaining story files
;;   *.aif.md companions excluded throughout.
;;
;; Marginal annotation sources:
;;   - r-criterion-audit (current R-criteria status)
;;   - bilateral-evidence (cross-side correspondences)
;;   - closure annotations (where they reference story content via the
;;     stack-annotations.edn :sections graph)
;;   - revisions log (date-stamped milestones)
;;
;; The document is regenerated on demand.  Future R-criterion
;; advancements surface automatically as the .aif.edn grows.

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.set]
         '[clojure.string :as str]
         '[babashka.fs :as fs])

(def STORIES-DIR (str (fs/expand-home "~/code/futon5a/holes/stories")))
(def STACK-ANN-PATH (str (fs/expand-home "~/code/futon5a/holes/stack-annotations.edn")))
(def AIF-EDN-PATH (str (fs/expand-home "~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn")))
(def PROSE-PATH (str (fs/expand-home "~/code/futon4/docs/vsatarcs-r-criteria-in-prose.md")))
(def READER-PROSE-PATH (str (fs/expand-home "~/code/futon4/docs/vsatarcs-reader-criteria-in-prose.md")))
(def OUT-PATH (str (fs/expand-home "~/code/futon4/docs/VSATARCS.md")))
(def HTML-OUT-PATH (str (fs/expand-home "~/code/futon7a/vsatarcs.html")))

(def cli-args (vec *command-line-args*))

(defn arg-after [flag]
  (when-let [i (some #(when (= flag (nth cli-args % nil)) %)
                     (range (count cli-args)))]
    (nth cli-args (inc i) nil)))

(def opts {:out (or (arg-after "--out") OUT-PATH)
           :html-out (or (arg-after "--html-out") HTML-OUT-PATH)
           :stories (or (arg-after "--stories") STORIES-DIR)
           :stack-ann STACK-ANN-PATH
           :aif AIF-EDN-PATH})

;; ---------- read sources ----------

(defn read-edn-file [path]
  (with-open [r (io/reader path)]
    (edn/read {:readers {'inst (fn [s] s)}    ; tagged literals tolerated
               :default (fn [_tag form] form)}
              (java.io.PushbackReader. r))))

(def stack-ann (read-edn-file (:stack-ann opts)))
(def aif (read-edn-file (:aif opts)))

(def sections (vec (:sections stack-ann)))
(def r-audit (:r-criterion-audit aif))
(def reader-audit (:reader-criterion-audit aif))
(def bilateral-evidence (vec (:bilateral-evidence aif)))
(def revisions (vec (get-in aif [:provenance :revisions])))
(def closures (filter :closure (:annotations aif)))

;; ---------- story listing + ordering ----------

(defn story-files [dir]
  (->> (fs/list-dir dir)
       (map str)
       (filter #(str/ends-with? % ".md"))
       (remove #(str/ends-with? % ".aif.md"))
       sort
       vec))

(defn basename [path] (fs/file-name path))

(defn section-for-story-basename [base]
  (some (fn [s]
          (when-let [ref (:ref s)]
            (when (= base (fs/file-name ref))
              s)))
        sections))

(defn ordered-story-files [all-files]
  (let [landing (str (fs/path (:stories opts) "leaf-start-here.md"))
        landing? (fn [f] (= (basename f) "leaf-start-here.md"))
        ;; Lifted: files referenced by stack-annotations :sections[]
        lifted-paths (set
                      (keep (fn [s]
                              (when-let [ref (:ref s)]
                                (let [p (str (fs/path (:stories opts)
                                                      (fs/file-name ref)))]
                                  (when (some #{p} all-files) p))))
                            sections))
        landing-files (filter landing? all-files)
        lifted-files (filter #(and (lifted-paths %)
                                    (not (landing? %)))
                              all-files)
        unlifted-files (->> all-files
                            (remove landing?)
                            (remove lifted-paths))]
    (concat landing-files lifted-files unlifted-files)))

;; ---------- parse story files ----------

(defn parse-story [path]
  (let [text (slurp path)
        lines (str/split-lines text)
        title (some (fn [l]
                      (when-let [m (re-matches #"^#\s+(.+?)\s*$" l)]
                        (second m)))
                    (take 5 lines))
        scenes (atom [])
        current (atom nil)
        body-lines (atom [])
        flush! (fn []
                 (when @current
                   (swap! scenes conj (assoc @current
                                             :body (str/trim
                                                    (str/join "\n" @body-lines))))
                   (reset! body-lines [])))]
    (doseq [l lines]
      (if-let [m (re-matches #"^##+\s+Scene:\s+(.+?)(?:\s+\|\s*([A-Za-z0-9 -]+))?\s*$" l)]
        (do (flush!)
            (reset! current {:title (str/trim (second m))
                             :anchor (when (nth m 2) (str/trim (nth m 2)))}))
        (when @current (swap! body-lines conj l))))
    (flush!)
    {:title (or title (str/replace (basename path) #"\.md$" ""))
     :path path
     :basename (basename path)
     :scenes @scenes}))

;; ---------- alignment annotation rendering ----------

(defn r-status-summary []
  (let [order [:R1 :R2 :R3 :R4 :R5 :R6 :R7 :R8 :R9 :R10 :R11 :R12]
        rows (for [r order
                   :let [entry (get r-audit r)
                         status (:status entry)]]
               (format "- **%s** — %s"
                       (name r)
                       (or status ":not-recorded")))]
    (str/join "\n" rows)))

(defn reader-status-summary []
  (let [order [:Q1 :Q2 :Q3 :Q4 :Q5 :Q6 :Q7 :Q8]
        rows (for [q order
                   :let [entry (get reader-audit q)
                         status (:status entry)
                         tests (:tests entry)
                         question (:question entry)]]
               (format "- **%s** — %s%s%s"
                       (name q)
                       (or status ":not-recorded")
                       (if (seq tests)
                         (str " [" (str/join " " (map name tests)) "]")
                         "")
                       (if question (str " — " question) "")))]
    (str/join "\n" rows)))

(defn bilateral-evidence-summary []
  (str/join "\n"
            (for [[i e] (map-indexed vector bilateral-evidence)]
              (format "%d. **%s** — `%s` ↔ `%s` (landed %s)"
                      (inc i)
                      (or (:principle e) "?")
                      (or (:vsatarcs-id e) "?")
                      (or (:wm-id e) "?")
                      (or (:landed e) "?")))))

(defn revisions-summary []
  (str/join "\n"
            (for [r revisions]
              (format "- **%s** (%s) — %s"
                      (or (:rev r) "?")
                      (or (:on r) "?")
                      (let [s (or (:summary r) "")]
                        (if (> (count s) 200)
                          (str (subs s 0 197) "...")
                          s))))))

(defn alignment-callout-for-story [section]
  (if (nil? section)
    (str
     "> **Alignment status:** this story is not yet lifted into the "
     "canonical `stack-annotations.edn` `:sections[]`.  When lifted, "
     "it will gain an entity-id (`arxana/stack/futon-v1/...`) and "
     "the R-criteria satisfactions named in the audit row will apply "
     "to it directly.")
    (let [eid (:id section)
          kind (:kind section)
          name (:name section)]
      (str/join "\n"
                ["> **Alignment status — lifted entity** "
                 (format "> `%s` (kind `%s`)" eid kind)
                 ">"
                 "> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:"
                 "> "
                 (format "> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)")
                 "> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)"
                 "> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape"
                 "> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store"
                 "> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity"
                 "> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth"
                 ">"
                 "> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires)."]))))

;; ---------- emit ----------

(defn header []
  (let [ts (str (java.time.Instant/now))]
    (str
     "# VSATARCS — Linearised Anthology with Alignment Annotations\n\n"
     "*Live-regenerated distillation of the VSATARCS scene-form anthology at "
     "`~/code/futon5a/holes/stories/`, annotated with the current alignment "
     "state per `~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn`.*\n\n"
     "**Regenerated:** " ts "\n\n"
     "**Regenerate:** `bb ~/code/futon4/scripts/generate_vsatarcs_md.bb`\n\n"
     "**PDF:** `pandoc " (:out opts) " -o ~/code/futon4/docs/VSATARCS.pdf "
     "-V geometry:margin=1in`\n\n"
     "---\n\n"
     "## R-criterion audit (VSATARCS-side, current state)\n\n"
     (r-status-summary)
     "\n\n"
     ;; Inline the R-criteria-in-prose file contents (paragraph-by-paragraph
     ;; explanation; the long-form counterpart to the keyword audit above)
     (if (fs/exists? PROSE-PATH)
       (str (slurp PROSE-PATH) "\n\n")
       "")
     "## Reader-criterion audit (Q1-Q8; V-CUR / V-COV / V-COM / V-BIL)\n\n"
     "*Parallel contract from `~/code/futon2/docs/vsatarcs-reader-criteria.md` "
     "grading VSATARCS as a **reader surface** — distinct from the R-criteria "
     "above which grade it as an AIF agent.*\n\n"
     (reader-status-summary)
     "\n\n"
     ;; Inline the reader-criteria-in-prose file (paragraph-by-paragraph
     ;; explanation of Q1-Q8; long-form counterpart to the keyword audit)
     (if (fs/exists? READER-PROSE-PATH)
       (str (slurp READER-PROSE-PATH) "\n\n")
       "")
     "## Bilateral evidence (cross-side correspondences)\n\n"
     (bilateral-evidence-summary)
     "\n\n"
     "## Revisions trail\n\n"
     (revisions-summary)
     "\n\n"
     "## How to read this document\n\n"
     "Each story below opens with a **blockquote alignment callout** "
     "naming whether it's lifted into the canonical hypergraph and "
     "(if so) which R-criteria currently apply to its entity domain.  "
     "Marginal alignment annotations are inline blockquotes because "
     "Markdown lacks true margins; PDF generation via pandoc can "
     "render these into wide-margin marginalia if desired (`pandoc "
     "... --variable=geometry:margin=2.5in`).\n\n"
     "**Future integration points** named in closure `:enables` blocks "
     "(R5 preferences; multi_watcher full tap; bridge precision/F "
     "trajectory comparison) will surface here automatically as new "
     "closures land in the `.aif.edn`.\n\n"
     "---\n")))

(defn render-story [path]
  (let [story (parse-story path)
        section (section-for-story-basename (basename path))]
    (str
     "## " (:title story) "\n\n"
     "*Source: `" (str/replace path
                               (str (fs/expand-home "~"))
                               "~")
     "`*\n\n"
     (alignment-callout-for-story section)
     "\n\n"
     (str/join "\n\n"
               (for [s (:scenes story)]
                 (str "### "
                      (:title s)
                      (when-let [a (:anchor s)] (str " | " a))
                      "\n\n"
                      (:body s))))
     "\n\n---\n\n")))

;; ============================================================
;; HTML emission (Tufte CSS marginnotes; sibling artefact at
;; ~/code/futon7a/vsatarcs.html for the public-facing surface)
;; ============================================================

(defn html-escape [s]
  (-> (or s "")
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

;; Story-context-aware link resolution.
;;
;; A Markdown link `[text](target)' inside a story body can mean:
;;   1. Scene anchor within the SAME story  → #<story-stem>-<target>
;;   2. Another story (target = story basename) → #<target>  (jump to story h2)
;;   3. Scene in another story (when target matches an anchor that's
;;      unambiguously in exactly one other story)  → #<other-stem>-<target>
;;   4. External URL or unresolved → keep as-is
(defn story-stem [path]
  (str/replace (basename path) #"\.md$" ""))

(def story-stems-set (atom #{}))
(def scenes-by-stem (atom {}))     ; {stem #{anchor anchor ...}}
(def current-story-stem (atom nil))

(defn build-anchor-index [ordered-paths]
  (let [stems (set (map story-stem ordered-paths))
        per-story (into {}
                        (for [p ordered-paths]
                          [(story-stem p)
                           (set (keep :anchor (:scenes (parse-story p))))]))]
    (reset! story-stems-set stems)
    (reset! scenes-by-stem per-story)))

(defn scene-anchor-host [anchor]
  "Return the unique story-stem hosting ANCHOR, or nil if 0 or >1
   stories own it."
  (let [owners (for [[stem scenes] @scenes-by-stem
                     :when (contains? scenes anchor)]
                 stem)]
    (when (= 1 (count owners)) (first owners))))

(defn resolve-link-href [target]
  "Resolve a Markdown link TARGET using the current-story-stem
   context.  Same-story scene anchors take precedence over global
   matches.  Targets ending in `.md' are treated as story basenames
   with the suffix stripped."
  (let [stripped (str/replace target #"\.md$" "")]
    (cond
      (or (str/starts-with? target "http://")
          (str/starts-with? target "https://")
          (str/starts-with? target "mailto:")
          (str/starts-with? target "/"))
      target

      ;; (1) Scene anchor in the current story
      (and @current-story-stem
           (contains? (get @scenes-by-stem @current-story-stem #{}) target))
      (str "#" @current-story-stem "-" target)

      ;; (2) Story basename (with or without `.md' suffix)
      (contains? @story-stems-set stripped)
      (str "#" stripped)

      :else
      ;; (3) Unique scene anchor in some other story → host; else keep target.
      (if-let [host (scene-anchor-host target)]
        (str "#" host "-" target)
        target))))

(defn- smart-typography
  "Apply typographic substitutions: straight quotes → curly quotes,
   triple-dot → ellipsis, internal apostrophe → curly. Conservative
   regex-based heuristic: open double quote after whitespace/start/
   punct, close double quote elsewhere; same for single. Internal
   apostrophe between word chars becomes ’. Caveat: quotes inside
   code spans (between backticks) are also smart-quoted; avoid `\"`
   inside `…` if a literal straight quote matters."
  [s]
  (-> (or s "")
      ;; … (triple-dot → ellipsis)
      (str/replace #"\.\.\." "…")
      ;; word'word → word’word (apostrophe inside word)
      (str/replace #"(\w)'(\w)" "$1’$2")
      ;; open double quote: at start, after whitespace, or after open-punct
      (str/replace #"(^|\s|[(\[{>])\"" "$1“")
      ;; remaining " → ” (close double quote)
      (str/replace #"\"" "”")
      ;; open single quote
      (str/replace #"(^|\s|[(\[{>])'" "$1‘")
      ;; remaining ' → ’ (close single quote)
      (str/replace #"'" "’")))

(defn md-inline-to-html [s]
  "Convert minimal Markdown inline syntax to HTML.  Order matters:
   smart-typography (curly quotes etc.) → code → links → bold → italic.
   Preserves already-HTML content. Resolves in-document links via
   anchor-set."
  (-> (or s "")
      ;; Smart-typography pass first — runs on raw prose before any
      ;; markdown markers are converted, so the HTML tags introduced
      ;; below still use plain ASCII quotes for attributes.
      smart-typography
      ;; `code`
      (str/replace #"`([^`]+)`"
                   (fn [m] (str "<code>" (html-escape (nth m 1)) "</code>")))
      ;; [text](url)
      (str/replace #"\[([^\]]+)\]\(([^)]+)\)"
                   (fn [m]
                     (str "<a href=\""
                          (html-escape (resolve-link-href (nth m 2)))
                          "\">"
                          (html-escape (nth m 1))
                          "</a>")))
      ;; **bold**  (inner content already passed through code/link
      ;; substitution; do not re-escape, or `<code>` becomes `&lt;code&gt;`)
      (str/replace #"\*\*([^*]+)\*\*"
                   (fn [m] (str "<strong>" (nth m 1) "</strong>")))
      ;; *italic*  (after **bold**; same no-re-escape rationale)
      (str/replace #"\*([^*]+)\*"
                   (fn [m] (str "<em>" (nth m 1) "</em>")))))

(defn md-paragraphs-to-html [text]
  "Split prose TEXT on blank-line boundaries into <p> blocks.
   Single-line elements that look like blockquote / list items are
   passed through best-effort."
  (let [paras (str/split (or text "") #"\n\n+")]
    (str/join "\n"
              (for [p paras
                    :let [p (str/trim p)]
                    :when (seq p)]
                (cond
                  ;; Markdown list (starts with `- `)
                  (re-find #"(?m)^\s*-\s+" p)
                  (let [items (->> (str/split-lines p)
                                   (filter #(re-find #"^\s*-\s+" %))
                                   (map #(str "<li>"
                                              (md-inline-to-html
                                               (str/replace % #"^\s*-\s+" ""))
                                              "</li>")))]
                    (str "<ul>" (str/join "" items) "</ul>"))
                  ;; Markdown blockquote
                  (re-find #"(?m)^>\s*" p)
                  (let [body (->> (str/split-lines p)
                                  (map #(str/replace % #"^>\s?" ""))
                                  (str/join " "))]
                    (str "<blockquote><p>" (md-inline-to-html body)
                         "</p></blockquote>"))
                  :else
                  (str "<p>" (md-inline-to-html p) "</p>"))))))

(def html-mn-counter (atom 0))

(defn next-mn-id [stem]
  (format "mn-vsat-%s-%d" stem (swap! html-mn-counter inc)))

;; ---------------------------------------------------------------------
;; Substrate-signal indices for stub-lifted marginpars (v0.5.33 upgrade).
;; Precomputed once at first marginpar emission so per-stem lookup is O(1).
;; ---------------------------------------------------------------------

(def stub-substrate-index (atom nil))

(defn- compute-stub-substrate-index! []
  "Build {:inbound-citations {stem #{citing-stem ...}}
          :issue-queue-by-stem {stem [{:id ... :title ... :priority ... :status ...} ...]}}.

   Inbound citations: for each story, scan its text for occurrences of OTHER
   stems (word-boundary match) and record the cite-edge.  Cheap O(n × |text|)
   one-shot — runs once per regen.

   Issue-queue-by-stem: walks (:issue-queue aif); any entry whose :pointers
   strings contain a story-stem records the entry under that stem."
  (let [all-paths (story-files (:stories opts))
        stems (mapv story-stem all-paths)
        stem->path (zipmap stems all-paths)
        ;; Inbound citations: for each citing-stem, see which other stems appear in its text
        cite-edges (for [citing-stem stems
                         :let [text (slurp (stem->path citing-stem))]
                         target-stem stems
                         :when (and (not= citing-stem target-stem)
                                    ;; word-boundary match on the stem string
                                    (re-find (re-pattern (str "\\b"
                                                              (java.util.regex.Pattern/quote target-stem)
                                                              "\\b"))
                                             text))]
                     [target-stem citing-stem])
        inbound (reduce (fn [m [target citing]]
                          (update m target (fnil conj #{}) citing))
                        {} cite-edges)
        ;; Issue-queue by stem
        iq-entries (vec (:issue-queue aif))
        iq-by-stem (reduce
                    (fn [acc stem]
                      (let [matching
                            (filter
                             (fn [entry]
                               (some #(and (string? %)
                                           (str/includes? % stem))
                                     (:pointers entry)))
                             iq-entries)]
                        (if (seq matching)
                          (assoc acc stem
                                 (mapv #(select-keys % [:id :title :priority :status])
                                       matching))
                          acc)))
                    {} stems)]
    {:inbound-citations inbound
     :issue-queue-by-stem iq-by-stem}))

(defn- substrate-index []
  (or @stub-substrate-index
      (reset! stub-substrate-index (compute-stub-substrate-index!))))

(defn- days-since-file-mtime [path]
  (when (fs/exists? path)
    (let [mtime-millis (.toMillis (fs/last-modified-time path))
          now-millis (System/currentTimeMillis)]
      (/ (- now-millis mtime-millis) (* 1000.0 60 60 24)))))

(defn- file-size-bytes [path]
  (when (fs/exists? path)
    (.length (io/file path))))

(defn- stub-substrate-signals-html [stem]
  "Return a small HTML fragment summarising the substrate signals for
   STEM, used in v0.5.33's upgraded stub-lifted marginpar branch."
  (let [story-path (str (fs/path (:stories opts) (str stem ".md")))
        days (days-since-file-mtime story-path)
        size (file-size-bytes story-path)
        idx (substrate-index)
        inbound (get-in idx [:inbound-citations stem] #{})
        iq-entries (get-in idx [:issue-queue-by-stem stem] [])]
    (str
     "<strong>Substrate signals</strong><br>"
     "<ul style=\"margin-top:0.3em;margin-bottom:0.3em;font-size:0.85em\">"
     ;; Freshness + depth
     (str "<li>Story: <code>" (html-escape stem) ".md</code>"
          (when size (str ", " size " bytes"))
          (when days
            (str ", touched "
                 (if (< days 1) "today"
                     (format "%.0f day%s ago"
                             (Math/floor days)
                             (if (< days 2) "" "s")))))
          "</li>")
     ;; Inbound citations
     (let [n (count inbound)]
       (str "<li>Inbound citations: "
            (if (zero? n)
              "<em>none</em>"
              (str n " other "
                   (if (= 1 n) "story references" "stories reference")
                   " this stem"
                   (when (<= n 3)
                     (str " — "
                          (str/join ", "
                                    (map #(str "<code>" (html-escape %) "</code>")
                                         (sort inbound)))))))
            "</li>"))
     ;; Issue-queue
     (if (seq iq-entries)
       (str "<li>Issue-queue: "
            (str/join ", "
                      (for [e iq-entries]
                        (str "<code>" (html-escape (:id e)) "</code> "
                             "(" (html-escape (str (:priority e))) " "
                             (html-escape (str (:status e))) ")")))
            "</li>")
       "<li>Issue-queue: <em>no entries point at this stem</em></li>")
     "</ul>")))

(defn alignment-marginnote-html [stem section]
  "Build the Tufte marginnote HTML for a story's alignment callout.

   Three states:
     - section nil          → not yet lifted into :sections[] at all.
     - section :stub-no-aif → registered but decomposition pending (no .aif.edn).
     - section :composite-top-level → full lift with decomposition.

   v0.5.33 upgrade (Joe directive 2026-05-21): stub-lifted branch now
   carries substrate-signal report (story mtime + size, inbound citation
   count + names, issue-queue entries pointing at the stem) instead of
   discharge-procedure boilerplate.  The discharge line is preserved as
   a footer."
  (let [id (next-mn-id stem)
        lifted-from (some-> section :provenance :lifted-from)
        is-stub? (= lifted-from :stub-no-aif)
        decomposes (:decomposes-into section)
        content
        (cond
          (nil? section)
          (str "<strong>Alignment status:</strong> not yet lifted "
               "into the canonical "
               "<code>stack-annotations.edn</code> "
               "<code>:sections[]</code>.  When lifted, it gains "
               "an entity-id and the active R-criteria apply directly.")
          is-stub?
          (str "<strong>Alignment status — stub-lifted</strong><br>"
               "<code>" (html-escape (str (:id section))) "</code> "
               "(kind <code>" (html-escape (str (:kind section))) "</code>)<br><br>"
               (stub-substrate-signals-html stem)
               "<small><em>Decomposition pending</em> — "
               "tracked by <code>:sorry/stub-lifts-pending-aif-edn</code>; "
               "classified <code>:prototyping-forward</code>.  Discharge by "
               "authoring <code>"
               (html-escape (str (story-stem (:ref section)) ".aif.edn"))
               "</code> and re-running "
               "<code>lift_unlifted_stories.bb</code>.</small>")
          :else
          (str "<strong>Alignment status — lifted entity</strong><br>"
               "<code>" (html-escape (str (:id section))) "</code> "
               "(kind <code>" (html-escape (str (:kind section))) "</code>)<br><br>"
               "<strong>Decomposes into " (count decomposes)
               (if (= 1 (count decomposes)) " entity" " entities")
               "</strong>"
               (if (seq decomposes)
                 (str ":<br><ul style=\"margin-top:0.3em;font-size:0.85em\">"
                      (str/join ""
                                (for [c (take 6 decomposes)]
                                  (str "<li><code>"
                                       (html-escape
                                        (str/replace c (str (:id section) "/") ""))
                                       "</code></li>")))
                      (when (> (count decomposes) 6)
                        (str "<li><em>… and " (- (count decomposes) 6)
                             " more</em></li>"))
                      "</ul>")
                 ".")
               "<br><strong>Active R-criteria:</strong><br>"
               "<strong>R1</strong> ✓ belief posterior + bridge fetch<br>"
               "<strong>R2</strong> ✓ 5-channel observation schema<br>"
               "<strong>R3</strong> ✓ predictive-coding belief update<br>"
               "<strong>R7</strong> ✓ adaptive precision (rolling window)<br>"
               "<strong>R8</strong> ✓ per-tick trace<br>"
               "<strong>R10</strong> ✓ file-notify wakeup<br><br>"
               "<em>Pending:</em> R5 preferences; R4/R5/R6 writer-capability."))]
    (str "<label for=\"" id "\" class=\"margin-toggle\">&#8853;</label>"
         "<input type=\"checkbox\" id=\"" id "\" class=\"margin-toggle\"/>"
         "<span class=\"marginnote\">" content "</span>")))

(defn html-header []
  (let [ts (str (java.time.Instant/now))]
    (str
     "<!DOCTYPE html>\n"
     "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">\n"
     "<head>\n"
     "<link rel=icon href=\"favicon.png\">\n"
     "<title>VSATARCS — Linearised Anthology with Alignment Annotations</title>\n"
     "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>\n"
     "<link rel=\"stylesheet\" href=\"tufte.css\"/>\n"
     "<link rel=\"stylesheet\" href=\"latex.css\"/>\n"
     "</head>\n"
     "<body bgcolor=\"#F5F5F5\" vlink=\"blue\" link=\"blue\">\n"
     "<article>\n\n"
     "<h1>VSATARCS — Linearised Anthology</h1>\n"
     "<p class=\"subtitle\"><i>Live-regenerated distillation of the "
     "scene-form anthology at <code>~/code/futon5a/holes/stories/</code>, "
     "with margin annotations sourced from "
     "<code>vsatarcs-alignment-completeness.aif.edn</code>.</i></p>\n"
     "<p><small>Regenerated " ts ". "
     "Regenerate: <code>bb ~/code/futon4/scripts/generate_vsatarcs_md.bb</code>.</small></p>\n")))

(defn html-r-audit-section []
  (let [order [:R1 :R2 :R3 :R4 :R5 :R6 :R7 :R8 :R9 :R10 :R11 :R12]]
    (str
     "<section>\n<h2>R-criterion audit (VSATARCS-side, current state)</h2>\n"
     "<ul>\n"
     (str/join ""
               (for [r order
                     :let [entry (get r-audit r)
                           status (:status entry)]]
                 (format "<li><strong>%s</strong> — <code>%s</code></li>\n"
                         (name r) (or status ":not-recorded"))))
     "</ul>\n</section>\n")))

(defn html-reader-audit-section []
  (let [order [:Q1 :Q2 :Q3 :Q4 :Q5 :Q6 :Q7 :Q8]]
    (str
     "<section>\n<h2>Reader-criterion audit (Q1-Q8; V-CUR / V-COV / V-COM / V-BIL)</h2>\n"
     "<p><i>Parallel contract from "
     "<code>~/code/futon2/docs/vsatarcs-reader-criteria.md</code> "
     "grading VSATARCS as a reader surface — distinct from the "
     "R-criteria above which grade it as an AIF agent.</i></p>\n"
     "<ul>\n"
     (str/join ""
               (for [q order
                     :let [entry (get reader-audit q)
                           status (:status entry)
                           tests (:tests entry)
                           question (:question entry)]]
                 (format "<li><strong>%s</strong> — <code>%s</code>%s%s</li>\n"
                         (name q)
                         (or status ":not-recorded")
                         (if (seq tests)
                           (str " <small>["
                                (html-escape (str/join " " (map name tests)))
                                "]</small>")
                           "")
                         (if question
                           (str " — " (html-escape question))
                           ""))))
     "</ul>\n</section>\n")))

(defn html-bilateral-evidence-section []
  (str
   "<section>\n<h2>Bilateral evidence (cross-side correspondences)</h2>\n"
   "<ol>\n"
   (str/join ""
             (for [e bilateral-evidence]
               (format
                "<li><strong>%s</strong>: <code>%s</code> ↔ <code>%s</code> (landed %s)</li>\n"
                (html-escape (or (some-> (:principle e) name) "?"))
                (html-escape (or (:vsatarcs-id e) "?"))
                (html-escape (or (:wm-id e) "?"))
                (html-escape (or (:landed e) "?")))))
   "</ol>\n</section>\n"))

(defn html-revisions-section []
  (str
   "<section>\n<h2>Revisions trail</h2>\n"
   "<ul>\n"
   (str/join ""
             (for [r revisions]
               (format "<li><strong>%s</strong> (%s) — %s</li>\n"
                       (html-escape (or (:rev r) "?"))
                       (html-escape (or (:on r) "?"))
                       (let [s (html-escape (or (:summary r) ""))]
                         (if (> (count s) 240)
                           (str (subs s 0 237) "…")
                           s)))))
   "</ul>\n</section>\n"))

(defn html-reader-instructions []
  (str
   "<section>\n<h2>How to read this document</h2>\n"
   "<p>This document begins with an Operator's Foreword (Section 0) "
   "that frames the FUTON stack and futonic discipline in plain language.  "
   "After that, the Futon Anthology proper begins at <em>Start Here</em>, "
   "followed by devmaps, scenes, and the various leaves and globes.  "
   "Each story (leaf, devmap, globe) opens with a margin annotation "
   "(the <code>&#8853;</code> toggle) naming whether it is lifted "
   "into the canonical hypergraph and which R-criteria currently apply.  "
   "Open the toggle to read the annotation in the margin (wide screens) "
   "or inline (mobile).</p>\n"
   "<p>R-criteria audits, R-criterion prose, Reader-criteria audit, "
   "Reader-criteria prose, bilateral evidence, and the revisions trail "
   "now live in the <em>backmatter</em> at the end of this document.  "
   "Future R-criterion landings will surface in the audit rows there "
   "as new closures land in the <code>.aif.edn</code>.</p>\n"
   "</section>\n"))

(def FOREWORD-PATH
  (str (fs/expand-home "~/code/futon7a/essays/operator-foreword/operator-foreword.md")))

(defn html-foreword-section []
  "Render the Operator's Foreword (Section 0) from the canonical .md.
   The .md is H1 + prose paragraphs only (H2 scaffolding subheadings
   were stripped after the v1 annotation pass).  Strip the H1 (the
   section wraps the prose in its own <h2>), render the remaining
   prose as <p> blocks via md-paragraphs-to-html."
  (if-not (fs/exists? FOREWORD-PATH)
    ""
    (let [text (slurp FOREWORD-PATH)
          ;; Strip the leading H1 line and any following blank lines.
          without-h1 (str/replace text #"\A#[^\n]*\n+" "")]
      (str "<section id=\"operators-foreword\">\n"
           "<h2>Operator's Foreword</h2>\n"
           "<p class=\"source\"><small>Source: <code>"
           "~/code/futon7a/essays/operator-foreword/operator-foreword.md"
           "</code> · <a href=\"#vsatarcs-toc\">↑ TOC</a></small></p>\n"
           (md-paragraphs-to-html (str/trim without-h1))
           "\n</section>\n"))))

(defn html-prose-section
  "Convert a prose file (Markdown ## headings + paragraphs) into a
   top-level <section> with H2 TITLE and inner H3 per ## chunk.
   Returns empty string when PATH does not exist."
  [path title]
  (if-not (fs/exists? path)
    ""
    (let [text (slurp path)
          without-h1 (str/replace text #"(?s)^.*?(?=\n## )" "")
          chunks (str/split without-h1 #"(?m)^## ")
          intro (first chunks)
          bodies (rest chunks)
          as-html
          (str
           (md-paragraphs-to-html (str/trim intro))
           (str/join "\n"
                     (for [body bodies
                           :let [lines (str/split-lines body)
                                 head-line (first lines)
                                 rest-text (str/join "\n" (rest lines))]]
                       (str "<h3>" (html-escape head-line) "</h3>\n"
                            (md-paragraphs-to-html (str/trim rest-text))))))]
      (str "<section>\n<h2>" (html-escape title) "</h2>\n"
           as-html
           "\n</section>\n"))))

(defn html-r-criteria-prose-section []
  (html-prose-section PROSE-PATH "R-criteria in prose"))

(defn html-reader-criteria-prose-section []
  (html-prose-section READER-PROSE-PATH "Reader-criteria in prose (Q1-Q8)"))

(defn html-backmatter-section []
  "Wrap the technical reference apparatus (R-criteria, Reader-criteria,
   bilateral evidence, revisions trail) into a single backmatter
   section at the end of the document.  Frontmatter is now: how-to-read,
   TOC, Operator's Foreword, anthology body."
  (str "<section id=\"vsatarcs-backmatter\">\n"
       "<h2>Backmatter — R-criteria, Reader-criteria, Bilateral evidence, Revisions</h2>\n"
       "<p><em>R-criterion audit, R-criteria in prose (R1–R12), "
       "Reader-criterion audit, Reader-criteria in prose (Q1–Q8), "
       "bilateral evidence, and the revisions trail.  Technical "
       "reference apparatus for the Anthology above.</em></p>\n"
       (html-r-audit-section)
       (html-r-criteria-prose-section)
       (html-reader-audit-section)
       (html-reader-criteria-prose-section)
       (html-bilateral-evidence-section)
       (html-revisions-section)
       "</section>\n"))

(defn html-render-story [path]
  (let [story (parse-story path)
        section (section-for-story-basename (basename path))
        stem (story-stem path)]
    ;; Set context for link resolution within this story's body
    (reset! current-story-stem stem)
    (try
      (str
       "<section id=\"" (html-escape stem) "\">\n"
       "<h2>" (html-escape (:title story)) "</h2>\n"
       ;; Source + TOC backlink lives in a `section > p` (55% width per
       ;; Tufte rule).  The marginnote is placed INSIDE this paragraph
       ;; so the right-float + -60% margin lands it in the wide right
       ;; margin alongside the story's top.  This matches the inline
       ;; marginnote convention in `~/code/futon7a/index.html` / `ati.html`.
       "<p class=\"source\"><small>Source: <code>"
       (html-escape (str/replace path (str (fs/expand-home "~")) "~"))
       "</code> · <a href=\"#vsatarcs-toc\">↑ TOC</a></small>"
       (alignment-marginnote-html stem section)
       "</p>\n"
       ;; Flat structure: scenes are h3 + paragraphs at the same level as
       ;; the story's h2.  No nested sections.  Matches ati.html /
       ;; epsrc.html convention.  `section > p' rule applies (55% width
       ;; for marginnote-compatible layout).
       ;; Scene anchors are namespaced as `<stem>-<anchor>' so 47 stories'
       ;; "overview" scenes don't collide on the HTML id.
       (str/join "\n"
                 (for [s (:scenes story)
                       :let [anchor (:anchor s)]]
                   (str "<h3"
                        (when anchor
                          (str " id=\"" (html-escape stem) "-"
                               (html-escape anchor) "\""))
                        ">"
                        (html-escape (:title s))
                        "</h3>\n"
                        (md-paragraphs-to-html (:body s))
                        "\n")))
       "</section>\n\n")
      (finally (reset! current-story-stem nil)))))

(defn html-toc [ordered-paths]
  "Top-of-document table of contents linking to each story by stem.
   Prepends a Section 0 entry for the Operator's Foreword and appends
   a pointer to the backmatter section."
  (str "<section id=\"vsatarcs-toc\">\n"
       "<h2>Table of contents</h2>\n"
       "<ol start=\"0\">\n"
       "<li><a href=\"#operators-foreword\">Operator's Foreword</a> "
       "<small>(Section 0)</small></li>\n"
       (str/join ""
                 (for [p ordered-paths
                       :let [stem (story-stem p)
                             story (parse-story p)
                             lifted? (some? (section-for-story-basename (basename p)))]]
                   (format "<li><a href=\"#%s\">%s</a>%s</li>\n"
                           (html-escape stem)
                           (html-escape (:title story))
                           (if lifted? " <small>(lifted)</small>" ""))))
       "</ol>\n"
       "<p><strong>Backmatter:</strong> <a href=\"#vsatarcs-backmatter\">"
       "R-criterion audit · R-criteria in prose · Reader-criteria · "
       "Bilateral evidence · Revisions trail</a></p>\n"
       "</section>\n"))

(defn html-footer []
  "</article>\n</body>\n</html>\n")

(defn emit-html [ordered]
  (reset! html-mn-counter 0)
  (build-anchor-index ordered)
  (let [body (str/join "" (map html-render-story ordered))]
    (str (html-header)
         (html-reader-instructions)
         (html-toc ordered)
         (html-foreword-section)
         body
         (html-backmatter-section)
         (html-footer))))

(defn -main []
  (let [all (story-files (:stories opts))
        ordered (ordered-story-files all)
        md (str (header) (str/join "" (map render-story ordered)))
        html (emit-html ordered)]
    (io/make-parents (:out opts))
    (spit (:out opts) md)
    (io/make-parents (:html-out opts))
    (spit (:html-out opts) html)
    (let [lifted-count (count (keep #(section-for-story-basename (basename %))
                                    ordered))]
      (println (format "Generated %s (%d lines)" (:out opts) (count (str/split-lines md))))
      (println (format "Generated %s (%d lines)" (:html-out opts) (count (str/split-lines html))))
      (println (format "  %d stories total; %d lifted; %d unlifted"
                       (count ordered) lifted-count
                       (- (count ordered) lifted-count))))))

(-main)
