#!/usr/bin/env bb
;; lift_unlifted_stories.bb — Walk ~/code/futon5a/holes/stories/ and add
;; :sections[] entries to stack-annotations.edn for every story that is
;; not yet lifted.  For stories with a companion .aif.edn, decompose
;; into child claim entities mapped from the .aif.edn :nodes; for those
;; without, register a stub-lift parent only.  Idempotent: rerunning
;; detects already-lifted stories (by :ref) and skips them.
;;
;; This implements the agent-driven lift described in the VSATARCS
;; "Alignment status: not yet lifted" marginnote.  The claim that
;; documentation stays up to date automatically requires this kind of
;; routine: any unlifted candidate eventually becomes lifted via the
;; same procedure, repeatably.
;;
;; Usage:
;;   bb ~/code/futon4/scripts/lift_unlifted_stories.bb [--dry-run]
;;   bb ~/code/futon4/scripts/lift_unlifted_stories.bb [--dry-run] --only leaf-1.md
;;
;; A backup of stack-annotations.edn is written to
;;   ~/code/futon5a/holes/stack-annotations.edn~arxana~
;; before any mutation.

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[babashka.fs :as fs])

(def STORIES-DIR (str (fs/expand-home "~/code/futon5a/holes/stories")))
(def STACK-ANN-PATH (str (fs/expand-home "~/code/futon5a/holes/stack-annotations.edn")))
(def SORRYS-PATH ;; R-A.1: relocated data/ → resources/; prefer resources, fall back to data
  (let [res (str (fs/expand-home "~/code/futon2/resources/sorrys.edn"))
        old (str (fs/expand-home "~/code/futon2/data/sorrys.edn"))]
    (if (fs/exists? res) res old)))
(def STUB-SORRY-ID :sorry/stub-lifts-pending-aif-edn)
(def DRY-RUN? (some #{"--dry-run"} *command-line-args*))
(def ONLY
  (when-let [[_ value] (some (fn [[arg next-arg]]
                               (when (= "--only" arg)
                                 [arg next-arg]))
                             (partition 2 1 (concat *command-line-args* [nil])))]
    value))

(defn selected-story?
  "When --only is supplied, match either the story filename or its repo-relative ref."
  [ref]
  (or (nil? ONLY)
      (= ONLY ref)
      (= ONLY (fs/file-name ref))))

;; ---------- read / parse ----------

(defn read-edn-file [path]
  (with-open [r (io/reader path)]
    (edn/read {:readers {'inst (fn [s] s)}
               :default (fn [_t form] form)}
              (java.io.PushbackReader. r))))

(defn lifted-refs [stack-ann]
  (set (keep :ref (:sections stack-ann))))

(defn story-files [dir]
  (->> (fs/list-dir dir) (map str)
       (filter #(str/ends-with? % ".md"))
       (remove #(str/ends-with? % ".aif.md"))
       sort vec))

(defn ref-for-story [story-path]
  (str "futon5a/holes/stories/" (fs/file-name story-path)))

(defn unlifted-stories [stack-ann]
  (let [lifted (lifted-refs stack-ann)]
    (->> (story-files STORIES-DIR)
         (filter #(selected-story? (ref-for-story %)))
         (remove #(lifted (ref-for-story %))))))

;; ---------- naming ----------

(defn story-kind+slug
  "Map filename → [kind-keyword slug-string]."
  [filename]
  (let [stem (str/replace filename #"\.md$" "")]
    (cond
      (str/starts-with? stem "devmap-")
      [:devmap (subs stem (count "devmap-"))]
      (str/starts-with? stem "leaf-")
      [:leaf (subs stem (count "leaf-"))]
      (re-find #"^globe\d" stem)
      [:globe (subs stem (count "globe"))]
      (str/starts-with? stem "war-machine-")
      [:war-machine (subs stem (count "war-machine-"))]
      (str/starts-with? stem "strategic-")
      [:strategic (subs stem (count "strategic-"))]
      :else
      [:other stem])))

(defn entity-id [kind slug]
  (str "arxana/stack/futon-v1/" (name kind) "/" slug))

(defn parse-name-from-md [path]
  (let [text (slurp path)
        h1 (re-find #"(?m)^#\s+(.+?)$" text)]
    (if h1
      (str/trim (nth h1 1))
      (str/replace (fs/file-name path) #"\.md$" ""))))

(defn aif-path-for [story-path]
  (str (str/replace story-path #"\.md$" "") ".aif.edn"))

;; ---------- lift building ----------

(defn child-entries
  "Generate child claim entities for STORY-PATH from its companion
   .aif.edn.  Returns [] if no .aif.edn or no :nodes."
  [story-path parent-id aif-edn]
  (let [nodes (:nodes aif-edn)
        spine (set (:spine aif-edn))
        aif-path (aif-path-for story-path)
        aif-rel (str (fs/relativize (fs/expand-home "~/code") aif-path))]
    (vec
     (for [node nodes
           :let [node-id (:id node)
                 node-slug (str/replace (name node-id) #"^n" "")
                 child-id (str parent-id "/" node-slug)]]
       (cond-> {:id child-id
                :kind (or (:type node) :claim)
                :content (:content node)
                :status (or (:status node) :open)
                :spine-member? (boolean (spine node-id))
                :part-of parent-id
                :provenance {:source-file aif-rel
                             :node-id node-id}}
         (:role node) (assoc :role (:role node))
         (:ref node) (assoc :ref (:ref node))
         (:gap node) (assoc :gap (:gap node))
         (:aif-region node) (assoc :aif-region (:aif-region node))
         (:note node) (assoc :note (:note node))
         (:evidence-count node) (assoc :evidence-count (:evidence-count node)))))))

(defn lift-for-story [story-path]
  (let [filename (fs/file-name story-path)
        [kind slug] (story-kind+slug filename)
        parent-id (entity-id kind slug)
        story-name (parse-name-from-md story-path)
        ref-str (ref-for-story story-path)
        aif (aif-path-for story-path)
        aif-exists? (fs/exists? aif)
        aif-edn (when aif-exists? (read-edn-file aif))
        children (if aif-edn (child-entries story-path parent-id aif-edn) [])
        child-ids (mapv :id children)
        aif-rel (when aif-exists?
                  (str (fs/relativize (fs/expand-home "~/code") aif)))]
    {:parent (cond-> {:id parent-id
                      :kind kind
                      :name story-name
                      :ref ref-str
                      :decomposes-into child-ids
                      :provenance (cond-> {:lifted-from (if aif-exists?
                                                          :composite-top-level
                                                          :stub-no-aif)
                                           :auto-lifted-by "lift_unlifted_stories.bb"
                                           :auto-lifted-at (str (java.time.Instant/now))}
                                    aif-rel (assoc :source-file aif-rel))})
     :children children
     :had-aif? aif-exists?}))

;; ---------- EDN serialization for insertion ----------

(defn edn-render-map [m indent]
  "Render M as EDN-compatible string with reasonable per-key indentation."
  (let [pad (apply str (repeat indent " "))
        kv-pad (apply str (repeat (+ indent 1) " "))]
    (str "{"
         (str/join (str "\n" kv-pad)
                   (for [[k v] m]
                     (str (pr-str k) " " (pr-str v))))
         "}")))

(defn render-entries-block [parents+children]
  "Render LIFTS (the parent + child sections) as an EDN-compatible text
   block to splice into the :sections vector.  Each entry on its own
   chunk with a blank line separator."
  (str/join "\n\n"
            (for [entry parents+children]
              (str "  " (edn-render-map entry 2)))))

;; ---------- :sections vector closing-] finder ----------

(defn skip-string
  "Given TEXT and index I at a `\"`, return the index just past the
   matching closing `\"`.  Handles backslash escapes."
  [text i]
  (let [n (count text)]
    (loop [j (inc i)]
      (cond
        (>= j n) n
        (= (.charAt text j) \\) (recur (+ j 2))
        (= (.charAt text j) \") (inc j)
        :else (recur (inc j))))))

(defn find-sections-close
  "In TEXT, find the position of the `]` that closes the `:sections`
   vector.  Returns the index of the `]` character, or nil.

   Scan forward from `:sections`, find the first `[`, then walk
   tracking bracket depth, skipping string literals and EDN line
   comments (;...newline)."
  [text]
  (let [start (str/index-of text ":sections")
        n (count text)]
    (when start
      (let [open (str/index-of text "[" start)]
        (when open
          (loop [i (inc open)
                 depth 1]
            (cond
              (>= i n) nil
              (zero? depth) nil
              :else
              (let [c (.charAt text i)]
                (cond
                  (= c \") (recur (skip-string text i) depth)
                  (= c \;) (let [eol (str/index-of text "\n" i)]
                             (recur (if eol (inc eol) n) depth))
                  (= c \[) (recur (inc i) (inc depth))
                  (= c \]) (if (= depth 1)
                             i
                             (recur (inc i) (dec depth)))
                  :else (recur (inc i) depth))))))))))

;; ---------- stub-upgrade detection + in-place replacement ----------

(defn stub-section? [section]
  (= :stub-no-aif (some-> section :provenance :lifted-from)))

(defn upgradable-stubs
  "Sections currently stub-lifted whose .aif.edn now exists."
  [stack-ann]
  (let [code-root (fs/expand-home "~/code")]
    (->> (:sections stack-ann)
         (filter stub-section?)
         (filter #(selected-story? (:ref %)))
         (keep (fn [section]
                 (let [story-path (str (fs/path code-root (:ref section)))
                       aif-path (aif-path-for story-path)]
                   (when (fs/exists? aif-path)
                     {:section section :story-path story-path})))))))

(defn find-matching-close-brace
  "Given TEXT and OPEN-POS at a `{`, return position of matching `}`."
  [text open-pos]
  (let [n (count text)]
    (loop [i (inc open-pos) depth 1]
      (cond
        (>= i n) nil
        :else
        (let [c (.charAt text i)]
          (cond
            (= c \") (recur (skip-string text i) depth)
            (= c \;) (let [eol (str/index-of text "\n" i)]
                       (recur (if eol (inc eol) n) depth))
            (= c \{) (recur (inc i) (inc depth))
            (= c \}) (if (= depth 1) i (recur (inc i) (dec depth)))
            :else (recur (inc i) depth)))))))

(defn find-entry-bounds
  "Find [start end] of the `{...}` map containing TARGET (a literal substring,
   typically `:id \"<entity-id>\"`).  Returns nil if not found.  END is the
   position just past the closing `}`."
  [text target]
  (let [tpos (str/index-of text target)]
    (when tpos
      (loop [i tpos]
        (cond
          (neg? i) nil
          (= (.charAt text i) \{)
          (when-let [close (find-matching-close-brace text i)]
            [i (inc close)])
          :else (recur (dec i)))))))

(defn upgrade-stubs-in-text
  "For each upgradable stub, remove its entry from TEXT.  Returns
   {:text text', :upgraded-count N, :upgraded-stories [..]}.  Removing
   the stub entry means the next `add-missing' pass will re-lift it
   with the full decomposition (since .aif.edn now exists)."
  [text stack-ann]
  (let [stubs (upgradable-stubs stack-ann)]
    (loop [text text n 0 stories [] remaining stubs]
      (if-let [{:keys [section]} (first remaining)]
        (let [target (str ":id \"" (:id section) "\"")
              bounds (find-entry-bounds text target)]
          (if-not bounds
            (do (binding [*out* *err*]
                  (println "WARN: stub entry not found textually:" (:id section)))
                (recur text count stories (rest remaining)))
            (let [[start end] bounds
                  ;; Also consume trailing whitespace / newline so we don't
                  ;; leave a blank-line gap.
                  end' (loop [j end]
                         (if (and (< j (count text))
                                  (#{\space \tab \newline} (.charAt text j)))
                           (recur (inc j))
                           j))]
              (recur (str (subs text 0 start) (subs text end'))
                     (inc n)
                     (conj stories (:ref section))
                     (rest remaining)))))
        {:text text :upgraded-count n :upgraded-stories stories}))))

;; ---------- sorrys.edn :links update ----------

(defn current-stub-refs
  "Return sorted vector of :ref values for sections still stub-lifted."
  [stack-ann]
  (->> (:sections stack-ann) (filter stub-section?) (map :ref) sort vec))

(defn find-matching-close-bracket
  "Given TEXT and OPEN-POS at a `[`, return position of matching `]`."
  [text open-pos]
  (let [n (count text)]
    (loop [i (inc open-pos) depth 1]
      (cond
        (>= i n) nil
        :else
        (let [c (.charAt text i)]
          (cond
            (= c \") (recur (skip-string text i) depth)
            (= c \;) (let [eol (str/index-of text "\n" i)]
                       (recur (if eol (inc eol) n) depth))
            (= c \[) (recur (inc i) (inc depth))
            (= c \]) (if (= depth 1) i (recur (inc i) (dec depth)))
            :else (recur (inc i) depth)))))))

(defn refresh-stub-sorry!
  "Surgical update of :sorry/stub-lifts-pending-aif-edn in sorrys.edn:
   replace its :status value and :links vector; leave everything else
   (rationale, title, kind, etc.) untouched.  No-op if both already
   match current stub state.  Returns a result map for logging."
  []
  (let [stack-ann (read-edn-file STACK-ANN-PATH)
        sorrys (read-edn-file SORRYS-PATH)
        sorry (some #(when (= STUB-SORRY-ID (:id %)) %) (:sorrys sorrys))
        stub-refs (current-stub-refs stack-ann)
        supports ["futon4/scripts/lift_unlifted_stories.bb"
                  "futon5a/holes/stack-annotations.edn"]
        new-links (vec (concat stub-refs supports))
        new-status (if (seq stub-refs) :open :addressed)]
    (cond
      (not sorry)
      {:result :sorry-not-found}

      (and (= new-links (:links sorry))
           (= new-status (:status sorry)))
      {:result :no-change :stub-count (count stub-refs) :status new-status}

      :else
      (let [text (slurp SORRYS-PATH)
            target (str ":id " (pr-str STUB-SORRY-ID))
            bounds (find-entry-bounds text target)]
        (if-not bounds
          {:result :sorry-not-located}
          (let [[s e] bounds
                sorry-text (subs text s e)
                ;; Replace the top-level :status key. Do not rewrite
                ;; keyword-looking text inside the rationale string.
                sorry-text1 (str/replace-first sorry-text
                                               #"(?m)^(\s*:status\s+):[A-Za-z-]+"
                                               (str "$1" (pr-str new-status)))
                ;; Replace :links [...] vector
                links-idx (str/index-of sorry-text1 "\n   :links ")
                vec-open (str/index-of sorry-text1 "[" links-idx)
                vec-close (find-matching-close-bracket sorry-text1 vec-open)
                links-rendered (str "[" (str/join (str "\n" (apply str (repeat 11 " ")))
                                                  (map pr-str new-links)) "]")
                sorry-text2 (str (subs sorry-text1 0 vec-open)
                                 links-rendered
                                 (subs sorry-text1 (inc vec-close)))]
            (spit SORRYS-PATH (str (subs text 0 s)
                                   sorry-text2
                                   (subs text e)))
            {:result :updated
             :stub-count (count stub-refs)
             :status new-status
             :links-count (count new-links)}))))))

;; ---------- main ----------

(defn -main []
  (let [stack-ann0 (read-edn-file STACK-ANN-PATH)
        text0 (slurp STACK-ANN-PATH)
        ;; Backup
        _ (when-not DRY-RUN?
            (fs/copy STACK-ANN-PATH (str STACK-ANN-PATH "~arxana~")
                     {:replace-existing true}))
        ;; STEP 1: detect upgradable stubs, remove them from text so the
        ;; subsequent add-missing pass re-lifts them with full decomposition.
        upgrade-result (upgrade-stubs-in-text text0 stack-ann0)
        text-after-upgrade (:text upgrade-result)
        upgraded-count (:upgraded-count upgrade-result)
        ;; Re-parse from in-memory text so add-missing sees the post-upgrade state.
        stack-ann (edn/read-string {:readers {'inst identity}
                                    :default (fn [_ form] form)}
                                   text-after-upgrade)
        ;; STEP 2: add-missing for any unlifted .md.
        unlifted (unlifted-stories stack-ann)
        lifts (mapv lift-for-story unlifted)
        with-aif (count (filter :had-aif? lifts))
        stubs (- (count lifts) with-aif)
        new-entries (mapcat (fn [l] (into [(:parent l)] (:children l))) lifts)]
    (println (format "Upgrade pass: %d stub(s) upgraded to full decomposition"
                     upgraded-count))
    (when (seq (:upgraded-stories upgrade-result))
      (doseq [s (:upgraded-stories upgrade-result)]
        (println (format "  upgraded: %s" s))))
    (println (format "Add pass: %d unlifted stories" (count lifts)))
    (println (format "  %d with .aif.edn (full decomposition)" with-aif))
    (println (format "  %d as stub (no .aif.edn yet)" stubs))
    (println (format "  %d new entities to add" (count new-entries)))
    (when DRY-RUN?
      (println "DRY-RUN: no changes written.")
      (System/exit 0))
    ;; STEP 3: write back stack-annotations.edn with upgrades-removed +
    ;; new entries appended before the :sections closing ].
    (let [close-pos (find-sections-close text-after-upgrade)
          _ (when-not close-pos
              (println "ERROR: could not find :sections closing ].  Aborting.")
              (System/exit 1))
          block (when (seq new-entries) (render-entries-block new-entries))
          updated (if block
                    (str (subs text-after-upgrade 0 close-pos)
                         "\n\n" block "\n  "
                         (subs text-after-upgrade close-pos))
                    text-after-upgrade)]
      (spit STACK-ANN-PATH updated)
      (println (format "Wrote stack-annotations.edn (upgrades=%d, adds=%d, backup at %s~arxana~)"
                       upgraded-count (count new-entries) STACK-ANN-PATH)))
    ;; STEP 4: refresh the stub-lifts sorry (surgical, preserves rationale form).
    (when-not DRY-RUN?
      (fs/copy SORRYS-PATH (str SORRYS-PATH "~arxana~")
               {:replace-existing true})
      (let [r (refresh-stub-sorry!)]
        (case (:result r)
          :no-change      (println (format "Sorry: no change (still %d stub refs, status=%s)"
                                           (:stub-count r) (:status r)))
          :updated        (println (format "Sorry: updated → %d stub refs, status=%s"
                                           (:stub-count r) (:status r)))
          :sorry-not-found (println "WARN: stub-lifts sorry not found in sorrys.edn")
          :sorry-not-located (println "WARN: stub-lifts sorry's text bounds not found")
          (println "WARN: sorry update returned unexpected result:" r))))))

(-main)
