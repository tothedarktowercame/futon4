#!/usr/bin/env bb
;; peeragogy-migrate-annotations.clj
;;
;; Re-anchor the 88 Peeragogy annotations from their .md (Markdown)
;; passages to the canonical .mw (wikitext) text.
;;
;; Strategy: a "fingerprint" string (lowercase, alphanumeric+space only,
;; whitespace-collapsed) makes both formats comparable.  Find the unique
;; fingerprint match in the wikitext, then walk the original wikitext
;; counting non-skip characters to recover the corresponding original
;; wikitext span.  That span becomes the new :passage.
;;
;; Inputs:
;;   data/essays/wikibooks/peeragogy-handbook/peeragogy-handbook-book-annotations.el
;;   data/essays/wikibooks/peeragogy-handbook/peeragogy-handbook-book-manifest.el
;;   data/essays/wikibooks/peeragogy-handbook/wikitext/*.mw
;;
;; Outputs:
;;   data/essays/wikibooks/peeragogy-handbook/peeragogy-handbook-book-annotations-mw.el
;;     (new sidecar with :passage replaced; identical structure otherwise)
;;   stdout: per-annotation status line + final summary
;;   stderr: detailed report of unmatched annotations

(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(def REPO-ROOT (or (System/getenv "FUTON4_ROOT") "/home/joe/code/futon4"))
(def DATA-DIR  (str REPO-ROOT "/data/essays/wikibooks/peeragogy-handbook"))
(def WIKITEXT-DIR (str DATA-DIR "/wikitext"))
(def ANNOTATIONS-IN  (str DATA-DIR "/peeragogy-handbook-book-annotations.el"))
(def MANIFEST-IN     (str DATA-DIR "/peeragogy-handbook-book-manifest.el"))
(def ANNOTATIONS-OUT (str DATA-DIR "/peeragogy-handbook-book-annotations-mw.el"))

;; ----- manifest parsing: entity-id → .mw file path ----------------------

(defn parse-manifest-id->mw
  "Returns map from full entity-id (string) to .mw file path (string)."
  []
  (let [text (slurp MANIFEST-IN)
        ;; (:id "arxana/.../section/N-slug" :name "..." ... (page-title . "...") (source-file . "/path/.../slug.md") ...)
        re #"\(:id\s+\"(arxana/essay/wikibooks/book/peeragogy-handbook/section/[^\"]+)\"[\s\S]*?source-file[\s\S]*?\"([^\"]+\.md)\""]
    (into {} (for [[_ entity-id md-path] (re-seq re text)]
               (let [base (.getName (io/file md-path))
                     mw-name (str/replace base #"\.md$" ".mw")
                     mw-path (str WIKITEXT-DIR "/" mw-name)]
                 [entity-id mw-path])))))

;; ----- annotation parsing -----------------------------------------------

(defn unescape-elisp-string
  "Reverse elisp string escapes inside :passage values.  Handles
   \\\" \\\\ \\n \\t \\r — what the sidecar uses in practice."
  [s]
  (let [n (count s)
        sb (StringBuilder.)]
    (loop [i 0]
      (if (>= i n) (.toString sb)
          (let [c (.charAt ^String s i)]
            (cond
              (and (= c \\) (< (inc i) n))
              (let [c2 (.charAt ^String s (inc i))]
                (case c2
                  \"  (do (.append sb \") (recur (+ i 2)))
                  \\  (do (.append sb \\) (recur (+ i 2)))
                  \n  (do (.append sb \newline) (recur (+ i 2)))
                  \t  (do (.append sb \tab) (recur (+ i 2)))
                  \r  (do (.append sb \return) (recur (+ i 2)))
                  ;; unknown escape: keep both chars
                  (do (.append sb c) (.append sb c2) (recur (+ i 2)))))
              :else
              (do (.append sb c) (recur (inc i)))))))))

(defn- pad [n] (apply str (repeat n \space)))

(defn strip-markdown
  "Pre-strip Markdown markup, REPLACING markup characters with same-length
   runs of spaces so byte offsets are preserved.  Fingerprinting then
   collapses those spaces; visible prose remains in its original positions.
   Links and images become just their visible text (URL replaced with
   spaces of equal length)."
  [s]
  (-> s
      ;; images: ![alt](url) → "  " + alt + "    " (pads ![ + ]( + ) )
      (str/replace #"!\[([^\]]*)\]\(([^)]*)\)"
                   (fn [[_ alt url]]
                     (str "  " alt "  " (pad (count url)) " ")))
      ;; links:  [text](url) → " " + text + "  " + spaces(url) + " "
      (str/replace #"\[([^\]]*)\]\(([^)]*)\)"
                   (fn [[_ text url]]
                     (str " " text "  " (pad (count url)) " ")))
      ;; reference-style links [text][id] → " " + text + "  " + spaces(id) + " "
      (str/replace #"\[([^\]]*)\]\[([^\]]*)\]"
                   (fn [[_ text id]]
                     (str " " text "  " (pad (count id)) " ")))
      ;; inline code: `text` → " " + text + " "
      (str/replace #"`([^`]*)`"
                   (fn [[_ inner]] (str " " inner " ")))
      ;; bold/italic markers — replace each marker with one space
      (str/replace #"\*\*\*|\*\*|\*"
                   (fn [m] (pad (count (if (vector? m) (first m) m)))))
      (str/replace #"___|__|_"
                   (fn [m] (pad (count (if (vector? m) (first m) m)))))
      ;; ATX heading markers at start of line — pad with spaces
      (str/replace #"(?m)^(#{1,6}\s+)"
                   (fn [[whole]] (pad (count whole))))
      ;; list bullets at start of line
      (str/replace #"(?m)^(\s*[-*+]\s+)"
                   (fn [[whole]] (pad (count whole))))
      ;; numbered list at start of line
      (str/replace #"(?m)^(\s*\d+\.\s+)"
                   (fn [[whole]] (pad (count whole))))
      ;; blockquote marker at start of line
      (str/replace #"(?m)^(>\s?)"
                   (fn [[whole]] (pad (count whole))))))

(defn strip-wikitext
  "Pre-strip wikitext markup with byte offsets preserved (same-length
   space replacement).  Constructs containing visible text keep that
   text in place; surrounding markup becomes spaces."
  [s]
  (-> s
      ;; templates {{...}} (single-level) — drop entirely (all spaces)
      (str/replace #"\{\{[^{}]*\}\}"
                   (fn [m] (pad (count (if (vector? m) (first m) m)))))
      ;; File / Image embeds — drop entirely
      (str/replace #"\[\[(?:File|Image):[^\]]*\]\]"
                   (fn [m] (pad (count (if (vector? m) (first m) m)))))
      ;; internal links with display text [[Page|text]]
      ;; → spaces([[Page|) + text + spaces(]])
      (str/replace #"\[\[([^\]|]*)\|([^\]]*)\]\]"
                   (fn [[_ link text]]
                     (str "  " (pad (count link)) " " text "  ")))
      ;; internal links [[Page]] → spaces([[) + Page + spaces(]])
      (str/replace #"\[\[([^\]]*)\]\]"
                   (fn [[_ link]] (str "  " link "  ")))
      ;; external links with display text [http://... text]
      ;; → spaces([) + spaces(url) + spaces( ) + text + spaces(])
      (str/replace #"\[(https?://\S+)(\s+)([^\]]*)\]"
                   (fn [[_ url ws text]]
                     (str " " (pad (count url)) (pad (count ws)) text " ")))
      ;; bare external links [http://...] → all spaces
      (str/replace #"\[https?://\S+\]"
                   (fn [m] (pad (count (if (vector? m) (first m) m)))))
      ;; bold '''x''' / italic ''x'' — replace markers with spaces
      (str/replace #"'''" "   ")
      (str/replace #"''" "  ")
      ;; heading markers ==X== — pad leading and trailing = runs
      (str/replace #"(?m)^(=+)\s*" (fn [[whole]] (pad (count whole))))
      (str/replace #"(?m)\s*(=+)\s*$" (fn [[whole]] (pad (count whole))))
      ;; line-leading list/numbered/desc/blockquote markers
      (str/replace #"(?m)^([*#:;>]+\s*)"
                   (fn [[whole]] (pad (count whole))))
      ;; nowiki/ref/code tags (drop tag only, keep inner content)
      (str/replace #"</?(?:nowiki|ref|code|pre|small|big|sub|sup|tt|u|s)[^>]*>"
                   (fn [m] (pad (count (if (vector? m) (first m) m)))))
      ;; HTML entities → keep length but make them whitespace-ish.
      ;; &quot; (6c) → 6 spaces; &amp; (5c) → 5 spaces; etc.
      ;; This is fine for fingerprint purposes since the entities don't
      ;; carry alphanumeric prose anyway — they're punctuation surrogates.
      (str/replace #"&(?:quot|amp|lt|gt|nbsp|#\d+);"
                   (fn [m] (pad (count (if (vector? m) (first m) m)))))))

(defn parse-annotations
  "Returns seq of {:id, :entity-id, :passage, :raw-block-start, :raw-block-end}.
   Reads the .el sidecar with regex; stores byte ranges so the output can
   be assembled by splicing edited :passage into the same surrounding text."
  []
  (let [text (slurp ANNOTATIONS-IN)
        ;; Each annotation block starts with `(:id "hx:...` — capture the id
        ;; and the entity-id + passage that follow.
        ;;
        ;; passage is multi-line elisp string; we use [\s\S]*? non-greedy
        ;; up to the close-quote that immediately precedes `:source` or `)`
        ;; at the same nesting.  Practically: passage is between :passage "
        ;; and the next `\n      :source` or `\n      :note`.
        re-id #"\(:id\s+\"(hx:[^\"]+)\"[\s\S]*?:annotated[\s\S]*?:entity-id[\s\S]*?\"(arxana/essay/wikibooks/book/peeragogy-handbook/section/[^\"]+)\"[\s\S]*?:passage\s+\"((?:[^\"\\]|\\.)*)\""]
    (for [m (re-seq re-id text)]
      {:id (nth m 1)
       :entity-id (nth m 2)
       :passage (unescape-elisp-string (nth m 3))})))

;; ----- fingerprint + migration ------------------------------------------

(defn fingerprint-char?
  "A character that participates in the fingerprint: lowercase ASCII
   alpha, ASCII digit, or space (space comes from any whitespace)."
  [c]
  (or (and (>= (int c) (int \a)) (<= (int c) (int \z)))
      (and (>= (int c) (int \0)) (<= (int c) (int \9)))
      (= c \space)))

(defn fingerprint+map
  "Returns [fingerprint-string, position-map].
   position-map[i] = byte offset in original where fingerprint char i came from.
   The fingerprint is whitespace-collapsed, lowercase, alphanumeric+space only.
   Newlines, tabs, punctuation, markup characters all become 'skip'."
  [s]
  (let [n (count s)
        sb (StringBuilder.)
        positions (java.util.ArrayList.)
        last-was-space (atom true)] ;; collapse leading whitespace
    (doseq [i (range n)]
      (let [c (Character/toLowerCase (.charAt ^String s i))
            ws? (Character/isWhitespace c)]
        (cond
          ws?
          (when-not @last-was-space
            (.append sb \space)
            (.add positions (int i))
            (reset! last-was-space true))
          (fingerprint-char? c)
          (do (.append sb c)
              (.add positions (int i))
              (reset! last-was-space false))
          ;; everything else (markup, punctuation): skip
          :else nil)))
    (let [fp (.toString sb)
          ;; trim trailing space if present (won't show in count fence)
          fp (if (and (pos? (count fp)) (= \space (.charAt fp (dec (count fp)))))
               (subs fp 0 (dec (count fp)))
               fp)
          pmap (vec (take (count fp) positions))]
      [fp pmap])))

(defn count-occurrences [^String haystack ^String needle]
  (loop [i 0 n 0]
    (let [j (.indexOf haystack needle i)]
      (if (neg? j) n (recur (+ j (count needle)) (inc n))))))

(defn migrate-passage
  "Returns one of:
   {:status :ok       :new-passage \"...wikitext span...\"}
   {:status :no-match :reason \"...\"}
   {:status :ambiguous :hits N}
   {:status :missing-source :file path}"
  [md-passage mw-path]
  (cond
    (not (.exists (io/file mw-path)))
    {:status :missing-source :file mw-path}

    :else
    (let [mw-text (slurp mw-path)
          ;; pre-strip with same-length space substitution so byte offsets
          ;; into the original wikitext are preserved through the fingerprint
          mw-stripped (strip-wikitext mw-text)
          [mw-fp mw-pmap] (fingerprint+map mw-stripped)
          try-search
          (fn [search-text]
            (let [search-stripped (strip-markdown search-text)
                  [search-fp _] (fingerprint+map search-stripped)]
              (cond
                (str/blank? search-fp) :empty
                :else
                (let [hits (count-occurrences mw-fp search-fp)]
                  (cond
                    (zero? hits) :miss
                    (> hits 1)   {:ambig hits}
                    :else
                    (let [start-fp-idx (.indexOf mw-fp search-fp)
                          end-fp-idx   (+ start-fp-idx (count search-fp) -1)
                          orig-start (nth mw-pmap start-fp-idx)
                          orig-end   (inc (nth mw-pmap end-fp-idx))]
                      {:ok true
                       :new-passage (subs mw-text orig-start orig-end)
                       :orig-start orig-start :orig-end orig-end}))))))
          ;; Pass 1: the whole passage as-is.
          r1 (try-search md-passage)]
      (cond
        (and (map? r1) (:ok r1))
        (assoc (dissoc r1 :ok) :status :ok)

        :else
        ;; Pass 2: first non-empty line only (handles condensed multi-line
        ;; passages where the annotation summarised a bullet list etc.)
        (let [first-line (->> (str/split md-passage #"\n")
                              (map str/trim)
                              (remove str/blank?)
                              first)
              r2 (when (and first-line (>= (count first-line) 20))
                   (try-search first-line))]
          (cond
            (and (map? r2) (:ok r2))
            (-> r2 (dissoc :ok) (assoc :status :ok :match-mode :first-line))

            (and (map? r1) (:ambig r1))
            {:status :ambiguous :hits (:ambig r1)}

            :else
            {:status :no-match
             :reason (cond
                       (= r1 :empty) "passage produced empty fingerprint"
                       (= r1 :miss) "fingerprint not found in wikitext (full or first-line)"
                       :else (str "search returned " (pr-str r1)))}))))))

;; ----- main -------------------------------------------------------------

(defn elisp-escape [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")))

(defn find-annotation-block-bounds
  "Returns [open-pos, close-pos+1] for the annotation block whose :id is
   ANNOTATION-ID, or nil if not found.  Uses balanced-paren scanning with
   elisp-string awareness."
  [^String text annotation-id]
  (let [marker (str ":id \"" annotation-id "\"")
        marker-pos (.indexOf text marker)]
    (when (>= marker-pos 0)
      (let [open-pos
            (loop [i (dec marker-pos)]
              (cond
                (neg? i) nil
                (= \( (.charAt text i)) i
                :else (recur (dec i))))]
        (when open-pos
          (let [n (count text)
                close-pos
                (loop [i open-pos depth 0 in-str? false escape? false]
                  (if (>= i n) nil
                      (let [c (.charAt text i)]
                        (cond
                          in-str?
                          (cond
                            escape?       (recur (inc i) depth in-str? false)
                            (= c \\)      (recur (inc i) depth in-str? true)
                            (= c \")      (recur (inc i) depth false false)
                            :else         (recur (inc i) depth in-str? false))
                          :else
                          (cond
                            (= c \")      (recur (inc i) depth true false)
                            (= c \()      (recur (inc i) (inc depth) in-str? false)
                            (= c \))      (let [nd (dec depth)]
                                            (if (zero? nd) i
                                                (recur (inc i) nd in-str? false)))
                            :else         (recur (inc i) depth in-str? false))))))]
            (when close-pos
              [open-pos (inc close-pos)])))))))

(defn delete-annotation-block
  "Remove the annotation block with ANNOTATION-ID from TEXT, including any
   immediately-trailing blank line so the surrounding spacing stays clean."
  [^String text annotation-id]
  (if-let [[start end] (find-annotation-block-bounds text annotation-id)]
    (let [n (count text)
          ;; Consume one trailing blank line (whitespace + newline + optional whitespace + newline)
          tail-end
          (loop [i end blanks-consumed 0]
            (cond
              (>= i n) i
              (= (.charAt text i) \space) (recur (inc i) blanks-consumed)
              (= (.charAt text i) \tab)   (recur (inc i) blanks-consumed)
              (and (= (.charAt text i) \newline) (zero? blanks-consumed))
              (recur (inc i) 1)
              :else i))]
      (str (subs text 0 start) (subs text tail-end)))
    text))

(defn -main [& _args]
  (let [id->mw (parse-manifest-id->mw)
        annotations (parse-annotations)
        results
        (for [{:keys [id entity-id passage] :as ann} annotations]
          (let [mw-path (id->mw entity-id)]
            (if (nil? mw-path)
              (assoc ann :status :no-mw-mapping)
              (let [r (migrate-passage passage mw-path)]
                (merge ann r)))))]
    ;; per-annotation status line
    (doseq [r results]
      (println (str (case (:status r)
                      :ok             "OK     "
                      :no-match       "MISS   "
                      :ambiguous      "AMBIG  "
                      :missing-source "NO-SRC "
                      :no-mw-mapping  "NO-MAP "
                      "?      ")
                    (:id r))))
    (let [groups (group-by :status results)
          tally (fn [k] (count (groups k)))]
      (binding [*out* *err*]
        (println (str "\n[migrate] " (count results) " annotations | "
                      "ok=" (tally :ok) "  miss=" (tally :no-match)
                      "  ambig=" (tally :ambiguous)
                      "  no-src=" (tally :missing-source)
                      "  no-map=" (tally :no-mw-mapping)))
        (doseq [k [:no-match :ambiguous :missing-source :no-mw-mapping]
                r (groups k)]
          (println (str "  " (name k) "  " (:id r) "  in " (:entity-id r)))
          (println (str "    passage(60c): "
                        (subs (:passage r) 0 (min 60 (count (:passage r))))
                        (when (> (count (:passage r)) 60) "..."))))))
    ;; companion record of dropped annotations (audit trail; not actionable)
    (let [unmatched (filter #(not= :ok (:status %)) results)
          review-path (str DATA-DIR "/peeragogy-handbook-book-annotations-mw-dropped.md")
          lines (cons (str "# Annotations dropped during .md → .mw migration — "
                           (count unmatched) " of " (count results) " total\n"
                           "\nGenerated by `scripts/peeragogy-migrate-annotations.clj`."
                           "  These annotations did not auto-anchor against the live"
                           " wikitext and were dropped from the new `-mw.el` sidecar."
                           "  Recorded here as an audit trail in case any need to be"
                           " re-introduced manually later.  The dominant reasons are:"
                           " annotation target is an import-side artifact that doesn't"
                           " exist in clean wikitext (mojibake, broken-image-as-text,"
                           " vandalism we cleared), passage is a multi-line condensation"
                           " that doesn't match contiguously, or link string occurs"
                           " ambiguously across the chapter.\n")
                      (for [r unmatched]
                        (str "\n## " (:id r) "\n"
                             "- **Section:** " (:entity-id r) "\n"
                             "- **Status:** " (name (:status r))
                             (when (:hits r) (str " (" (:hits r) " matches)"))
                             (when (:reason r) (str " — " (:reason r)))
                             "\n"
                             "- **Original passage:**\n  > "
                             (-> (:passage r)
                                 (str/replace #"\n" "\n  > "))
                             "\n")))]
      (spit review-path (str/join "" lines))
      (binding [*out* *err*]
        (println (str "[migrate] wrote " review-path))))

    ;; emit the new sidecar: splice :passage for OK matches, drop blocks for non-OK
    (let [src-text (slurp ANNOTATIONS-IN)
          out-text
          (reduce
           (fn [acc r]
             (let [id (:id r)]
               (case (:status r)
                 :ok
                 (let [id-marker (str ":id \"" id "\"")
                       start (.indexOf ^String acc id-marker)]
                   (if (neg? start)
                     (do (binding [*out* *err*]
                           (println (str "[migrate] could not splice " id " — id-marker not found")))
                         acc)
                     (let [passage-key ":passage"
                           source-key ":source"
                           p-start (.indexOf ^String acc passage-key (int start))
                           s-start (.indexOf ^String acc source-key (int start))
                           q1 (.indexOf ^String acc "\"" (int p-start))
                           q2 (loop [i (inc q1)]
                                (if (>= i (count acc)) -1
                                    (let [c (.charAt ^String acc i)]
                                      (cond
                                        (= c \\) (recur (+ i 2))
                                        (= c \") i
                                        :else (recur (inc i))))))]
                       (if (or (neg? p-start) (neg? s-start) (>= p-start s-start)
                               (neg? q1) (neg? q2))
                         (do (binding [*out* *err*]
                               (println (str "[migrate] could not locate :passage span for " id)))
                             acc)
                         (str (subs acc 0 (inc q1))
                              (elisp-escape (:new-passage r))
                              (subs acc q2))))))
                 ;; non-OK: drop the entire annotation block
                 (delete-annotation-block acc id))))
           src-text
           results)
          ;; Also rewrite the file header banner so it advertises mw-anchored
          out-text (-> out-text
                       (str/replace
                        "peeragogy-handbook-book-annotations.el --- Review annotations for Peeragogy Handbook"
                        "peeragogy-handbook-book-annotations-mw.el --- Review annotations (wikitext-anchored) for Peeragogy Handbook")
                       (str/replace
                        "(defconst peeragogy-handbook-book-annotations"
                        "(defconst peeragogy-handbook-book-annotations-mw")
                       (str/replace
                        "(provide 'peeragogy-handbook-book-annotations)"
                        "(provide 'peeragogy-handbook-book-annotations-mw)"))]
      (spit ANNOTATIONS-OUT out-text)
      (binding [*out* *err*]
        (println (str "[migrate] wrote " ANNOTATIONS-OUT))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
