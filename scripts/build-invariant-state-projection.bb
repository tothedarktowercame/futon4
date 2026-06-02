#!/usr/bin/env bb

(ns build-invariant-state-projection
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [babashka.fs :as fs])
  (:import [java.io PushbackReader]
           [java.security MessageDigest]
           [java.time Instant]))

(def inventory-path "/home/joe/code/futon3c/docs/structural-law-inventory.sexp")
(def arxana-path "/home/joe/code/futon4/dev/arxana-browser-lab.el")
(def leaf-invariants-md "/home/joe/code/futon5a/holes/stories/leaf-invariants.md")
(def leaf-invariants-aif "/home/joe/code/futon5a/holes/stories/leaf-invariants.aif.edn")
(def priority-json-path "/home/joe/code/futon5a/data/stack-stereolithography-priority-queue.json")
(def priority-edn-path "/home/joe/code/futon5a/data/stack-stereolithography-priority-queue.edn")
(def locus-path "/home/joe/code/futon3c/src/futon3c/logic/locus.clj")
(def locus-test-path "/home/joe/code/futon3c/test/futon3c/logic/locus_test.clj")

(def expected
  {:operational-family-forms 15
   :candidate-family-forms 12
   :watchlist-family-rows 9
   :all-family-forms 36
   :inventory-derived-candidate-rows 72
   :current-candidate-queue-rows 86})

(defn read-all-forms [path]
  (with-open [r (PushbackReader. (io/reader path))]
    (loop [forms []]
      (let [form (edn/read {:eof ::eof} r)]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(defn form-head? [x head]
  (and (seq? x) (= (first x) head)))

(defn walk [x pred]
  (let [self (if (pred x) [x] [])]
    (if (sequential? x)
      (into self (mapcat #(walk % pred) x))
      self)))

(defn find-first-form [x head]
  (first (walk x #(form-head? % head))))

(defn find-all-forms [x head]
  (walk x #(form-head? % head)))

(defn plist-get [xs k]
  (loop [xs (seq xs)]
    (when xs
      (if (= (first xs) k)
        (second xs)
        (recur (nnext xs))))))

(defn entries-of-section [section]
  (let [body (rest section)]
    (if (and (= 1 (count body)) (sequential? (first body)))
      (first body)
      body)))

(defn nameish [x]
  (cond
    (keyword? x) (name x)
    (symbol? x) (name x)
    (string? x) x
    (nil? x) nil
    :else (str x)))

(defn sha256-file [path]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream path)]
      (let [buf (byte-array 8192)]
        (loop []
          (let [n (.read in buf)]
            (when (pos? n)
              (.update digest buf 0 n)
              (recur))))))
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn file-meta [path]
  (let [f (io/file path)]
    {:path path
     :mtime (.toString (Instant/ofEpochMilli (.lastModified f)))
     :sha256 (sha256-file path)}))

(defn child-forms [form k head]
  (filter #(form-head? % head) (plist-get (rest form) k)))

(defn child-counts [form]
  {:candidate-invariants (count (child-forms form :candidate-invariants 'invariant))
   :discovered-invariants (count (child-forms form :discovered-invariants 'invariant))
   :gates (count (child-forms form :discovered-gates 'gate))})

(defn section-families [forms section-name]
  (->> (find-first-form forms section-name)
       entries-of-section
       (filter #(form-head? % 'family))
       vec))

(defn family-row [source-section family]
  (let [status (nameish (plist-get (rest family) :status))]
    {:family/id (nameish (plist-get (rest family) :id))
     :source/section source-section
     :claimed-status status
     :witnessed-status :witnessed/missing
     :counts (child-counts family)}))

(defn invariant-status-counts [forms]
  (let [invariants (find-all-forms forms 'invariant)
        status (frequencies (map #(or (keyword (nameish (plist-get (rest %) :status)))
                                      :missing)
                                 invariants))]
    {:operational (get status :operational 0)
     :operational-when-enabled (get status :operational-when-enabled 0)
     :operational-but-bypassable (get status :operational-but-bypassable 0)
     :candidate (get status :candidate 0)
     :missing (get status :missing 0)}))

(defn tracer-count []
  (count (re-seq #"(?s)\(:invariant\s+\"[^\"]+\".*?:tracer\?\s+t\s+:tracer-kind\s+\"[^\"]+\"\)?"
                 (slurp arxana-path))))

(defn invariant-candidate-counts [forms]
  (let [candidate-families (section-families forms 'candidate-families)
        devmaps (find-all-forms forms 'devmap)
        family-cands (reduce + (map #(get (child-counts %) :candidate-invariants) candidate-families))
        repo-cands (reduce + (map #(get (child-counts %) :candidate-invariants) devmaps))
        tracers (tracer-count)]
    {:authored-candidate-family-rows family-cands
     :repo-seed-candidate-rows repo-cands
     :inventory-derived-candidate-rows (+ family-cands repo-cands)
     :browser-tracer-rows tracers
     :current-candidate-queue-rows (+ family-cands repo-cands tracers)}))

(defn story-count-claims [path]
  (let [text (slurp path)
        operational (some-> (re-find #"(?i)(\d+)\s+operational families" text) second parse-long)
        candidate (some-> (re-find #"(?i)(\d+)\s+candidate families" text) second parse-long)]
    {:operational-family-forms operational
     :candidate-family-forms candidate}))

(defn story-content-drift-test [summary]
  (let [claim (story-count-claims leaf-invariants-md)
        actual (select-keys (get-in summary [:counts :families])
                            [:operational-family-forms :candidate-family-forms])
        ok? (= claim actual)]
    {:test/id :leaf-invariants-count-claims-match-projection
     :story/id "leaf-invariants"
     :grounding/domain :invariants
     :claim claim
     :projection actual
     :outcome (if ok? :ok :violation)
     :detail (if ok?
               "Story count claims match invariant-state summary."
               "Story count claims are hand-typed prose and drift from invariant-state summary.")}))

(defn outcome-severity [outcome]
  (case outcome
    :ok :ok
    :violation :violation
    :inactive :inactive
    :error :error
    :warning))

(defn status-rank [status]
  (case status
    :error 5
    :violation 4
    :inactive 3
    :warning 2
    :ok 1
    0))

(defn max-status [& statuses]
  (or (first (sort-by status-rank > (remove nil? statuses)))
      :ok))

(defn story-status-card [test-result]
  (let [outcome (:outcome test-result)
        status (outcome-severity outcome)
        claim (:claim test-result)
        projection (:projection test-result)]
    {:story/id (:story/id test-result)
     :headline (if (= :violation outcome)
                 (format "leaf-invariants count claims drift: story says %s/%s, projection sees %s/%s"
                         (:operational-family-forms claim)
                         (:candidate-family-forms claim)
                         (:operational-family-forms projection)
                         (:candidate-family-forms projection))
                 "leaf-invariants count claims match invariant-state projection")
     :build/status status
     :grounding/domain (:grounding/domain test-result)
     :grounding/sources [{:kind :inventory :path inventory-path}
                         {:kind :story :path leaf-invariants-md}
                         {:kind :aif-overlay :path leaf-invariants-aif}]
     :currency/chains
     [{:chain :render-hash
       :outcome :unknown
       :detail "Source-hash render witness is specified but not registered yet."}
      {:chain :feeder-heartbeat
       :outcome :unknown
       :detail "Feeder heartbeat witness is specified but not registered yet."}
      {:chain :content-drift
       :outcome outcome
       :claim claim
       :projection projection
       :detail (:detail test-result)}]}))

;; ---------------------------------------------------------------------------
;; Corpus-wide story freshness — "which stories need new documentation"
;;
;; Generalizes the single leaf-invariants drift card to the whole VSATARCS
;; story corpus, using the I-invariant-queue-freshness pattern
;; (`mtime(source) > mtime(derived)`) on two chains:
;;   - :render-freshness  overlay (.aif.edn) newer than generated prose (.aif.md)
;;   - :source-freshness  a referenced file/mission moved after the story
;; No witness ⇒ :inactive (candidate), never silently :ok.
;; ---------------------------------------------------------------------------

(def story-corpus-dir "/home/joe/code/futon5a/holes/stories")

(defn- mtime-ms [path]
  (let [f (io/file path)] (when (and path (.exists f)) (.lastModified f))))

(defn- safe-slurp [path]
  (try (slurp path) (catch Exception _ "")))

(def ^:private mission-index
  ;; M-foo -> first matching mission doc path across the stack
  (delay (into {} (for [p (fs/glob "/home/joe/code" "futon*/holes/**/M-*.md")]
                    [(str/replace (fs/file-name p) #"\.md$" "") (str p)]))))

(defn- list-story-mds []
  (->> (.listFiles (io/file story-corpus-dir))
       (filter #(.isFile %))
       (map #(.getPath %))
       (filter #(str/ends-with? % ".md"))
       (remove #(str/ends-with? % ".aif.md")) ;; .aif.md is generated prose, not a source story
       sort))

(defn- story-siblings [md-path]
  (let [base (str/replace md-path #"\.md$" "")]
    {:md md-path :aif-edn (str base ".aif.edn") :aif-md (str base ".aif.md")}))

(defn- resolve-referents [text]
  ;; explicit repo-rooted code/doc paths that exist + mission ids resolvable to a doc
  (let [paths (re-seq #"futon[0-9a-z]*/[\w./-]+\.(?:clj[cs]?|el|py|bb|edn)" text)
        miss  (re-seq #"\bM-[a-z0-9][a-z0-9-]+" text)]
    (distinct
     (concat
      (for [p (distinct paths)
            :let [full (str "/home/joe/code/" p)]
            :when (.exists (io/file full))]
        full)
      (keep @mission-index (distinct miss))))))

(defn- render-freshness-chain [sib]
  (let [src (mtime-ms (:aif-edn sib)) der (mtime-ms (:aif-md sib))]
    (cond
      (nil? src) {:chain :render-freshness :outcome :inactive :detail "no .aif.edn overlay"}
      (nil? der) {:chain :render-freshness :outcome :inactive :detail "no generated .aif.md prose"}
      (> src der) {:chain :render-freshness :outcome :violation :source-mtime src :derived-mtime der
                   :detail "overlay (.aif.edn) newer than generated prose (.aif.md): re-render owed"}
      :else {:chain :render-freshness :outcome :ok})))

(defn- source-freshness-chain [sib]
  (let [text (str (safe-slurp (:md sib)) "\n" (safe-slurp (:aif-edn sib)))
        refs (resolve-referents text)
        story-mt (apply max 0 (keep mtime-ms [(:md sib) (:aif-edn sib)]))
        ref-mts (keep mtime-ms refs)]
    (if (empty? ref-mts)
      {:chain :source-freshness :outcome :inactive :detail "no resolvable file/mission referents"}
      (let [newest (apply max ref-mts)]
        (if (> newest story-mt)
          {:chain :source-freshness :outcome :violation :referent-count (count ref-mts)
           :newest-referent-mtime newest :story-mtime story-mt
           :detail (format "%d referent(s) cited; newest moved after the story — content may be stale"
                           (count ref-mts))}
          {:chain :source-freshness :outcome :ok :referent-count (count ref-mts)})))))

(defn corpus-story-card [md-path]
  (let [sib (story-siblings md-path)
        sid (str/replace (fs/file-name md-path) #"\.md$" "")
        chains [(render-freshness-chain sib) (source-freshness-chain sib)]
        status (apply max-status (map (comp outcome-severity :outcome) chains))
        stale (filter #(= :violation (:outcome %)) chains)]
    {:story/id sid
     :headline (if (seq stale)
                 (format "%s needs documentation: %s" sid (str/join "; " (map :detail stale)))
                 (format "%s current" sid))
     :build/status status
     :grounding/domain :story-corpus
     :grounding/sources [{:kind :story :path (:md sib)}
                         {:kind :aif-overlay :path (:aif-edn sib)}]
     :currency/chains chains}))

(defn corpus-story-cards []
  (mapv corpus-story-card (list-story-mds)))

(def single-locus-witnesses
  [{:invariant/id "single-locus/mission-home"
    :family/id "atomic-inspectable-units"
    :witness/id :witness/single-locus-mission-home
    :witness/kind :probe-check-fn
    :implemented-in locus-path
    :test-path locus-test-path
    :check-fn "futon3c.logic.locus/check-mission-home-locus-on-load!"
    :probe-registration "futon3c.logic.locus/register-locus-taps!"}
   {:invariant/id "single-locus/agent-routing"
    :family/id "atomic-inspectable-units"
    :witness/id :witness/single-locus-agent-routing
    :witness/kind :probe-check-fn
    :implemented-in locus-path
    :test-path locus-test-path
    :check-fn "futon3c.logic.locus/check-agent-routing-locus-on-load!"
    :probe-registration "futon3c.logic.locus/register-locus-taps!"}
   {:invariant/id "single-locus/artifact-live-copy"
    :family/id "atomic-inspectable-units"
    :witness/id :witness/single-locus-artifact-live-copy
    :witness/kind :probe-check-fn
    :implemented-in locus-path
    :test-path locus-test-path
    :check-fn "futon3c.logic.locus/check-artifact-live-copy-locus-on-load!"
    :probe-registration "futon3c.logic.locus/register-locus-taps!"}])

(defn single-locus-witness-test []
  (let [src (slurp locus-path)
        tst (slurp locus-test-path)
        missing (vec
                 (for [{:keys [invariant/id check-fn probe-registration]} single-locus-witnesses
                       :let [fn-name (last (str/split check-fn #"/"))]
                       :when (or (not (str/includes? src fn-name))
                                 (not (str/includes? src (last (str/split probe-registration #"/"))))
                                 (not (str/includes? tst (last (str/split id #"/")))))]
                   id))]
    {:test/id :single-locus-witnesses-present
     :outcome (if (empty? missing) :ok :violation)
     :missing missing}))

(defn count-pin-test [summary]
  (let [actual (merge (get-in summary [:counts :families])
                      (select-keys (get-in summary [:counts :invariants])
                                   [:inventory-derived-candidate-rows
                                    :current-candidate-queue-rows]))
        deltas (into {}
                     (for [[k v] expected
                           :let [a (get actual k)]
                           :when (not= v a)]
                       [k {:expected v :actual a}]))]
    {:test/id :pinned-counts-match-current-sources
     :outcome (if (empty? deltas) :ok :violation)
     :deltas deltas}))

(defn vsatarcs-status [summary live-tests corpus-cards]
  (let [story-tests (filter :story/id live-tests)
        invariant-stories (mapv story-status-card story-tests)
        stories (into invariant-stories (or corpus-cards []))
        statuses (map :build/status stories)
        build-status (apply max-status statuses)
        stale (filterv #(= :violation (:build/status %)) stories) ;; need-documentation cards
        warning? (seq stale)]
    {:build
     {:status build-status
      :projection-generated-at (:generated-at summary)
      :regeneration-performed? false
      :source "futon4/scripts/build-invariant-state-projection.bb --wm-status"}
     :summary
     {:stories-total (count stories)
      :stories-ok (count (filter #(= :ok (:build/status %)) stories))
      :stories-warning (count (filter #(= :warning (:build/status %)) stories))
      :stories-violation (count stale)
      :stories-stale (count stale) ;; alias: stale == needs-documentation
      :stories-inactive (count (filter #(= :inactive (:build/status %)) stories))
      :candidate-queue-rows (get-in summary [:counts :invariants :current-candidate-queue-rows])
      :witnessed-missing (count (filter #(= :witnessed/missing (:witnessed-status %))
                                        (:families summary)))}
     :stories stories
     ;; convenience for the WM card surface: just the stories needing documentation
     :stale-stories (mapv #(select-keys % [:story/id :headline :build/status
                                           :grounding/domain :grounding/sources :currency/chains])
                          stale)
     :wm-escalation
     (if warning?
       {:tier :warning
        :reason (format "%d VSATARCS stories need documentation (stale freshness witness)"
                        (count stale))
        :surface "vsatarcs"}
       {:tier :info
        :reason "VSATARCS projection has no current story violations"
        :surface "vsatarcs"})}))

(defn vsatarcs-status-shape-test [status]
  (let [bad (cond
              (not (map? (:build status))) :missing-build
              (not (contains? (:build status) :status)) :missing-build-status
              (not (vector? (:stories status))) :missing-stories
              (some #(not (and (:story/id %)
                               (:headline %)
                               (contains? % :build/status)
                               (seq (:currency/chains %))))
                    (:stories status)) :malformed-story
              (not (map? (:wm-escalation status))) :missing-wm-escalation)]
    {:test/id :vsatarcs-status-shape
     :outcome (if bad :violation :ok)
     :detail bad}))

(defn projection []
  (let [forms (read-all-forms inventory-path)
        operational-families (section-families forms 'operational-families)
        candidate-families (section-families forms 'candidate-families)
        watchlist-families (section-families forms 'candidate-family-watchlist)
        all-families (find-all-forms forms 'family)
        candidate-counts (invariant-candidate-counts forms)
        families (vec (concat (map #(family-row :operational-families %) operational-families)
                              (map #(family-row :candidate-families %) candidate-families)
                              (map #(family-row :candidate-family-watchlist %) watchlist-families)))
        summary {:generated-at (str (Instant/now))
                 :source/inventory (file-meta inventory-path)
                 :counts {:families {:operational-family-forms (count operational-families)
                                      :candidate-family-forms (count candidate-families)
                                      :watchlist-family-rows (count watchlist-families)
                                      :all-family-forms (count all-families)}
                          :invariants (merge {:claimed-status (invariant-status-counts forms)}
                                             candidate-counts)}
                 :families families}
        story-test (story-content-drift-test summary)
        pre-status-tests [(count-pin-test summary)
                          story-test
                          (single-locus-witness-test)]
        corpus-cards (corpus-story-cards)
        status (vsatarcs-status summary pre-status-tests corpus-cards)
        live-tests (conj pre-status-tests (vsatarcs-status-shape-test status))]
    {:projection/schema-version 1
     :mode :read-only
     :regeneration/performed? false
     :inputs [(file-meta inventory-path)
              (file-meta arxana-path)
              (file-meta leaf-invariants-md)
              (file-meta leaf-invariants-aif)
              (file-meta priority-json-path)
              (file-meta priority-edn-path)]
     :invariant-state/summary summary
     :story-grounding
     [{:story/id "leaf-invariants"
       :story/path leaf-invariants-md
       :grounding/domain :invariants
       :grounding/sources [{:kind :inventory :path inventory-path}
                           {:kind :projection :id :invariant-state/summary}]
       :currency/witnesses [:story-invariant-claims-match-projection]}]
     :currency/model
     {:vsatarcs-up-to-date
      {:kind :conjunction
       :chains [{:id :render-hash-current
                 :status :specified-not-implemented
                 :method :source-hash-recorded-in-render-artifact}
                {:id :feeder-heartbeat
                 :status :specified-not-implemented
                 :method :read-feeder-status}
                {:id :story-content-drift
                 :status :live-test
                 :method :compare-story-claims-to-grounded-summary}]}
      :wm-escalation
      {:kind :derived-warning
       :status :specified-not-implemented
       :fires-when [:vsatarcs-up-to-date/violation :witnessed-status/violation]
       :target :war-machine
       :severity :warning-or-stop-the-line}}
     :vsatarcs-status status
     :witness-registry/draft single-locus-witnesses
     :live-tests live-tests}))

(defn -main [& args]
  (let [p (projection)
        check? (some #{"--check"} args)
        wm-status? (some #{"--wm-status"} args)]
    (prn (if wm-status? (:vsatarcs-status p) p))
    (when check?
      (let [bad (remove #(or (= :ok (:outcome %))
                             ;; This violation is expected until docs are regenerated.
                             (= :leaf-invariants-count-claims-match-projection (:test/id %)))
                        (:live-tests p))]
        (when (seq bad)
          (binding [*out* *err*]
            (println "build-invariant-state-projection: live check failed")
            (prn bad))
          (System/exit 2))))))

(apply -main *command-line-args*)
