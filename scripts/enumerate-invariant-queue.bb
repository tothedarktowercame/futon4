#!/usr/bin/env bb

(ns enumerate-invariant-queue
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io PushbackReader]
           [java.security MessageDigest]
           [java.time Instant]))

(def inventory-path "/home/joe/code/futon3c/docs/structural-law-inventory.sexp")
(def arxana-path "/home/joe/code/futon4/dev/arxana-browser-lab.el")
(def priority-json-path "/home/joe/code/futon5a/data/stack-stereolithography-priority-queue.json")
(def priority-edn-path "/home/joe/code/futon5a/data/stack-stereolithography-priority-queue.edn")

(def expected-counts
  {:authored-candidate-family-rows 47
   :repo-seed-candidate-rows 25
   :inventory-derived-candidate-rows 72
   :watchlist-metadata-rows 9
   :browser-tracer-rows 14
   :current-browser-item-rows 86
   :priority-json-runs 68
   :priority-edn-runs 67})

(defn read-all-forms [path]
  (with-open [r (PushbackReader. (io/reader path))]
    (loop [forms []]
      (let [form (try
                   (edn/read {:eof ::eof} r)
                   (catch Exception e
                     (throw (ex-info (str "Cannot read form from " path)
                                     {:path path}
                                     e))))]
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

(defn child-invariants [form k]
  (filter #(form-head? % 'invariant) (plist-get (rest form) k)))

(defn nameish [x]
  (cond
    (keyword? x) (name x)
    (symbol? x) (name x)
    (string? x) x
    (nil? x) nil
    :else (str x)))

(defn priority-key [family invariant source]
  (str (or family "") "|" (or invariant "") "|" (or source "")))

(def witnessed-candidates
  #{["atomic-inspectable-units" "mission-home"]
    ["atomic-inspectable-units" "agent-routing"]
    ["atomic-inspectable-units" "artifact-live-copy"]})

(defn audit-default [family-id inv-id]
  (if (contains? witnessed-candidates [family-id inv-id])
    {:audit/value :already-witnessed
     :audit/decision :register-witness
     :witness/status :known-present}
    {:audit/value :needs-more-evidence
     :audit/decision :defer}))

(defn lucid-trigger [family-id inv-id]
  (str "The queue shows `" family-id "/" inv-id "` as a possible invariant, "
       "but the stack does not yet render what changes when it is enforced."))

(defn lucid-operator-outcome [family-id inv-id]
  (case [family-id inv-id]
    ["atomic-inspectable-units" "checkout-before-work"]
    "Joe can see, before an agent starts, whether the work has a declared checkout point and therefore whether later artifacts have custody."

    ["atomic-inspectable-units" "checkin-on-exit"]
    "Joe can see whether an agent has actually closed the loop instead of leaving a private scratch state behind."

    ["artifact-custody" "generated-vs-source-separation"]
    "Joe can distinguish authored source, generated projections, and disposable scratch without reading the whole tree."

    ["repo-role-clarity" "declared-role"]
    "Joe can glance at a repo and know why it exists in the stack before asking an agent to modify it."

    (str "Joe can decide whether `" family-id "/" inv-id
         "` is worth turning into a witnessed invariant, because the imagined payoff is explicit.")))

(defn lucid-agent-outcome [family-id inv-id]
  (case [family-id inv-id]
    ["atomic-inspectable-units" "checkout-before-work"]
    "I know where my work begins, which files are live, and which invariant will judge the boundary."

    ["atomic-inspectable-units" "checkin-on-exit"]
    "I leave a visible handoff: what changed, what remains open, and which artifacts should be trusted."

    ["artifact-custody" "generated-vs-source-separation"]
    "I avoid editing generated surfaces by mistake and can route fixes back to source."

    ["repo-role-clarity" "declared-role"]
    "I can infer the repo's role before proposing changes, instead of reverse-engineering its purpose from incidental files."

    (str "I can feel what constraint `" family-id "/" inv-id
         "` would add before anyone spends effort building it.")))

(defn lucid-dream [row]
  (let [family-id (:family/id row)
        inv-id (:invariant/id row)]
    {:lucid/type :war-machine-lucid-dream
     :lucid/status :imagined
     :lucid/title (str family-id " / " inv-id)
     :lucid/trigger (lucid-trigger family-id inv-id)
     :lucid/operator-outcome (lucid-operator-outcome family-id inv-id)
     :lucid/agent-outcome (lucid-agent-outcome family-id inv-id)}))

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

(defn family-candidate-rows [forms]
  (let [section (find-first-form forms 'candidate-families)
        families (filter #(form-head? % 'family) (entries-of-section section))]
    (vec
     (for [family families
           :let [family-id (nameish (plist-get (rest family) :id))]
           inv (child-invariants family :candidate-invariants)
           :let [inv-id (nameish (plist-get (rest inv) :id))]]
       (let [base {:candidate/id (str "candidate-families/" family-id "/" inv-id)
                   :normalized/key (priority-key family-id inv-id nil)
                   :source/feed :candidate-families
                   :source/path inventory-path
                   :row/class :invariant-candidate
                   :family/id family-id
                   :invariant/id inv-id
                   :origin :family-definition
                   :priority/key (priority-key family-id inv-id "family-definition")}
             audited (merge base (audit-default family-id inv-id))]
         (assoc audited :lucid/dream (lucid-dream audited)))))))

(defn repo-seed-candidate-rows [forms]
  (vec
   (for [devmap (find-all-forms forms 'devmap)
         :let [repo-id (nameish (plist-get (rest devmap) :id))]
         inv (child-invariants devmap :candidate-invariants)
         :let [family-id (nameish (plist-get (rest inv) :family))
               inv-id (nameish (plist-get (rest inv) :id))]]
     (let [base {:candidate/id (str "repo-seeds/" repo-id "/" family-id "/" inv-id)
                 :normalized/key (priority-key family-id inv-id nil)
                 :source/feed :repo-seeds
                 :source/path inventory-path
                 :source/repo repo-id
                 :row/class :invariant-candidate
                 :family/id family-id
                 :invariant/id inv-id
                 :origin :repo-seed
                 :priority/key (priority-key family-id inv-id "repo-seed")}
           audited (merge base (audit-default family-id inv-id))]
       (assoc audited :lucid/dream (lucid-dream audited))))))

(defn watchlist-rows [forms]
  (let [section (find-first-form forms 'candidate-family-watchlist)
        families (filter #(form-head? % 'family) (entries-of-section section))]
    (vec
     (for [family families
           :let [family-id (nameish (plist-get (rest family) :id))]]
       {:candidate/id (str "watchlist/" family-id)
        :normalized/key (priority-key family-id nil nil)
        :source/feed :candidate-family-watchlist
        :source/path inventory-path
        :row/class :watchlist-metadata
        :family/id family-id
        :invariant/id nil
        :audit/value :metadata-only
        :audit/decision :exclude-from-row-denominator}))))

(defn re-field [body field]
  (second (re-find (re-pattern (str ":" field "\\s+\"([^\"]+)\"")) body)))

(defn tracer-rows [path]
  (let [text (slurp path)
        pattern #"(?s)\(:invariant\s+\"([^\"]+)\"(.*?):tracer\?\s+t\s+:tracer-kind\s+\"([^\"]+)\"\)?"
        matches (re-seq pattern text)]
    (vec
     (for [[_ invariant body kind] matches
           :let [family (re-field body "family")
                 klass (case kind
                         "pipeline-tracer" :work-tracer
                         "deferred-stub" :deferred-witness-gap
                         :work-tracer)
                 [audit-value audit-decision] (case klass
                                                :work-tracer [:work-item-not-invariant :convert-to-work-item]
                                                :deferred-witness-gap [:needs-more-evidence :needs-witness]
                                                [:needs-more-evidence :defer])]]
       {:candidate/id (str "tracer/" kind "/" family "/" invariant)
        :normalized/key (priority-key family invariant nil)
        :source/feed :browser-local-tracer
        :source/path path
        :row/class klass
        :family/id family
        :invariant/id invariant
        :tracer/kind kind
        :audit/value audit-value
        :audit/decision audit-decision}))))

(defn priority-json-runs [path]
  (:runs (json/parse-string (slurp path) true)))

(defn priority-edn-runs [path]
  (:runs (edn/read-string (slurp path))))

(defn priority-index [runs]
  (into {}
        (map-indexed
         (fn [idx run]
           (let [source (nameish (:run/source run))
                 family (nameish (:family/id run))
                 invariant (nameish (:invariant/id run))]
             [(priority-key family invariant source)
              {:priority/rank (inc idx)
               :priority/score (:priority/score run)
               :priority/source source
               :run/id (:run/id run)
               :family/id family
               :invariant/id invariant}]))
         runs)))

(defn join-priority [rows pidx]
  (mapv (fn [row]
          (if-let [p (get pidx (:priority/key row))]
            (merge row p {:priority/join :matched})
            (assoc row :priority/join :missing)))
        rows))

(defn priority-only-rows [pidx rows]
  (let [row-keys (set (keep :priority/key rows))]
    (vec
     (for [[k p] pidx
           :when (not (contains? row-keys k))]
       (merge {:candidate/id (str "priority-only/" k)
               :normalized/key (priority-key (:family/id p) (:invariant/id p) nil)
               :source/feed :priority-json
               :source/path priority-json-path
               :row/class :stale-priority-only
               :priority/key k
               :priority/join :priority-only
               :audit/value :needs-more-evidence
               :audit/decision :defer}
              p)))))

(defn duplicate-normalized-keys [rows]
  (->> rows
       (group-by :normalized/key)
       (filter (fn [[k rs]] (and (seq k) (< 1 (count rs)))))
       (mapv (fn [[k rs]] {:normalized/key k
                           :count (count rs)
                           :candidate/ids (mapv :candidate/id rs)}))))

(defn missing-normalized-ids [rows]
  (filterv #(or (str/blank? (:candidate/id %))
                (str/blank? (:normalized/key %))
                (str/blank? (:family/id %))
                (and (not= (:row/class %) :watchlist-metadata)
                     (str/blank? (:invariant/id %))))
           rows))

(defn count-status [counts]
  (into {}
        (for [[k expected] expected-counts
              :let [actual (get counts k)]]
          [k {:expected expected
              :actual actual
              :status (if (= expected actual) :matches :changed)}])))

(defn md-scene [idx row]
  (let [{:lucid/keys [title trigger operator-outcome agent-outcome]} (:lucid/dream row)
        suffix (-> (:normalized/key row)
                   (str/replace #"[^A-Za-z0-9]+" "-")
                   (str/replace #"^-+" "")
                   (str/replace #"-+$" ""))
        anchor (str "candidate-" idx "-" suffix)]
    (str "## Scene: " title " | " anchor "\n\n"
         "*Authored:* 2026-06-02 · codex projection seed\n"
         "*Status:* `:imagined`\n"
         "*Queue row:* `" (:candidate/id row) "`\n"
         "*Audit decision:* `" (:audit/decision row) "`\n"
         "*Trigger:* " trigger "\n"
         "*Imagined operator move:* Joe opens the VSATARCS stale/candidate card and asks whether this imagined invariant would change live work.\n"
         "*Imagined WM response:* The War Machine shows the dream as prospective, not true: the card names the payoff, the missing witness, and the next audit decision.\n"
         "*Operator outcome:* " operator-outcome "\n"
         "*Agent outcome:* " agent-outcome "\n\n"
         "I imagine this invariant as a pressure the stack might one day make "
         "legible. It is not a current fact just because it appears in the "
         "Candidate Queue. It earns preservation only if this imagined operator "
         "or agent experience is valuable enough to justify a witness.\n\n"
         "---\n\n")))

(defn lucid-dreams-md [result]
  (let [rows (->> (:rows result)
                  (filter #(= :invariant-candidate (:row/class %)))
                  (remove #(= :register-witness (:audit/decision %)))
                  (take 8))]
    (str "# War Machine -- Lucid Dreams for Candidate Invariants\n\n"
         "**Type:** `:lucid-dreams` (candidate invariants imagined before they are built)\n"
         "**Status:** `:projection-seed` -- generated from the invariant queue audit; not a live invariant surface.\n"
         "**Source:** `futon4/scripts/enumerate-invariant-queue.bb --lucid-dreams`\n\n"
         "These scenes preserve the Candidate Queue as fiction in the useful sense: "
         "each row imagines what the stack would feel like to Joe or to an agent "
         "if the invariant existed. A scene is not evidence that the invariant is "
         "worth building; it is a prompt for deciding whether it has value.\n\n"
         "---\n\n"
         (apply str (map-indexed (fn [idx row] (md-scene (inc idx) row)) rows)))))

(defn result []
  (let [forms (read-all-forms inventory-path)
        priority-json (priority-json-runs priority-json-path)
        priority-edn (priority-edn-runs priority-edn-path)
        pidx (priority-index priority-json)
        family-rows (family-candidate-rows forms)
        repo-rows (repo-seed-candidate-rows forms)
        regular-rows (vec (concat family-rows repo-rows))
        watch-rows (watchlist-rows forms)
        tracers (tracer-rows arxana-path)
        browser-rows (vec (concat regular-rows tracers))
        joined-regular (join-priority regular-rows pidx)
        priority-only (priority-only-rows pidx regular-rows)
        rows (vec (concat joined-regular tracers watch-rows priority-only))
        counts {:authored-candidate-family-rows (count family-rows)
                :repo-seed-candidate-rows (count repo-rows)
                :inventory-derived-candidate-rows (count regular-rows)
                :watchlist-metadata-rows (count watch-rows)
                :browser-tracer-rows (count tracers)
                :current-browser-item-rows (count browser-rows)
                :priority-json-runs (count priority-json)
                :priority-edn-runs (count priority-edn)
                :priority-json-matched (count (filter #(= :matched (:priority/join %)) joined-regular))
                :priority-json-missing (count (filter #(= :missing (:priority/join %)) joined-regular))
                :priority-json-only (count priority-only)}
        result {:generated-at (str (Instant/now))
                :inputs (mapv file-meta [inventory-path arxana-path priority-json-path priority-edn-path])
                :counts counts
                :count/status (count-status counts)
                :duplicates (duplicate-normalized-keys browser-rows)
                :malformed (missing-normalized-ids browser-rows)
                :rows rows}]
    result))

(defn -main [& args]
  (let [result (result)]
    (if (some #{"--lucid-dreams"} args)
      (print (lucid-dreams-md result))
      (prn result))
    (when (or (seq (:malformed result))
              (some (fn [[_ v]] (= :changed (:status v))) (:count/status result)))
      (binding [*out* *err*]
        (println "enumerate-invariant-queue: count or row-shape changed; inspect :count/status and :malformed"))
      (System/exit 2))))

(apply -main *command-line-args*)
