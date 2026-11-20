;; Check a causal model's declared (in)dependence claims with the DiagramProver
;; causal engine in futon3c (dag/load-spec, dsep/d-separated?, surgery/do-intervention).
;;   bb -cp ~/code/futon3c/src causal/check.clj causal/r20-blend-v0.json
;; Each check: x, y, given (conditioning set), optional do (variables set by the
;; experimenter: their incoming arrows are cut first), expected (true = the graph
;; must d-separate x from y). Exit 1 if any non-control check fails, or if a
;; check with status "control" unexpectedly holds.
(require '[cheshire.core :as json]
         '[clojure.string :as str]
         '[futon3c.diagramprover.causal.dag :as dag]
         '[futon3c.diagramprover.causal.dsep :as dsep]
         '[futon3c.diagramprover.causal.surgery :as surgery])

(let [path (first *command-line-args*)
      raw (json/parse-string (slurp path) true)
      graph (dag/load-spec path)                       ; throws on unknown ids or a cycle
      known (set (keys (:variables graph)))
      kw #(keyword %)
      by-status (frequencies (map :status (:arrows raw)))
      results
      (mapv (fn [{:keys [id x y given expected status claim] :as c}]
              (let [dos (map kw (:do c))
                    nodes (concat [(kw x) (kw y)] (map kw given) dos)]
                (when-not (and (boolean? expected) (every? known nodes))
                  (throw (ex-info "Invalid check" {:id id})))
                (let [g (reduce surgery/do-intervention graph dos)
                      sep (dsep/d-separated? g (kw x) (kw y) (set (map kw given)))
                      witness (when-not sep
                                (first (:paths (dsep/connecting-paths g (kw x) (kw y)
                                                                      (set (map kw given)) {:limit 1}))))]
                  {:id id :claim claim :status status :expected expected :separated sep
                   :holds (= expected sep) :control (= status "control")
                   :query (str x " _||_ " y
                               (when (seq given) (str " | " (str/join ", " given)))
                               (when (seq dos) (str "  under do(" (str/join ", " (map name dos)) ")")))
                   :witness witness})))
            (:checks raw))
      bad (filter #(if (:control %) (:holds %) (not (:holds %))) results)]
  (println (format "%s: %d variables, %d arrows %s; acyclic, all references known"
                   (get-in raw [:meta :model_id]) (count known) (count (:arrows raw)) (pr-str by-status)))
  (doseq [r results]
    (println (format "%-3s %-9s %-62s graph says %-11s %s"
                     (:id r)
                     (cond (and (:control r) (not (:holds r))) "CONTROL"
                           (:holds r) "holds"
                           :else "FAILS")
                     (:query r)
                     (if (:separated r) "separated" "connected")
                     (if (:witness r) (str "via " (str/join " - " (map name (:witness r)))) ""))))
  (println (if (seq bad) (str "MODEL CHECK FAILED: " (mapv :id bad)) "MODEL CHECK OK"))
  (System/exit (if (seq bad) 1 0)))
