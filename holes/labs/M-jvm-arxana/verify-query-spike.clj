(require '[xtdb.api :as xt]
         '[xtdb.node :as xtn])

(def move-count 2454)
(def manifestation-count 636)

(def moves
  (mapv (fn [i]
          {:xt/id (format "move:%04d" i)
           :proof/problem-id (format "problem:%03d" (mod i 347))
           :proof/ordinal i})
        (range move-count)))

(def manifestations
  (mapv (fn [i]
          {:xt/id (format "manifestation:%04d" i)
           :proof/move-id (format "move:%04d" i)
           :manifestation/role (if (even? i) :authored-prose :lean-source)
           :artifact/revision-id (format "arxana:revision:sha256:%064x" i)})
        (range manifestation-count)))

;; A materialized, evidence-derived status is the bounded query waist. It does
;; not own order or score: the client supplies :proof/ordinal, while Arxana
;; derives only whether the move has absent or ambiguous correspondence.
(def statuses
  (mapv (fn [i]
          {:xt/id (format "move-status:%04d" i)
           :proof/move-id (format "move:%04d" i)
           :proof/ordinal i
           :correspondence/state
           (cond
             (= i 17) :ambiguous
             (< i manifestation-count) :present
             :else :absent)})
        (range move-count)))

(defn ingest! [node table docs]
  (doseq [batch (partition-all 500 docs)]
    (xt/execute-tx node [(into [:put-docs table] batch)])))

(defn elapsed-ms [f]
  (let [start (System/nanoTime)
        value (f)]
    {:milliseconds (/ (double (- (System/nanoTime) start)) 1000000.0)
     :value value}))

(with-open [node (xtn/start-node)]
  (ingest! node :proof-moves moves)
  (ingest! node :manifestations manifestations)
  (ingest! node :proof-move-status statuses)
  ;; Warm each stable query shape once; runtime values are parameters in the
  ;; service implementation, rather than distinct compiled expressions.
  (let [all-form '(-> (from :manifestations [xt/id proof/move-id manifestation/role artifact/revision-id])
                      (where (= proof/move-id "move:0017")))
        next-form '(-> (from :proof-move-status [proof/move-id proof/ordinal correspondence/state])
                       (where (or (= correspondence/state :absent)
                                  (= correspondence/state :ambiguous)))
                       (order-by {:val proof/ordinal :dir :asc})
                       (limit 1))
        _ (xt/q node all-form)
        _ (xt/q node next-form)
        all-result (elapsed-ms #(vec (xt/q node all-form)))
        next-result (elapsed-ms #(vec (xt/q node next-form)))
        result {:fixture {:moves move-count
                          :problems 347
                          :manifestations manifestation-count}
                :all-manifestations all-result
                :next-unresolved next-result
                :checks {:all-manifestations-correct?
                         (= #{{:xt/id "manifestation:0017"
                               :proof/move-id "move:0017"
                               :manifestation/role :lean-source
                               :artifact/revision-id
                               (format "arxana:revision:sha256:%064x" 17)}}
                            (set (:value all-result)))
                         :domain-order-preserved?
                         (= [{:proof/move-id "move:0017"
                              :proof/ordinal 17
                              :correspondence/state :ambiguous}]
                            (:value next-result))}}]
    (prn result)
    (when-not (every? true? (vals (:checks result)))
      (System/exit 1))))
