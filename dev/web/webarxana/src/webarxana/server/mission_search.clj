(ns webarxana.server.mission-search
  (:require [clojure.string :as str]
            [futon.missions :as missions]
            [ring.util.response :as resp]))

(defn- parse-int [value fallback]
  (try
    (Integer/parseInt (str value))
    (catch Throwable _
      fallback)))

(defn- futon3a-root []
  (-> (java.io.File. ".")
      .getCanonicalFile
      (.toPath)
      (.resolve "../../../../futon3a")
      .normalize
      str))

(defn- futon3a-path [relative]
  (str (futon3a-root) "/" relative))

(defn search
  [req]
  (let [query (some-> (get-in req [:params :query]) str/trim)
        top-k (parse-int (get-in req [:params :top-k]) 5)
        confidence (or (get-in req [:params :confidence-threshold])
                       (get-in req [:params :confidence_threshold])
                       "0.0")
        agreement-only? (contains? #{"true" "1" "yes"}
                                   (some-> (get-in req [:params :agreement-only])
                                           str/lower-case))
        consumer-id (or (get-in req [:params :consumer-id])
                        "joe-webarxana")]
    (if-not (seq query)
      (-> (resp/response {:ok false :error "Missing query"})
          (resp/status 400))
      (try
        (System/setProperty "EMBED_TEXT_PATH" (futon3a-path "scripts/embed_text.py"))
        (System/setProperty "NOTIONS_PYTHON" (futon3a-path ".venv/bin/python3"))
        (resp/response
         (missions/search-missions
          query
          {:top-k top-k
           :confidence-threshold (Double/parseDouble (str confidence))
           :agreement-only? agreement-only?
           :consumer-id consumer-id
           :records-path (futon3a-path "resources/notions/mission_records.json")
           :mission-embeddings-path (futon3a-path "resources/notions/minilm_mission_embeddings.json")
           :corpus-embeddings-path (futon3a-path "resources/notions/minilm_corpus_embeddings.json")}))
        (catch Throwable t
          (-> (resp/response {:ok false
                              :error (.getMessage t)})
              (resp/status 502)))))))

(defn record-event
  [req]
  (let [{:keys [query-id consumer-id result-id action title]} (:body req)]
    (if (or (str/blank? query-id)
            (str/blank? consumer-id)
            (str/blank? result-id)
            (nil? action))
      (-> (resp/response {:ok false :error "Missing query-id/consumer-id/result-id/action"})
          (resp/status 400))
      (resp/response
       (missions/record-consumer-event!
        {:query-id query-id
         :consumer-id consumer-id
         :result-id result-id
         :action action
         :title title})))))
