#!/usr/bin/env bb
;; Export MUSN sessions from futon3 to Arxana Lab format
;;
;; Usage:
;;   bb scripts/musn-to-lab.clj <session-id>           # Export one session
;;   bb scripts/musn-to-lab.clj --all                  # Export all sessions
;;   bb scripts/musn-to-lab.clj --recent 5             # Export 5 most recent
;;
;; Reads from: ../futon3/lab/sessions/*.edn
;; Writes to:  ./lab/raw/*.json

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def futon3-sessions-dir
  (io/file (System/getProperty "user.dir") "../futon3/lab/sessions"))

(def lab-raw-dir
  (io/file (System/getProperty "user.dir") "lab/raw"))

;; Ensure output directory exists
(when-not (.exists lab-raw-dir)
  (.mkdirs lab-raw-dir))

(defn read-session [path]
  (try
    (edn/read-string (slurp path))
    (catch Exception e
      (println "Error reading" path ":" (.getMessage e))
      nil)))

(defn inst->iso [inst]
  (when inst
    (str inst)))

(defn extract-timestamps [events]
  (let [times (->> events
                   (map :at)
                   (filter some?)
                   (map inst->iso)
                   sort)]
    {:start (first times)
     :end (last times)}))

(defn extract-files [events]
  (->> events
       (keep :file)
       distinct
       vec))

(defn event->message
  "Convert a MUSN event to a Lab message format."
  [event idx]
  (let [event-type (:event/type event)
        at (inst->iso (:at event))
        payload (:payload event)]
    (case event-type
      ;; Turn events (if we have them)
      :turn/user
      {:id (str "turn-user-" idx)
       :timestamp at
       :text (get-in payload [:content] "")
       :role "user"}

      :turn/agent
      {:id (str "turn-agent-" idx)
       :timestamp at
       :text (get-in payload [:content] "")
       :role "assistant"}

      ;; Code edit
      :code/edit
      {:id (str "code-edit-" idx)
       :timestamp at
       :text (format "[Code Edit] %s\nFile: %s\nFunction: %s\nAction: %s"
                     (or (:description event) "")
                     (or (:file event) "?")
                     (or (:fn event) "?")
                     (or (:action event) "?"))
       :role "system"
       :event-type "code/edit"
       :file (:file event)
       :function (:fn event)}

      ;; Pattern Selection Record
      :pattern/selection-claimed
      (let [psr (get-in payload [:psr])]
        {:id (or (:psr/id psr) (str "psr-" idx))
         :timestamp at
         :text (format "[PSR] Selected: %s\nCandidates: %s\nRejections: %s\nContext: %s"
                       (or (:chosen psr) "?")
                       (str/join ", " (or (:candidates psr) []))
                       (str/join ", " (keys (or (:rejections psr) {})))
                       (get-in psr [:context] ""))
         :role "psr"
         :event-type "pattern/selection-claimed"
         :pattern-id (:chosen psr)
         :candidates (:candidates psr)
         :forecast (:forecast psr)
         :rejections (:rejections psr)})

      ;; Pattern Use Record
      :pattern/use-claimed
      (let [pur (get-in payload [:pur])
            fields (:fields pur)]
        {:id (or (:pur/id pur) (str "pur-" idx))
         :timestamp at
         :text (format "[PUR] Pattern: %s\nContext: %s\nIF: %s\nHOWEVER: %s\nTHEN: %s\nBECAUSE: %s\nOutcome: %s"
                       (or (:pattern/id pur) "?")
                       (or (:context fields) "")
                       (or (:if fields) "")
                       (or (:however fields) "")
                       (or (:then fields) "")
                       (or (:because fields) "")
                       (str/join ", " (map name (or (:outcome/tags pur) []))))
         :role "pur"
         :event-type "pattern/use-claimed"
         :pattern-id (:pattern/id pur)
         :fields fields
         :anchors (:anchors pur)
         :outcome-tags (:outcome/tags pur)})

      ;; Verification results
      :pattern/selection-verified
      {:id (str "psr-verified-" idx)
       :timestamp at
       :text (format "[PSR Verified] %s - %s"
                     (or (get-in payload [:psr/id]) "?")
                     (name (or (get-in payload [:check/status]) :unknown)))
       :role "system"
       :event-type "pattern/selection-verified"
       :check-status (get-in payload [:check/status])}

      :pattern/use-verified
      {:id (str "pur-verified-" idx)
       :timestamp at
       :text (format "[PUR Verified] %s - %s"
                     (or (get-in payload [:pur/id]) "?")
                     (name (or (get-in payload [:check/status]) :unknown)))
       :role "system"
       :event-type "pattern/use-verified"
       :check-status (get-in payload [:check/status])}

      ;; AIF summary
      :aif/summary
      (let [result (:aif/result payload)
            aif (:aif result)]
        {:id (str "aif-" idx)
         :timestamp at
         :text (format "[AIF %s] G=%.2f tau=%.2f pred-error=%.2f"
                       (name (or (:aif/kind payload) :unknown))
                       (or (:G-chosen aif) (:prediction-error aif) 0.0)
                       (or (:tau aif) (:tau-updated aif) 1.0)
                       (or (:prediction-error aif) 0.0))
         :role "aif"
         :event-type "aif/summary"
         :aif-kind (:aif/kind payload)
         :aif-data aif})

      ;; Default: generic event
      {:id (str "event-" idx)
       :timestamp at
       :text (format "[%s] %s"
                     (name (or event-type :unknown))
                     (pr-str (select-keys event [:description :payload])))
       :role "system"
       :event-type (name (or event-type :unknown))})))

(defn session->lab-json
  "Convert a MUSN session to Arxana Lab JSON format."
  [session]
  (let [session-id (:session/id session)
        agent (some-> (:session/agent session) name)
        events (:events session)
        timestamps (extract-timestamps events)
        files (extract-files events)
        messages (map-indexed (fn [idx ev] (event->message ev idx)) events)
        ;; Separate by role for Lab format
        user-msgs (filter #(= "user" (:role %)) messages)
        assistant-msgs (filter #(= "assistant" (:role %)) messages)
        ;; All messages for extended timeline
        all-msgs (vec messages)]
    {:lab/session-id session-id
     :lab/agent agent
     :lab/timestamp-start (:start timestamps)
     :lab/timestamp-end (:end timestamps)
     :lab/files-touched files
     :lab/user-messages (vec user-msgs)
     :lab/assistant-messages (vec assistant-msgs)
     ;; Extended fields for rich rendering
     :lab/all-events all-msgs
     :lab/event-count (count events)
     :lab/has-psr (some #(= :pattern/selection-claimed (:event/type %)) events)
     :lab/has-pur (some #(= :pattern/use-claimed (:event/type %)) events)
     :lab/has-aif (some #(= :aif/summary (:event/type %)) events)
     ;; Trace path for linking
     :lab/trace-path (format "lab/trace/%s.org" session-id)
     :lab/source "musn/futon3"}))

(defn export-session! [session-id]
  (let [src-file (io/file futon3-sessions-dir (str session-id ".edn"))
        dst-file (io/file lab-raw-dir (str session-id ".json"))]
    (if-not (.exists src-file)
      (println "Session not found:" session-id)
      (let [session (read-session src-file)]
        (when session
          (let [lab-json (session->lab-json session)]
            (spit dst-file (json/generate-string lab-json {:pretty true}))
            (println "Exported:" session-id "->" (.getPath dst-file))))))))

(defn list-sessions []
  (->> (.listFiles futon3-sessions-dir)
       (filter #(str/ends-with? (.getName %) ".edn"))
       (map #(.getName %))
       (map #(str/replace % ".edn" ""))
       sort))

(defn export-all! []
  (doseq [sid (list-sessions)]
    (export-session! sid)))

(defn export-recent! [n]
  (let [sessions (->> (.listFiles futon3-sessions-dir)
                      (filter #(str/ends-with? (.getName %) ".edn"))
                      (sort-by #(.lastModified %) >)
                      (take n)
                      (map #(str/replace (.getName %) ".edn" "")))]
    (doseq [sid sessions]
      (export-session! sid))))

;; CLI
(let [args *command-line-args*]
  (cond
    (empty? args)
    (println "Usage: musn-to-lab.clj <session-id> | --all | --recent N")

    (= "--all" (first args))
    (export-all!)

    (= "--recent" (first args))
    (export-recent! (parse-long (or (second args) "10")))

    :else
    (export-session! (first args))))
