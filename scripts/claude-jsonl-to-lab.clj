#!/usr/bin/env bb
;; Import Claude Code JSONL transcripts to Arxana Lab format
;;
;; Usage:
;;   bb scripts/claude-jsonl-to-lab.clj <path-to-jsonl> [session-name]
;;
;; Example:
;;   bb scripts/claude-jsonl-to-lab.clj ~/.claude/projects/.../abc123.jsonl my-session

(require '[cheshire.core :as json]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def lab-raw-dir
  (io/file (System/getProperty "user.dir") "lab/raw"))

(when-not (.exists lab-raw-dir)
  (.mkdirs lab-raw-dir))

(defn parse-jsonl-line [line]
  (try
    (json/parse-string line true)
    (catch Exception _ nil)))

(defn extract-timestamp [entry]
  (or (:timestamp entry)
      (get-in entry [:message :timestamp])
      ""))

(defn extract-text-content
  "Extract plain text from Claude Code message content.
   Content can be a string, or an array of blocks with types like:
   - {:type \"text\" :text \"...\"}
   - {:type \"thinking\" :thinking \"...\"}
   - {:type \"tool_use\" ...}"
  [content]
  (cond
    (string? content) content
    (sequential? content)
    (->> content
         (filter #(= "text" (:type %)))
         (map :text)
         (str/join "\n"))
    :else ""))

(defn truncate-text [text max-len]
  (if (> (count text) max-len)
    (str (subs text 0 (- max-len 3)) "...")
    text))

(defn entry->message [entry idx]
  (let [msg-type (:type entry)
        message (:message entry)]
    (case msg-type
      ;; User message
      "user"
      (let [content (extract-text-content (or (:content message) message ""))]
        (when (seq content)
          {:id (str "user-" idx)
           :timestamp (extract-timestamp entry)
           :text (truncate-text content 500)
           :role "user"}))

      ;; Assistant message
      "assistant"
      (let [content (extract-text-content (or (:content message) message ""))]
        (when (seq content)
          {:id (str "assistant-" idx)
           :timestamp (extract-timestamp entry)
           :text (truncate-text content 500)
           :role "assistant"}))

      ;; Tool use
      "tool_use"
      {:id (str "tool-" idx)
       :timestamp (extract-timestamp entry)
       :text (format "[Tool: %s]\n%s"
                     (or (:name message) "unknown")
                     (truncate-text (pr-str (or (:input message) {})) 300))
       :role "tool"
       :tool-name (:name message)}

      ;; Tool result
      "tool_result"
      {:id (str "tool-result-" idx)
       :timestamp (extract-timestamp entry)
       :text (format "[Tool Result]\n%s"
                     (truncate-text (str (or (:content message) "")) 500))
       :role "tool-result"}

      ;; Summary (context compaction)
      "summary"
      {:id (str "summary-" idx)
       :timestamp (extract-timestamp entry)
       :text "[Context Summary - conversation compacted]"
       :role "system"}

      ;; Default
      nil)))

(defn import-jsonl! [jsonl-path session-name]
  (let [lines (line-seq (io/reader jsonl-path))
        entries (->> lines
                     (map parse-jsonl-line)
                     (filter some?))
        messages (->> entries
                      (map-indexed (fn [idx entry] (entry->message entry idx)))
                      (filter some?)
                      vec)
        user-msgs (filterv #(= "user" (:role %)) messages)
        assistant-msgs (filterv #(= "assistant" (:role %)) messages)
        timestamps (->> messages (map :timestamp) (filter seq) sort)
        session-id (or session-name
                       (-> jsonl-path io/file .getName (str/replace ".jsonl" "")))
        lab-json {:lab/session-id session-id
                  :lab/agent "claude"
                  :lab/source "claude-code/jsonl"
                  :lab/timestamp-start (first timestamps)
                  :lab/timestamp-end (last timestamps)
                  :lab/files-touched []
                  :lab/user-messages user-msgs
                  :lab/assistant-messages assistant-msgs
                  :lab/all-events messages
                  :lab/event-count (count messages)
                  :lab/has-psr false
                  :lab/has-pur false
                  :lab/has-aif false}
        out-file (io/file lab-raw-dir (str session-id ".json"))]
    (spit out-file (json/generate-string lab-json {:pretty true}))
    (println (format "Imported %d messages (%d user, %d assistant) -> %s"
                     (count messages)
                     (count user-msgs)
                     (count assistant-msgs)
                     (.getPath out-file)))))

;; CLI
(let [args *command-line-args*]
  (if (empty? args)
    (println "Usage: claude-jsonl-to-lab.clj <path-to-jsonl> [session-name]")
    (let [jsonl-path (first args)
          session-name (second args)]
      (if (not (.exists (io/file jsonl-path)))
        (println "File not found:" jsonl-path)
        (import-jsonl! jsonl-path session-name)))))
