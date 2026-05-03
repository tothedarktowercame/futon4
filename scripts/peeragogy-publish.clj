#!/usr/bin/env bb
;; peeragogy-publish.clj
;;
;; Publish locally-edited Peeragogy Handbook chapters back to Wikibooks
;; via the MediaWiki API, using the futon-peeragogy bot.
;;
;; Login: Arided@futon-peeragogy with bot password (read from env or arg).
;; Edit pipeline (per chapter):
;;   1. Read local Markdown.
;;   2. Pandoc-convert gfm → mediawiki, strip Pandoc's <span id="..."> anchors,
;;      decode &quot;/&amp; entities back to plain.
;;   3. GET current page wikitext + revision-id.
;;   4. Show a unified-diff preview to stderr.
;;   5. Either dry-run-only (no edit) or POST action=edit with the rev-id
;;      check (basetimestamp + starttimestamp) and a sourced edit summary.
;;
;; Usage:
;;   bb scripts/peeragogy-publish.clj fetch <page-title>
;;   bb scripts/peeragogy-publish.clj diff <page-title> <local-md>
;;   bb scripts/peeragogy-publish.clj push <page-title> <local-md> <summary>
;;   bb scripts/peeragogy-publish.clj surgical-edit <page-title> <find> <replace> <summary> [--dry]
;;     Literal string find→replace on live wikitext, preserves all surrounding
;;     markup (File embeds, interwiki links, templates). Refuses if <find> is
;;     not present or appears more than once. --dry prints diff without pushing.
;;     (uses BOT_PASSWORD env var; refuses if absent unless --dry)
;;
;; Mission: M-arxana-roundtrip / M-peeragogy-rewrite (publish phase).

(require '[clojure.string :as str]
         '[clojure.java.shell :refer [sh]]
         '[babashka.http-client :as http]
         '[cheshire.core :as json])

(def WIKI-API "https://en.wikibooks.org/w/api.php")
(def BOT-USER "Arided@futon-peeragogy")

(def ^:dynamic *cookies* (atom {}))

(defn- merge-cookies! [resp]
  (when-let [set-cookie (get-in resp [:headers "set-cookie"])]
    (let [pieces (cond-> set-cookie (string? set-cookie) vector)]
      (doseq [piece pieces]
        (let [kv (first (str/split piece #";"))
              [k v] (str/split kv #"=" 2)]
          (when (and k v) (swap! *cookies* assoc k v)))))))

(defn- cookie-header []
  (->> @*cookies*
       (map (fn [[k v]] (str k "=" v)))
       (str/join "; ")))

(defn- api-get [params]
  (let [url (str WIKI-API "?"
                 (str/join "&" (for [[k v] (assoc params :format "json")]
                                 (str (name k) "=" (java.net.URLEncoder/encode (str v) "UTF-8")))))
        resp (http/get url {:headers {"User-Agent" "futon-peeragogy/0.1 (Joe Corneli operator)"
                                      "Cookie" (cookie-header)}
                            :throw false})]
    (merge-cookies! resp)
    (json/parse-string (:body resp) true)))

(defn- api-post [params]
  (let [resp (http/post WIKI-API
                        {:headers {"User-Agent" "futon-peeragogy/0.1 (Joe Corneli operator)"
                                   "Content-Type" "application/x-www-form-urlencoded"
                                   "Cookie" (cookie-header)}
                         :form-params (assoc params :format "json")
                         :throw false})]
    (merge-cookies! resp)
    (json/parse-string (:body resp) true)))

(defn login!
  "Two-step MediaWiki bot login. Returns true on success, throws otherwise."
  [bot-password]
  (let [token-resp (api-get {:action "query" :meta "tokens" :type "login"})
        login-token (get-in token-resp [:query :tokens :logintoken])
        _ (when-not login-token
            (throw (ex-info "Could not fetch login token" {:resp token-resp})))
        login-resp (api-post {:action "login"
                              :lgname BOT-USER
                              :lgpassword bot-password
                              :lgtoken login-token})]
    (when (not= "Success" (get-in login-resp [:login :result]))
      (throw (ex-info "Login failed" {:resp login-resp})))
    (binding [*out* *err*]
      (println (str "[publish] logged in as " (get-in login-resp [:login :lgusername]))))
    true))

(defn fetch-page
  "Return {:wikitext :starttimestamp :basetimestamp} for PAGE-TITLE,
   or nil if missing."
  [page-title]
  (let [resp (api-get {:action "query"
                       :prop "revisions"
                       :titles page-title
                       :rvprop "content|timestamp"
                       :rvslots "main"
                       :curtimestamp 1})
        pages (-> resp :query :pages)
        page (first (vals pages))
        rev (first (:revisions page))]
    (when rev
      {:wikitext (get-in rev [:slots :main :*])
       :basetimestamp (:timestamp rev)
       :starttimestamp (:curtimestamp resp)})))

(defn fetch-csrf-token []
  (-> (api-get {:action "query" :meta "tokens"})
      (get-in [:query :tokens :csrftoken])))

(defn md->wikitext
  "Convert Markdown LOCAL-MD-PATH to MediaWiki wikitext via Pandoc, then
   apply lightweight cleanup: strip Pandoc's <span id=\"...\"></span>
   anchor tags (cosmetic noise) and decode &quot;/&amp; entities back to
   plain so the published wikitext doesn't carry HTML escaping for ASCII."
  [local-md-path]
  (let [{:keys [exit out err]} (sh "pandoc" "-f" "gfm" "-t" "mediawiki" local-md-path)]
    (when (not= 0 exit)
      (throw (ex-info "Pandoc conversion failed" {:err err})))
    (-> out
        (str/replace #"<span id=\"[^\"]*\"></span>\n?" "")
        (str/replace #"&quot;" "\"")
        (str/replace #"&amp;" "&"))))

(defn unified-diff
  "Quick diff via system diff(1). Writes to stderr."
  [old-text new-text]
  (let [old-tmp (java.io.File/createTempFile "wb-old" ".txt")
        new-tmp (java.io.File/createTempFile "wb-new" ".txt")]
    (try
      (spit old-tmp old-text)
      (spit new-tmp new-text)
      (let [{:keys [out]} (sh "diff" "-u" (.getPath old-tmp) (.getPath new-tmp))]
        (binding [*out* *err*]
          (println "----- diff (current → proposed) -----")
          (print out)
          (println "----- end diff -----")))
      (finally
        (.delete old-tmp)
        (.delete new-tmp)))))

(defn edit-page
  "POST action=edit. Requires login first. Returns the API response map."
  [page-title new-wikitext summary {:keys [basetimestamp starttimestamp]}]
  (let [csrf (fetch-csrf-token)]
    (when-not csrf
      (throw (ex-info "Could not fetch CSRF token (login state?)" {})))
    (api-post (cond-> {:action "edit"
                       :title page-title
                       :text new-wikitext
                       :summary summary
                       :bot 1
                       :token csrf
                       :nocreate 1}
                basetimestamp (assoc :basetimestamp basetimestamp)
                starttimestamp (assoc :starttimestamp starttimestamp)))))

(defn -main [& args]
  (case (first args)
    "fetch"
    (let [page-title (second args)
          page (fetch-page page-title)]
      (if page
        (println (:wikitext page))
        (do (binding [*out* *err*] (println "page not found")) (System/exit 1))))

    "diff"
    (let [page-title (second args)
          local-md (nth args 2)
          page (fetch-page page-title)
          new-wt (md->wikitext local-md)]
      (if page
        (do (unified-diff (:wikitext page) new-wt)
            (println (str "[publish] page=" page-title))
            (println (str "[publish] basetimestamp=" (:basetimestamp page)))
            (println (str "[publish] new bytes=" (count new-wt))))
        (do (binding [*out* *err*] (println "page not found")) (System/exit 1))))

    "surgical-edit"
    (let [page-title (second args)
          find-str (nth args 2)
          replace-str (nth args 3)
          summary (nth args 4)
          dry? (some #{"--dry"} args)
          bot-password (System/getenv "BOT_PASSWORD")]
      (when-not dry?
        (when (str/blank? bot-password)
          (binding [*out* *err*]
            (println "ERROR: BOT_PASSWORD env var must be set (or pass --dry)"))
          (System/exit 2)))
      (when-not dry? (login! bot-password))
      (let [page (fetch-page page-title)]
        (when-not page
          (binding [*out* *err*] (println "page not found"))
          (System/exit 1))
        (let [old-wt (:wikitext page)
              hits (count (re-seq (re-pattern (java.util.regex.Pattern/quote find-str)) old-wt))]
          (cond
            (zero? hits)
            (do (binding [*out* *err*]
                  (println (str "[publish] FIND string not present in live wikitext for "
                                page-title))
                  (println (str "[publish] find=" (pr-str find-str))))
                (System/exit 3))
            (> hits 1)
            (do (binding [*out* *err*]
                  (println (str "[publish] FIND string occurs " hits " times — refusing.")))
                (System/exit 4))
            :else
            (let [new-wt (str/replace old-wt find-str replace-str)]
              (unified-diff old-wt new-wt)
              (binding [*out* *err*]
                (println (str "[publish] page=" page-title))
                (println (str "[publish] basetimestamp=" (:basetimestamp page)))
                (println (str "[publish] old bytes=" (count old-wt) " new bytes=" (count new-wt))))
              (if dry?
                (binding [*out* *err*] (println "[publish] DRY RUN — not pushing"))
                (let [resp (edit-page page-title new-wt summary
                                      {:basetimestamp (:basetimestamp page)
                                       :starttimestamp (:starttimestamp page)})
                      result (get-in resp [:edit :result])]
                  (binding [*out* *err*]
                    (println (str "[publish] edit response: " (pr-str resp))))
                  (if (= "Success" result)
                    (println (str "[publish] OK: " page-title " → revid "
                                  (get-in resp [:edit :newrevid])))
                    (do (binding [*out* *err*]
                          (println (str "[publish] FAILED: " page-title)))
                        (System/exit 1))))))))))

    "push"
    (let [page-title (second args)
          local-md (nth args 2)
          summary (or (nth args 3 nil)
                      "Update via futon-peeragogy bot (M-peeragogy-rewrite)")
          bot-password (System/getenv "BOT_PASSWORD")]
      (when (str/blank? bot-password)
        (binding [*out* *err*]
          (println "ERROR: BOT_PASSWORD env var must be set"))
        (System/exit 2))
      (login! bot-password)
      (let [page (fetch-page page-title)
            new-wt (md->wikitext local-md)]
        (when-not page
          (binding [*out* *err*] (println "page not found"))
          (System/exit 1))
        (unified-diff (:wikitext page) new-wt)
        (let [resp (edit-page page-title new-wt summary
                              {:basetimestamp (:basetimestamp page)
                               :starttimestamp (:starttimestamp page)})
              result (get-in resp [:edit :result])]
          (binding [*out* *err*]
            (println (str "[publish] edit response: " (pr-str resp))))
          (if (= "Success" result)
            (println (str "[publish] OK: " page-title " → revid "
                          (get-in resp [:edit :newrevid])))
            (do (binding [*out* *err*]
                  (println (str "[publish] FAILED: " page-title)))
                (System/exit 1))))))

    (do (binding [*out* *err*]
          (println "usage: bb peeragogy-publish.clj {fetch|diff|push|surgical-edit} <page-title> [args...]"))
        (System/exit 2))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
