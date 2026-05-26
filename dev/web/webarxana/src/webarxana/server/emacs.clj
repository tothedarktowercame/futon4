(ns webarxana.server.emacs
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [ring.util.response :as resp]))

(defn- open-form
  "Return the elisp form used to open LOCATION in Emacs."
  [location]
  (if (or (str/starts-with? location "arxana://")
          (str/starts-with? location "docbook://"))
    (str "(progn "
         "(let ((load-prefer-newer t)) "
         "(load \"arxana-docbook-ui\" nil t)) "
         "(arxana-docbook-open-uri "
         (pr-str location)
         "))")
    (str "(find-file " (pr-str location) ")")))

(defn open-location
  "Open an Arxana location in the configured Emacs server."
  [req cfg]
  (let [location (some-> (get-in req [:body :location]) str/trim)
        socket   (or (some-> (get-in req [:body :socket]) str/trim not-empty)
                     (:emacs-socket cfg)
                     "server")
        client   (or (:emacsclient-bin cfg) "emacsclient")]
    (cond
      (not (seq location))
      (-> (resp/response {:ok false :error "Missing location"})
          (resp/status 400))

      (not (or (str/starts-with? location "arxana://")
               (str/starts-with? location "docbook://")
               (str/starts-with? location "/")
               (str/starts-with? location "~/")))
      (-> (resp/response {:ok false
                          :error "Unsupported location"
                          :location location})
          (resp/status 400))

      :else
      (let [{:keys [exit out err]}
            (shell/sh client "-s" socket "-n" "-e" (open-form location))]
        (if (zero? exit)
          (resp/response {:ok true
                          :location location
                          :socket socket
                          :stdout (str/trim (or out ""))})
          (-> (resp/response {:ok false
                              :location location
                              :socket socket
                              :error (str/trim (or err "emacsclient failed"))
                              :exit exit})
              (resp/status 502)))))))
