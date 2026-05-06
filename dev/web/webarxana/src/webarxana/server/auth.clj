(ns webarxana.server.auth
  (:require [buddy.hashers :as hashers]
            [ring.util.response :as resp]))

;; Users stored as a simple EDN file alongside the server.
;; Format: {"username" "<bcrypt-hash>"}
;; Generate a hash:  (buddy.hashers/derive "password")
;;
;; Path resolution:
;;   - default: "users.edn" (resolved against the JVM working directory)
;;   - override: set the WEBARXANA_USERS_FILE environment variable to an
;;     absolute or cwd-relative path. Useful when the install root is a
;;     build-output tree and runtime state should land elsewhere.

(def users-file
  (or (System/getenv "WEBARXANA_USERS_FILE")
      "users.edn"))

(defn load-users []
  (try
    (read-string (slurp users-file))
    (catch Exception _
      ;; Bootstrap: if no users file, create a default dev user.
      ;; Password: "arxana"
      (let [default {"joe" (hashers/derive "arxana")}]
        (spit users-file (pr-str default))
        default))))

(defn login [req _cfg]
  (let [{:keys [username password]} (:body req)
        users (load-users)
        hash  (get users username)]
    (if (and hash (hashers/check password hash))
      (-> (resp/response {:ok true :username username})
          (assoc :session {:username username}))
      (-> (resp/response {:ok false :error "Invalid credentials"})
          (resp/status 401)))))

(defn logout []
  (-> (resp/response {:ok true})
      (assoc :session nil)))

(defn check [req]
  (if-let [username (get-in req [:session :username])]
    (resp/response {:authenticated true :username username})
    (-> (resp/response {:authenticated false})
        (resp/status 401))))

(defn wrap-require-auth [handler]
  (fn [req]
    (if (get-in req [:session :username])
      (handler req)
      (-> (resp/response {:error "Not authenticated"})
          (resp/status 401)))))
