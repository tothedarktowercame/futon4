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

;; On-laptop dev convenience: when enabled, skip the session gate entirely (so
;; Playwright / curl can drive /wa without logging in). Default OFF — production
;; and any unset environment are unaffected. Seeded from WEBARXANA_DEV_NO_AUTH at
;; load time, but held in a defonce atom so it can be toggled at runtime over the
;; nREPL: (webarxana.server.auth/set-dev-no-auth! true).
(defonce ^:private !dev-no-auth
  (atom (boolean (when-let [v (System/getenv "WEBARXANA_DEV_NO_AUTH")]
                   (contains? #{"1" "true" "yes" "on"} (.toLowerCase (.trim v)))))))

(defn set-dev-no-auth!
  "Toggle the on-laptop dev auth bypass at runtime. Returns the new value."
  [on?]
  (reset! !dev-no-auth (boolean on?)))

(defn- dev-no-auth? [] @!dev-no-auth)

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
  (if-let [username (or (get-in req [:session :username])
                        (when (dev-no-auth?) "dev"))]
    (resp/response {:authenticated true :username username})
    (-> (resp/response {:authenticated false})
        (resp/status 401))))

(defn wrap-require-auth [handler]
  (fn [req]
    (if-let [username (or (get-in req [:session :username])
                          (when (dev-no-auth?) "dev"))]
      ;; Synthesize the session username under bypass so downstream handlers
      ;; that read [:session :username] still work.
      (handler (assoc-in req [:session :username] username))
      (-> (resp/response {:error "Not authenticated"})
          (resp/status 401)))))
