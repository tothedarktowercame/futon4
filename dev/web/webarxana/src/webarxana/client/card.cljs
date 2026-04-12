(ns webarxana.client.card
  (:require [reagent.core :as r]
            [cljs.core.async :refer [go <!]]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]))

(defn focus-card
  "Editable card overlay for the focused nema."
  []
  (let [editing-text (r/atom nil)
        editing-name (r/atom nil)]
    (fn []
      (let [focus-id (state/focus-id)
            nema     (when focus-id (state/get-nema focus-id))]
        (when nema
          (let [is-editing (= (:editing @state/ui-state) focus-id)
                nema-name  (or (:nema/name nema) "")
                nema-text  (or (:nema/text nema) "")
                nema-type  (or (:nema/type nema) "unknown")]
            [:div.focus-card
             ;; Header
             [:div.card-header
              [:span.card-type nema-type]
              (if is-editing
                [:input.card-name-input
                 {:value (or @editing-name nema-name)
                  :on-change #(reset! editing-name (.. % -target -value))
                  :placeholder "Name..."}]
                [:span.card-name {:on-double-click
                                  #(do (reset! editing-name nema-name)
                                       (swap! state/ui-state assoc :editing focus-id))}
                 nema-name])
              [:span.card-id (subs (str focus-id) 0 8)]]
             ;; Body
             (if is-editing
               [:textarea.card-body-edit
                {:value (or @editing-text nema-text)
                 :on-change #(reset! editing-text (.. % -target -value))
                 :placeholder "Write here..."
                 :auto-focus true}]
               [:div.card-body
                {:on-double-click
                 #(do (reset! editing-text nema-text)
                      (swap! state/ui-state assoc :editing focus-id))}
                (if (seq nema-text) nema-text [:em "Empty — double-click to edit"])])
             ;; Footer
             [:div.card-footer
              (if is-editing
                [:<>
                 [:button.btn-save
                  {:on-click
                   (fn []
                     (let [new-name (or @editing-name nema-name)
                           new-text (or @editing-text nema-text)]
                       (api/save-entity! {:name new-name
                                          :type nema-type
                                          :notes new-text})
                       ;; Broadcast via WS
                       (api/ws-send! {:type "nema-updated"
                                      :entity {:nema/id focus-id
                                               :nema/name new-name
                                               :nema/text new-text
                                               :nema/type nema-type}})
                       (swap! state/ui-state assoc :editing nil)
                       (reset! editing-text nil)
                       (reset! editing-name nil)))}
                  "Save"]
                 [:button.btn-cancel
                  {:on-click #(do (swap! state/ui-state assoc :editing nil)
                                  (reset! editing-text nil)
                                  (reset! editing-name nil))}
                  "Cancel"]]
                [:<>
                 [:button.btn-edit
                  {:on-click #(do (reset! editing-text nema-text)
                                  (reset! editing-name nema-name)
                                  (swap! state/ui-state assoc :editing focus-id))}
                  "Edit"]
                 [:button.btn-new
                  {:on-click
                   (fn []
                     (go
                       (let [new-entity (<! (api/save-entity!
                                             {:name "New nema"
                                              :type nema-type
                                              :notes ""}))]
                         (when-let [new-id (or (:id new-entity)
                                              (:entity/id new-entity))]
                           (<! (api/save-relation!
                                {:type "defines"
                                 :src  focus-id
                                 :dst  new-id}))
                           (state/set-focus! new-id)))))}
                  "+ Adjacent"]])]]))))))

(defn login-form
  "Login form shown when not authenticated."
  []
  (let [username (r/atom "")
        password (r/atom "")]
    (fn []
      [:div.login-form
       [:h2 "WebArxana"]
       [:p "Sign in to access your nema graph."]
       (when-let [err (:login-error @state/ui-state)]
         [:div.login-error err])
       [:input {:type "text"
                :placeholder "Username"
                :value @username
                :on-change #(reset! username (.. % -target -value))
                :on-key-down #(when (= 13 (.-keyCode %))
                                (api/login! @username @password nil nil))}]
       [:input {:type "password"
                :placeholder "Password"
                :value @password
                :on-change #(reset! password (.. % -target -value))
                :on-key-down #(when (= 13 (.-keyCode %))
                                (api/login! @username @password nil nil))}]
       [:button {:on-click #(api/login! @username @password nil nil)}
        "Sign in"]])))

(defn search-bar
  "Top bar with search, entity type browser, and status."
  []
  (let [query (r/atom "")]
    (fn []
      (let [types     (:available-types @state/ui-state)
            sidebar?  (:sidebar-open @state/ui-state)]
        [:div.search-bar
         ;; Sidebar toggle
         [:button.sidebar-toggle
          {:on-click #(do (when (empty? types)
                           (api/fetch-types))
                         (swap! state/ui-state update :sidebar-open not))}
          (if sidebar? "\u25c0" "\u2630")]
         ;; Name search
         [:input {:type "text"
                  :placeholder "Search by name..."
                  :value @query
                  :on-change #(reset! query (.. % -target -value))
                  :on-key-down #(when (= 13 (.-keyCode %))
                                  (api/fetch-ego @query))}]
         [:button {:on-click #(api/fetch-ego @query)} "Go"]
         [:span.status-indicator
          {:class (if (:connected @state/ui-state) "connected" "disconnected")}
          (if (:connected @state/ui-state) "live" "offline")]
         (when-let [user (:username @state/ui-state)]
           [:span.username user])]))))

(defn sidebar
  "Sidebar for browsing entities by type."
  []
  (when (:sidebar-open @state/ui-state)
    (let [types     (:available-types @state/ui-state)
          cur-type  (:browse-type @state/ui-state)
          entities  (:browse-list @state/ui-state)]
      [:div.sidebar
       [:div.sidebar-types
        [:div.sidebar-heading "Types"]
        (for [t types]
          ^{:key t}
          [:div.sidebar-type-item
           {:class    (when (= t cur-type) "active")
            :on-click #(api/browse-type! t)}
           t])]
       (when cur-type
         [:div.sidebar-entities
          [:div.sidebar-heading (str cur-type " (" (count entities) ")")]
          (for [e entities]
            ^{:key (:id e)}
            [:div.sidebar-entity-item
             {:on-click #(api/browse-and-focus! (:name e) (:id e))}
             (or (:name e) (subs (str (:id e)) 0 12))])])])))
