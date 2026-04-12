(ns webarxana.client.card
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [cljs.core.async :refer [go <!]]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]))

;; --- Creation dialog (shared by top-level + and + Adjacent) ---

(defn creation-dialog
  "Modal dialog for creating a new entity.
   opts: {:adjacent? bool, :from-id string, :on-close fn}"
  [opts]
  (let [new-name     (r/atom "")
        new-type     (r/atom "article")
        rel-type     (r/atom "arxana/scholium")
        entity-types (:available-types @state/ui-state)]
    (fn [{:keys [adjacent? from-id on-close]}]
      (let [entity-types  (:available-types @state/ui-state)
            rel-types     (:available-relation-types @state/ui-state)]
        [:div.creation-overlay
         {:on-click (fn [e]
                      (when (= (.-target e) (.-currentTarget e))
                        (on-close)))}
         [:div.creation-dialog
          [:div.creation-header
           (if adjacent? "New adjacent node" "New node")]
          [:div.creation-field
           [:label "Name"]
           [:input {:type "text"
                    :placeholder "Enter name..."
                    :value @new-name
                    :auto-focus true
                    :on-change #(reset! new-name (.. % -target -value))
                    :on-key-down (fn [e]
                                   (when (= 13 (.-keyCode e))
                                     (let [dlg (.closest (.-target e) ".creation-dialog")]
                                       (when-let [btn (.querySelector dlg ".btn-create")]
                                         (.click btn)))))}]]
          [:div.creation-field
           [:label "Type"]
           [:select {:value @new-type
                     :on-change #(reset! new-type (.. % -target -value))}
            (for [t (or (seq entity-types) ["article"])]
              ^{:key t} [:option {:value t} t])]]
          (when adjacent?
            [:div.creation-field
             [:label "Relation"]
             [:select {:value @rel-type
                       :on-change #(reset! rel-type (.. % -target -value))}
              (for [t (or (seq rel-types)
                          ["arxana/scholium" "defines" "inspired-by"
                           "supported-by" "answered-by" "example"])]
                ^{:key t} [:option {:value t} t])]])
          [:div.creation-actions
           [:button.btn-create
            {:on-click
             (fn []
               (let [name-val (str/trim @new-name)]
                 (when (seq name-val)
                   (go
                     (let [entity (<! (api/save-entity!
                                       {:name name-val
                                        :type @new-type
                                        :props {:authors [(:username @state/ui-state)]}}))]
                       (when-let [eid (or (:id entity) (:entity/id entity))]
                         (when adjacent?
                           (<! (api/save-relation!
                                {:type @rel-type
                                 :src  from-id
                                 :dst  eid})))
                         (state/set-focus! eid)
                         (on-close)))))))}
            "Create"]
           [:button.btn-cancel {:on-click on-close} "Cancel"]]]]))))


;; --- Focus card ---

(defn focus-card
  "Editable card overlay for the focused nema."
  []
  (let [editing-text (r/atom nil)
        editing-name (r/atom nil)
        show-adjacent (r/atom false)]
    (fn []
      (let [focus-id (state/focus-id)
            nema     (when focus-id (state/get-nema focus-id))]
        [:<>
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
                                           :source new-text})
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
                   {:on-click #(reset! show-adjacent true)}
                   "+ Adjacent"]])]]))
         ;; Adjacent creation dialog
         (when @show-adjacent
           [creation-dialog {:adjacent? true
                             :from-id (state/focus-id)
                             :on-close #(reset! show-adjacent false)}])]))))

;; --- Login ---

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

;; --- Search bar with + button and hop controls ---

(defn search-bar
  "Top bar with search, entity type browser, new node, and hop controls."
  []
  (let [query          (r/atom "")
        show-new-node  (r/atom false)]
    (fn []
      (let [types     (:available-types @state/ui-state)
            sidebar?  (:sidebar-open @state/ui-state)
            hop-depth (:hop-depth @state/ui-state)]
        [:<>
         [:div.search-bar
          ;; Sidebar toggle
          [:button.sidebar-toggle
           {:on-click #(do (when (empty? types)
                            (api/fetch-types))
                          (swap! state/ui-state update :sidebar-open not))}
           (if sidebar? "\u25c0" "\u2630")]
          ;; New node
          [:button.btn-new-node
           {:on-click #(do (when (empty? types)
                            (api/fetch-types))
                          (reset! show-new-node true))}
           "+"]
          ;; Name search
          [:input {:type "text"
                   :placeholder "Search by name..."
                   :value @query
                   :on-change #(reset! query (.. % -target -value))
                   :on-key-down #(when (= 13 (.-keyCode %))
                                   (api/fetch-ego @query))}]
          [:button {:on-click #(api/fetch-ego @query)} "Go"]
          ;; Hop depth controls
          [:div.hop-controls
           [:button.hop-btn
            {:on-click #(swap! state/ui-state update :hop-depth (fn [k] (max 1 (dec k))))}
            "\u2212"]
           [:span.hop-label (str "k=" hop-depth)]
           [:button.hop-btn
            {:on-click #(swap! state/ui-state update :hop-depth (fn [k] (min 10 (inc k))))}
            "+"]]
          [:span.status-indicator
           {:class (if (:connected @state/ui-state) "connected" "disconnected")}
           (if (:connected @state/ui-state) "live" "offline")]
          (when-let [user (:username @state/ui-state)]
            [:span.username user])]
         ;; New node dialog
         (when @show-new-node
           [creation-dialog {:adjacent? false
                             :on-close #(reset! show-new-node false)}])]))))

;; --- Sidebar ---

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
