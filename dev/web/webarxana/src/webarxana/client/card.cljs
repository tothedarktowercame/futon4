(ns webarxana.client.card
  (:require [clojure.string :as str]
            [datascript.core :as d]
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
        new-text     (r/atom "")
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
          [:div.creation-field
           [:label "Text"]
           [:textarea
            {:value @new-text
             :placeholder "Write here..."
             :on-change #(reset! new-text (.. % -target -value))
             :style {:width "100%" :min-height "60px" :padding "8px"
                     :background "var(--surface)" :border "1px solid var(--border)"
                     :border-radius "4px" :color "var(--text)"
                     :font-size "13px" :resize "vertical" :outline "none"
                     :font-family "inherit"}}]]
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
                                        :source @new-text
                                        :props {:authors [(:username @state/ui-state)]}}))]
                       (when-let [eid (or (:id entity) (:entity/id entity))]
                         (when adjacent?
                           (<! (api/save-relation!
                                {:type @rel-type
                                 :src  from-id
                                 :dst  eid})))
                         ;; Fetch ego for the parent so the graph shows the connection
                         (when adjacent?
                           (<! (api/fetch-ego (str from-id))))
                         (state/pin! eid)
                         (on-close)))))))}
            "Create"]
           [:button.btn-cancel {:on-click on-close} "Cancel"]]]]))))


;; --- Focus card ---

(defn- pin-card-inner
  "A single editable card for a pinned entity."
  [pin-id]
  (let [editing-text (r/atom nil)
        editing-name (r/atom nil)
        show-adjacent (r/atom false)]
    (fn [pin-id]
      (let [nema     (state/get-nema pin-id)
            focus-id (state/focus-id)
            is-active (= pin-id focus-id)]
        (when nema
          (let [is-editing  (= (:editing @state/ui-state) pin-id)
                nema-name   (or (:nema/name nema) "")
                nema-text   (or (:nema/text nema) "")
                nema-type   (or (:nema/type nema) "unknown")
                nema-authors (or (:nema/authors nema) [])]
            [:<>
             [:div.focus-card {:class (when is-active "active-pin")}
              ;; Header
              [:div.card-header
               [:span.card-type nema-type]
               (if is-editing
                 [:input.card-name-input
                  {:value (or @editing-name nema-name)
                   :on-change #(reset! editing-name (.. % -target -value))
                   :placeholder "Name..."}]
                 [:span.card-name
                  {:on-click #(state/set-focus! pin-id)
                   :on-double-click
                   #(do (reset! editing-name nema-name)
                        (swap! state/ui-state assoc :editing pin-id))}
                  nema-name])
               [:span.card-id (subs (str pin-id) 0 8)]
               (when (seq nema-authors)
                 [:span.card-authors (str/join ", " nema-authors)])
               [:button.scratch-dismiss
                {:on-click #(state/unpin! pin-id)
                 :title "Unpin"}
                "\u00d7"]]
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
                       (swap! state/ui-state assoc :editing pin-id))}
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
                                   (swap! state/ui-state assoc :editing pin-id))}
                   "Edit"]
                  [:button.scratchpad-btn
                   {:on-click #(swap! state/ui-state assoc
                                      :connecting {:node-id pin-id})
                    :class (when (= pin-id (get-in @state/ui-state [:connecting :node-id]))
                             "connecting-active")}
                   "Link"]
                  [:button.btn-new
                   {:on-click #(do (state/set-focus! pin-id)
                                   (reset! show-adjacent true))}
                   "+ Adjacent"]])]]
             ;; Adjacent dialog for this pin
             (when (and @show-adjacent is-active)
               [creation-dialog {:adjacent? true
                                 :from-id pin-id
                                 :on-close #(reset! show-adjacent false)}])]))))))

(defn focus-card
  "Stacked cards for all pinned entities, scrollable on the right."
  []
  (let [pins (:pins @state/ui-state)]
    (when (seq pins)
      [:div.pin-cards
       (doall
        (for [pin (reverse pins)]
          ^{:key (:id pin)}
          [pin-card-inner (:id pin)]))])))

;; --- Scratch card (floats left, for newly created nodes) ---

(defn scratch-card
  "Editable card for the most recent scratchpad node, floats top-left."
  []
  (let [scratch-name (r/atom "")
        scratch-text (r/atom "")
        scratch-type (r/atom "article")
        editing-type (r/atom false)
        save-status  (r/atom nil)]
    (fn []
      (let [scratchpad (:scratchpad @state/ui-state)
            node       (last scratchpad)
            entity-types (:available-types @state/ui-state)]
        (when node
          (let [node-id (:id node)]
            [:div.scratch-card
             ;; Header
             [:div.card-header
              (if @editing-type
                [:input.card-type-input
                 {:value @scratch-type
                  :auto-focus true
                  :list "type-suggestions"
                  :on-change #(reset! scratch-type (.. % -target -value))
                  :on-blur #(reset! editing-type false)
                  :on-key-down (fn [e]
                                 (when (= 13 (.-keyCode e))
                                   (reset! editing-type false)
                                   ;; Update the scratchpad entry
                                   (swap! state/ui-state update :scratchpad
                                          (fn [sp] (mapv #(if (= (:id %) node-id)
                                                            (assoc % :type @scratch-type)
                                                            %) sp)))))}]
                [:span.card-type
                 {:on-click #(reset! editing-type true)
                  :style {:cursor "pointer"}}
                 @scratch-type])
              ;; Datalist for type suggestions
              [:datalist {:id "type-suggestions"}
               (for [t (or (seq entity-types) ["article"])]
                 ^{:key t} [:option {:value t}])]
              [:input.card-name-input
               {:value @scratch-name
                :on-change #(reset! scratch-name (.. % -target -value))
                :placeholder "Name..."}]
              [:button.scratch-dismiss
               {:on-click #(swap! state/ui-state update :scratchpad
                                  (fn [sp] (vec (remove (fn [n] (= (:id n) node-id)) sp))))}
               "\u00d7"]]
             ;; Body — always editable
             [:textarea.card-body-edit
              {:value @scratch-text
               :on-change (fn [e]
                            (let [new-val (.. e -target -value)
                                  old-val @scratch-text]
                              (reset! scratch-text new-val)
                              ;; Auto-populate name from first line on first newline
                              (when (and (empty? @scratch-name)
                                         (not (str/includes? old-val "\n"))
                                         (str/includes? new-val "\n"))
                                (let [first-line (str/trim (first (str/split new-val #"\n")))]
                                  (when (seq first-line)
                                    (reset! scratch-name first-line))))))
               :placeholder "Write here..."
               :auto-focus true}]
             ;; Footer
             [:div.card-footer
              [:button.btn-save
               {:on-click
                (fn []
                  (let [name-val (str/trim @scratch-name)
                        text-val @scratch-text
                        final-name (if (seq name-val) name-val "Untitled")]
                    (go
                      (let [entity (<! (api/save-entity!
                                        {:name   final-name
                                         :type   @scratch-type
                                         :source text-val
                                         :props  {:authors [(:username @state/ui-state)]}}))]
                        (when-let [server-id (or (:id entity) (:entity/id entity))]
                          ;; Replace local scratchpad entry with server ID
                          (swap! state/ui-state update :scratchpad
                                 (fn [sp] (mapv #(if (= (:id %) node-id)
                                                   (assoc % :id server-id
                                                            :name final-name
                                                            :local? false)
                                                   %) sp)))
                          ;; Ingest into Datascript with content
                          (state/ingest-entity! {:id server-id
                                                 :name final-name
                                                 :type @scratch-type
                                                 :source text-val})
                          ;; Also pin so a card appears on the right with the content
                          (state/pin! server-id)
                          ;; Remove from scratchpad since it's now a real pinned entity
                          (swap! state/ui-state update :scratchpad
                                 (fn [sp] (vec (remove (fn [n] (= (:id n) server-id)) sp))))
                          ;; Flash confirmation
                          (reset! save-status "Saved")
                          (js/setTimeout #(reset! save-status nil) 2000))))))}
               (or @save-status "Save")]
              [:button.btn-edit
               {:on-click
                (fn []
                  (go
                    ;; Save first if not yet persisted
                    (let [final-name (let [n (str/trim @scratch-name)]
                                      (if (seq n) n "Untitled"))
                          eid (if (:local? node)
                                (let [entity (<! (api/save-entity!
                                                  {:name   final-name
                                                   :type   @scratch-type
                                                   :source @scratch-text
                                                   :props  {:authors [(:username @state/ui-state)]}}))]
                                  (or (:id entity) (:entity/id entity)))
                                node-id)]
                      (when eid
                        (state/ingest-entity! {:id eid
                                               :name final-name
                                               :type @scratch-type
                                               :source @scratch-text})
                        (state/pin! eid)
                        (swap! state/ui-state update :scratchpad
                               (fn [sp] (vec (remove (fn [n] (= (:id n) node-id)) sp))))))))}
               "Focus"]
              [:button.scratchpad-btn
               {:on-click #(swap! state/ui-state assoc
                                  :connecting {:node-id node-id})}
               "Connect"]]]))))))

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
  (let [query (r/atom "")
        saving-diagram (r/atom false)
        diagram-name (r/atom "")
        diagram-saved (r/atom false)]
    (fn []
      (let [types     (:available-types @state/ui-state)
            sidebar?  (:sidebar-open @state/ui-state)
            hop-depth (:hop-depth @state/ui-state)]
        [:div.search-bar
         ;; Sidebar toggle
         [:button.sidebar-toggle
          {:on-click #(do (when (empty? types)
                           (api/fetch-types))
                         (swap! state/ui-state update :sidebar-open not))}
          (if sidebar? "\u25c0" "\u2630")]
         ;; Instant new node — creates immediately, adds to scratchpad
         [:button.btn-new-node
          {:on-click #(api/create-scratch-node!)}
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
         ;; Save diagram button (only when pins exist)
         (when (seq (:pins @state/ui-state))
           (if @saving-diagram
             [:div.diagram-save-inline
              [:input {:type "text"
                       :placeholder "Diagram name..."
                       :value @diagram-name
                       :auto-focus true
                       :on-change #(reset! diagram-name (.. % -target -value))
                       :on-key-down #(when (= 13 (.-keyCode %))
                                       (api/save-diagram! @diagram-name)
                                       (reset! saving-diagram false)
                                       (reset! diagram-name ""))}]
              [:button.hop-btn
               {:on-click #(do (api/save-diagram! @diagram-name)
                               (reset! saving-diagram false)
                               (reset! diagram-name "")
                               (reset! diagram-saved true)
                               (js/setTimeout (fn [] (reset! diagram-saved false)) 2000))}
               "\u2713"]
              [:button.hop-btn
               {:on-click #(reset! saving-diagram false)}
               "\u2717"]]
             [:button.hop-btn
              {:on-click #(reset! saving-diagram true)
               :title "Save current spread as a diagram"
               :class (when @diagram-saved "saved")}
              (if @diagram-saved "\u2713" "\ud83d\udcbe")]))
         [:span.status-indicator
          {:class (if (:connected @state/ui-state) "connected" "disconnected")}
          (if (:connected @state/ui-state) "live" "offline")]
         (when-let [user (:username @state/ui-state)]
           [:span.username user])]))))

;; --- Sidebar ---

(defn sidebar
  "Sidebar for browsing entities by type, with scratchpad for new nodes."
  []
  (when (:sidebar-open @state/ui-state)
    (let [types      (:available-types @state/ui-state)
          cur-type   (:browse-type @state/ui-state)
          entities   (:browse-list @state/ui-state)
          connecting (:connecting @state/ui-state)]
      [:div.sidebar
       ;; Connect mode banner
       (when connecting
         [:div.connect-banner
          [:span "Click a node in the graph to connect"]
          [:button.scratchpad-btn
           {:on-click #(swap! state/ui-state assoc :connecting nil)}
           "Cancel"]])
       ;; Recent activity
       [:div.sidebar-recent
        [:div.sidebar-heading
         {:on-click #(api/fetch-recent)
          :style {:cursor "pointer"}}
         "Recent"]
        (when-let [recent (:recent-entities @state/ui-state)]
          (for [e recent]
            ^{:key (:id e)}
            [:div.sidebar-entity-item
             (let [is-diagram (= "diagram" (or (:_type e) (:type e)))
                   is-expanded (= (:id e) (:expanded-diagram @state/ui-state))]
               [:<>
                [:span.entity-name
                 {:on-click (if is-diagram
                              (if is-expanded
                                #(api/compress-diagram! (:id e))
                                #(api/expand-diagram! (:id e)))
                              #(api/browse-and-focus! (:name e) (:id e)))}
                 (str (or (:name e) "?") " ")]
                [:span.entity-type-badge (or (:_type e) (:type e) "?")]
                (when is-diagram
                  [:button.scratchpad-btn
                   {:on-click (if is-expanded
                                #(api/compress-diagram! (:id e))
                                #(api/expand-diagram! (:id e)))
                    :title (if is-expanded "Compress diagram" "Expand diagram")}
                   (if is-expanded "\u23f8" "\u25b6")])])
             [:button.pin-btn
              {:on-click (fn [evt]
                           (.stopPropagation evt)
                           (api/pin-entity! (:name e) (:id e)))
               :title (if (state/pinned? (:id e)) "Unpin" "Pin")}
              (if (state/pinned? (:id e)) "\u25cb" "\u25cf")]]))]
       ;; Types browser
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
             [:span.entity-name
              {:on-click #(api/browse-and-focus! (:name e) (:id e))}
              (or (:name e) (subs (str (:id e)) 0 12))]
             [:button.pin-btn
              {:on-click (fn [evt]
                           (.stopPropagation evt)
                           (api/pin-entity! (:name e) (:id e)))
               :title (if (state/pinned? (:id e)) "Unpin" "Pin")}
              (if (state/pinned? (:id e)) "\u25cb" "\u25cf")]])])])))

;; --- Link editor popup ---

(defn link-editor
  "Popup for editing a link's type and annotation text."
  []
  (let [edit-text (r/atom nil)
        edit-type (r/atom nil)]
    (fn []
      (when-let [info (:editing-link @state/ui-state)]
        (let [rel-types (or (seq (:available-relation-types @state/ui-state))
                            ["arxana/scholium" "defines" "inspired-by"
                             "supported-by" "answered-by" "example"
                             "implemented-by" "surface" "evolved-into"])
              cur-text (if (nil? @edit-text) (:text info) @edit-text)
              cur-type (if (nil? @edit-type) (:type info) @edit-type)]
          [:div.link-editor-popup
           {:style {:left (str (:x info) "px")
                    :top  (str (:y info) "px")}}
           [:div.link-editor-header
            [:span "Edit relation"]
            [:button.scratch-dismiss
             {:on-click #(do (reset! edit-text nil)
                             (reset! edit-type nil)
                             (swap! state/ui-state dissoc :editing-link))}
             "\u00d7"]]
           [:div.link-editor-body
            [:label "Type"]
            [:select
             {:value cur-type
              :on-change #(reset! edit-type (.. % -target -value))}
             (for [t rel-types]
               ^{:key t} [:option {:value t} t])]
            [:label "Annotation"]
            [:textarea.link-editor-text
             {:value cur-text
              :placeholder "Describe this connection..."
              :on-change #(reset! edit-text (.. % -target -value))}]
            [:div.link-editor-actions
             [:button.btn-save
              {:on-click
               (fn []
                 (let [link-id (:id info)
                       new-text (or @edit-text cur-text)
                       new-type (or @edit-type cur-type)]
                   (when link-id
                     (d/transact! state/conn
                       [{:link/id   link-id
                         :link/type new-type
                         :link/text new-text}]))
                   (reset! edit-text nil)
                   (reset! edit-type nil)
                   (swap! state/ui-state dissoc :editing-link)
                   (swap! state/ui-state update :_render-tick (fnil inc 0))))}
              "Save"]
             [:span.card-id (subs (str (:id info)) 0 20)]]]])))))


