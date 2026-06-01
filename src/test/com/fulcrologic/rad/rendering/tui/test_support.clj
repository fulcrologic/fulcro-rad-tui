(ns com.fulcrologic.rad.rendering.tui.test-support
  "A self-contained, DB-free RAD model + headless fulcro-tui app for exercising the TUI RAD plugin.

   A real client app (statechart routing + a RAD report/list/form using the plugin) is wired to a
   **mock** loopback remote that returns canned rows for the report's source attribute — no Pathom, no
   Datomic. Picker options (ref fields, picker controls) are seeded directly into the picker-options
   cache. The app drives a `string-terminal`, so specs assert on the painted screen / focus / state
   exactly as a user would see it. JVM-only (not in the bb spec set)."
  (:refer-clojure :exclude [name])
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.raw.application :as rapp]
    [com.fulcrologic.fulcro.headless.loopback-remotes :as lb]
    [com.fulcrologic.fulcro.tui.elements :as e]
    [com.fulcrologic.fulcro.tui.application :as tui-app]
    [com.fulcrologic.fulcro.tui.engine :as engine]
    [com.fulcrologic.fulcro.tui.terminal :as term]
    [com.fulcrologic.rad.application :as rad-app]
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]
    [com.fulcrologic.rad.form-options :as fo]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.rendering.tui.tui-controls :as tui-plugin]
    [com.fulcrologic.rad.report-options :as ro]
    [com.fulcrologic.rad.statechart.form :as form :refer [defsc-form]]
    [com.fulcrologic.rad.statechart.report :as report :refer [defsc-report]]
    [com.fulcrologic.statecharts.chart :refer [statechart]]
    [com.fulcrologic.statecharts.integration.fulcro :as scf]
    [com.fulcrologic.statecharts.integration.fulcro.routing :as scr]
    [edn-query-language.core :as eql]
    [taoensso.timbre :as log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model — one entity whose attributes cover every field type/style the plugin renders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defattr id :thing/id :uuid {ao/identity? true})
(defattr name :thing/name :string {ao/identities #{:thing/id} ao/required? true})
(defattr bio :thing/bio :string {ao/identities #{:thing/id}})
(defattr pw :thing/pw :string {ao/identities #{:thing/id}})
(defattr token :thing/token :string {ao/identities #{:thing/id}})
(defattr size :thing/size :string {ao/identities #{:thing/id}})
(defattr qty :thing/qty :int {ao/identities #{:thing/id}})
(defattr big :thing/big :long {ao/identities #{:thing/id}})
(defattr rating :thing/rating :double {ao/identities #{:thing/id}})
(defattr price :thing/price :decimal {ao/identities #{:thing/id}})
(defattr cost :thing/cost :decimal {ao/identities #{:thing/id}})
(defattr active? :thing/active? :boolean {ao/identities #{:thing/id}})

(def priorities {1 "Low" 2 "Medium" 3 "High"})
(defattr priority :thing/priority :int
  {ao/identities #{:thing/id} ao/enumerated-values #{1 2 3} ao/enumerated-labels priorities})

(def colors {:color/red "Red" :color/green "Green" :color/blue "Blue"})
(defattr color :thing/color :keyword
  {ao/identities #{:thing/id} ao/enumerated-values (set (keys colors)) ao/enumerated-labels colors})

(def roles {:role/admin "Administrator" :role/user "Normal User"})
(defattr role :thing/role :enum
  {ao/identities #{:thing/id} ao/enumerated-values (set (keys roles)) ao/enumerated-labels roles})

(def statuses {:status/active "Active" :status/closed "Closed" :status/pending "Pending"})
(defattr status :thing/status :enum
  {ao/identities #{:thing/id} ao/enumerated-values (set (keys statuses)) ao/enumerated-labels statuses})

(defattr created :thing/created :instant {ao/identities #{:thing/id}})

;; A to-one ref (picker) and the cache key its options live under.
(defattr owner :thing/owner :ref
  {ao/identities #{:thing/id} ao/target :person/id ao/cardinality :one
   po/cache-key  ::people})

;; A component to-many subform ref.
(defattr tags :thing/tags :ref
  {ao/identities #{:thing/id} ao/target :tag/id ao/cardinality :many ao/component? true})

(defattr tag-id :tag/id :uuid {ao/identity? true})
(defattr tag-label :tag/label :string {ao/identities #{:tag/id} ao/required? true})

(defattr all-things :thing/all :ref {ao/target :thing/id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forms / reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsc-form TagForm [this props]
  {fo/id         tag-id
   fo/attributes [tag-label]})

(defsc-form ThingForm [this props]
  {fo/id                 id
   fo/title              "Edit Thing"
   fo/cancel-route       :com.fulcrologic.rad.rendering.tui.test-support/ThingReport
   fo/attributes         [name bio pw token size qty big rating price cost active?
                          priority color role status created owner tags]
   fo/default-values     {:thing/active? true :thing/tags [{}]}
   fo/field-styles       {:thing/bio      :multi-line
                          :thing/pw       :password
                          :thing/token    :viewable-password
                          :thing/size     :sorted-set
                          :thing/priority :picker
                          :thing/cost     :USD
                          :thing/status   :autocomplete
                          :thing/owner    :pick-one}
   fo/field-style-configs {:thing/size {:sorted-set/valid-values ["S" "M" "L" "XL"]}}
   fo/subforms           {:thing/tags {fo/ui          TagForm
                                       fo/title       "Tags"
                                       fo/can-add?    (fn [_ _] true)
                                       fo/can-delete? (fn [_ _] true)}}})

(defsc-report ThingReport [this props]
  {ro/title               "Things"
   ro/source-attribute    :thing/all
   ro/row-pk              id
   ro/columns             [name active? cost]
   ro/column-formatters   {:thing/active? (fn [_ v _ _] (if v "Yes" "No"))
                           :thing/cost    (fn [_ v _ _] (when v (str "$" v)))}
   ro/column-headings     {:thing/name "Thing Name"}
   ro/form-links          {name ThingForm}
   ro/initial-sort-params {:sort-by          :thing/name
                           :ascending?       true
                           :sortable-columns #{:thing/name :thing/active?}}
   ro/controls            {::new      {:type :button :local? true :label "New Thing"
                                       :action (fn [this] (form/create! this ThingForm))}
                           ::flt      {:type :string :style :search :local? true :label "Filter"
                                       :onChange (fn [this _] (report/filter-rows! this))}
                           ::only-on? {:type :boolean :local? false :default-value false :label "Active only?"
                                       :onChange (fn [this _] (report/run-report! this))}
                           ::since    {:type :instant :style :starting-date :local? true :label "Since"}
                           ::kind     {:type :picker :local? false :default-value :all :label "Kind"
                                       po/cache-key ::kinds}}
   ro/control-layout      {:action-buttons [::new]
                           :inputs         [[::flt] [::only-on?] [::since] [::kind]]}
   ro/row-visible?        (fn [{::keys [flt]} {:thing/keys [name]}]
                            (let [t (some-> flt str/trim str/lower-case)]
                              (or (str/blank? t) (str/includes? (str/lower-case (str name)) t))))
   ro/row-actions         [{:label "Ping" :action (fn [_ _] :ok)}]
   ro/run-on-mount?       true
   ro/route               "things"})

(defsc-report ThingList [this props]
  {ro/title            "Things (list)"
   ro/source-attribute :thing/all
   ro/row-pk           id
   ro/columns          [name cost active?]
   ro/column-formatters {:thing/active? (fn [_ v _ _] (if v "Yes" "No"))
                         :thing/cost    (fn [_ v _ _] (when v (str "$" v)))}
   ro/layout-style     :list
   ro/row-style        :list
   ro/run-on-mount?    true
   ro/route            "thing-list"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root + routing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comp/defsc Routes [this _props]
  {:query                   [:ui/current-route]
   :ident                   (fn [] [:component/id ::Routes])
   :preserve-dynamic-query? true
   :initial-state           {}}
  (scr/ui-current-subroute this comp/factory))

(def ui-routes (comp/factory Routes))

(comp/defsc Root [this {:ui/keys [routes]}]
  {:query         [{:ui/routes (comp/get-query Routes)}
                   [:com.fulcrologic.statecharts/session-id '_]]
   :initial-state {:ui/routes {}}}
  (if (seq (scf/current-configuration this scr/session-id))
    (ui-routes routes)
    (e/text "Starting…")))

(def routing-chart
  (statechart {:initial :state/route-root}
    (scr/routing-regions
      (scr/routes {:id :state/root :routing/root Routes}
        (report/report-route-state {:route/target ThingReport})
        (report/report-route-state {:route/target ThingList})
        (form/form-route-state {:route/target ThingForm :route/params #{:id}})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canned data + mock remote
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- uuid* [n] (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(def things
  "Canned rows for the `:thing/all` source attribute."
  [{:thing/id (uuid* 1) :thing/name "Anvil"   :thing/active? true  :thing/cost "9.99"}
   {:thing/id (uuid* 2) :thing/name "Bellows" :thing/active? false :thing/cost "19.50"}
   {:thing/id (uuid* 3) :thing/name "Crucible":thing/active? true  :thing/cost "4.00"}])

(defn mock-remote
  "A loopback remote that answers top-level query joins from `responses` (a map of join-key -> data),
   ignoring anything it does not recognize. The join-key may be a plain keyword (e.g. the report's
   `:thing/all` source attribute) or an ident vector (e.g. `[:thing/id #uuid …]` for a form edit load).
   Used so loads resolve without a Pathom parser / database."
  [responses]
  (lb/sync-remote
    (fn [eql]
      (let [ast (eql/query->ast eql)]
        (reduce (fn [acc {:keys [key dispatch-key]}]
                  (let [k (if (contains? responses key) key dispatch-key)]
                    (if (contains? responses k)
                      (assoc acc k (get responses k))
                      acc)))
          {} (:children ast))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App construction + screen helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn seed-picker-options!
  "Seeds the picker-options cache so ref fields and picker controls have options without a load. `cache->options`
   is a map of cache-key -> vector of `{:text :value}` option maps."
  [app cache->options]
  (swap! (:com.fulcrologic.fulcro.application/state-atom app)
    (fn [s] (reduce-kv (fn [s k opts] (assoc-in s [::po/options-cache k :options] opts)) s cache->options))))

(defn new-app!
  "Builds a headless TUI app using the plugin, a mock remote returning the canned things, statechart
   routing, seeded picker options, and a `string-terminal` of `rows`x`cols`."
  [& {:keys [rows cols] :or {rows 70 cols 120}}]
  (let [people-opts [{:text "Ada"  :value [:person/id (uuid* 11)]}
                     {:text "Babbage" :value [:person/id (uuid* 12)]}]
        kind-opts   [{:text "All" :value :all} {:text "Hardware" :value :hw} {:text "Tools" :value :tools}]
        by-ident (into {} (map (fn [t] [[:thing/id (:thing/id t)] t])) things)
        app (tui-app/application
              {:root-class Root
               :remotes    {:remote (mock-remote (merge {:thing/all things} by-ident))}})]
    (rad-app/install-ui-controls! app tui-plugin/all-controls)
    (scf/install-fulcro-statecharts! app {:event-loop? true})
    (scr/start! app routing-chart)
    (seed-picker-options! app {::people people-opts ::kinds kind-opts})
    (tui-app/attach! app (term/string-terminal {:rows rows :cols cols}))
    app))

(defn screen-text
  "Returns the painted screen of `app` as a single newline-joined string (trailing blanks trimmed)."
  [app]
  (tui-app/render! app)
  (str/join "\n" (mapv str/trimr (tui-app/screen-of app))))

(defn wait-for
  "Renders `app` and returns the screen text once it matches `re`, polling up to ~20s; returns the last
   screen on timeout so the assertion fails against the real screen."
  [app re]
  (loop [n 0]
    (let [screen (screen-text app)]
      (if (or (re-find re screen) (>= n 80))
        screen
        (do (Thread/sleep 250) (recur (inc n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interaction helpers (keyboard-driving + state poking)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tab! [app] (tui-app/step! app {:key :tab}))
(defn enter! [app] (tui-app/step! app {:key :enter}))

(defn focus-ns
  "The namespace (string) of the currently-focused node id, or nil."
  [app]
  (some-> (engine/current-focus app) namespace))

(defn tab-to-ns!
  "Presses Tab until the focused node id's namespace is `ns-str` (or ~60 stops elapse). Returns the
   resulting focus namespace. Renders between presses."
  [app ns-str]
  (loop [i 0]
    (when (and (not= ns-str (focus-ns app)) (< i 60))
      (tab! app)
      (recur (inc i))))
  (focus-ns app))

(defn tab-to-id!
  "Presses Tab until the focused node id equals `id` (or ~80 stops elapse). Returns the resulting focus
   id. Use when several nodes share a namespace (e.g. a report's action button and toggle control)."
  [app id]
  (loop [i 0]
    (when (and (not= id (engine/current-focus app)) (< i 80))
      (tab! app)
      (recur (inc i))))
  (engine/current-focus app))

(defn settle!
  "Renders `app` a few times so async statechart events (e.g. add-child!, sort) are processed and
   reflected on screen. Returns the final screen text."
  [app]
  (dotimes [_ 4] (screen-text app) (Thread/sleep 30))
  (screen-text app))

(defn form-tempid
  "The tempid of the single in-progress ThingForm entity (the create form's id)."
  [app]
  (->> (get (rapp/current-state app) :thing/id) keys (filter tempid/tempid?) first))

(defn set-field!
  "Pokes `value` directly into the create form's `:thing/<k>` field in app state (to exercise a
   renderer's display logic without driving keystrokes)."
  [app k value]
  (swap! (:com.fulcrologic.fulcro.application/state-atom app)
    assoc-in [:thing/id (form-tempid app) k] value))

(defn report-app!
  "Builds a fresh app, routes to the report named `report-kw`, and waits for its seeded rows. `report-kw`
   defaults to the table `ThingReport`."
  ([] (report-app! ::ThingReport))
  ([report-kw]
   (let [app (new-app!)]
     (scr/route-to! app report-kw {})
     (wait-for app #"Anvil")
     app)))

(defn form-app!
  "Builds a fresh app, routes to the report, and opens a create `ThingForm`. Returns the app once the
   form has rendered."
  []
  (let [app (report-app!)]
    (form/create! app ThingForm)
    (wait-for app #"Edit Thing")
    app))

(defmacro quiet
  "Runs `body` with timbre quieted to errors (loads/routes log a lot at debug)."
  [& body]
  `(log/with-merged-config {:min-level :error} ~@body))
