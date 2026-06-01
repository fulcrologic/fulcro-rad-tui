(ns com.fulcrologic.rad.rendering.tui.container
  "TUI renderer for a RAD *container* — a component that groups several reports/forms under one shared
   set of controls (see `com.fulcrologic.rad.statechart.container`).

   WIRING NOTE: in fulcro-rad-statecharts 0.1.5 the statechart container's auto-generated body calls
   `com.fulcrologic.rad.statechart.container/render-layout`, which has no plugin hook (it only logs an
   error). So unlike the form/report renderers there is no controls-map bridge that reaches this fn
   automatically. To render a container with this plugin, give the container an explicit body that
   calls `render-container-layout`:

   ```
   (defsc-container Dashboard [this props]
     {co/children {:sales SalesReport :recent RecentReport}
      co/layout   [[:sales] [:recent]]}
     (render-container-layout this))
   ```

   `render-container-layout` is also registered under `:com.fulcrologic.rad.container/style->layout`
   `:default` in `tui-controls` for forward-compatibility, should a future engine version add a bridge."
  (:require
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.tui.elements :as e :refer [vbox hbox text line]]
    [com.fulcrologic.rad.control :as control]
    [com.fulcrologic.rad.options-util :refer [?!]]
    [com.fulcrologic.rad.statechart.container :as container]
    [com.fulcrologic.rad.statechart.container-options :as co]
    [com.fulcrologic.rad.tui-options :as tuo]))

(defn- render-controls
  "Renders the container's pulled-up controls: a row of action buttons followed by the input control
   rows (each via the installed `control/render-control`)."
  [container-instance]
  (let [{:keys [action-layout input-layout]} (control/standard-control-layout container-instance)]
    (vbox {}
      (when (seq action-layout)
        (hbox {:height 1}
          (mapv (fn [k] (control/render-control container-instance k)) action-layout)))
      (mapv (fn [row]
              (vbox {} (mapv (fn [k] (control/render-control container-instance k)) row)))
        input-layout))))

(defn render-container-layout
  "Renders a RAD container as a bordered, titled block: the shared controls, then each child component
   rendered via its own factory. With `co/layout` the children are stacked in the declared row order
   (a terminal is width-limited, so grid rows render top-to-bottom); without it, children render in
   `container/id-child-pairs` order. Each child is given the computed flag
   `:com.fulcrologic.rad.container/controlled? true`."
  [container-instance]
  (let [options   (comp/component-options container-instance)
        children  (get options co/children)
        layout    (get options co/layout)
        title     (?! (get options co/title) container-instance)
        props     (comp/props container-instance)
        render-id (fn [id]
                    (when-let [cls (get children id)]
                      ((comp/computed-factory cls) (get props id {})
                       {:com.fulcrologic.rad.container/controlled? true})))
        ids       (if (vector? layout)
                    (into [] (comp (mapcat identity)
                                   (map (fn [entry] (if (map? entry) (:id entry) entry))))
                      layout)
                    (mapv first (container/id-child-pairs container-instance)))]
    (vbox {:border? true :color (tuo/option container-instance tuo/border-color) :padding 1 :grow 1}
      (when title (text {:bold true :color (tuo/option container-instance tuo/title-color)} (str title)))
      (render-controls container-instance)
      (line {})
      (vbox {:grow 1}
        (mapv render-id ids)))))
