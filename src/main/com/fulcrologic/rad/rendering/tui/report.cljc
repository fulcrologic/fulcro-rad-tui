(ns com.fulcrologic.rad.rendering.tui.report
  "TUI report renderers for the fulcro-rad-statecharts report engine. Two styles are provided:

   * `:default` — a table: a control bar, a header row of (optionally sortable) column labels, then a
     scrolling `viewport` of row `hbox`es.
   * `:list` — a compact one-line-per-row list (no column header), each row a focusable summary line.

   Plus the shared control bar (`render-standard-controls`) and pagination bar (`render-page-nav`).
   These are installed via the report controls-map in `com.fulcrologic.rad.rendering.tui.tui-controls`;
   the base-RAD report-render bridge dispatches `render-layout`/`render-row`/`control-renderer` to them
   by the report's layout/row/control style."
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.tui.elements :as e :refer [vbox hbox text button line viewport]]
    [com.fulcrologic.rad.attributes :as-alias attr]
    [com.fulcrologic.rad.attributes-options :as ao]
    [com.fulcrologic.rad.control :as control]
    [com.fulcrologic.rad.options-util :refer [?!]]
    [com.fulcrologic.rad.report :as-alias report]
    [com.fulcrologic.rad.statechart.form :as form]
    [com.fulcrologic.rad.statechart.report :as screport]
    [com.fulcrologic.rad.tui-options :as tuo]))

(defn- col-width [report-instance] (tuo/option report-instance tuo/report-column-width))

(defn- column-label
  "Returns the heading label for report `column` on `report-instance`: the column's own
   `::report/column-heading`, else the report's `ro/column-headings` map entry for the column, else the
   attribute label, else a capitalized key."
  [report-instance {::report/keys [column-heading] ::attr/keys [qualified-key] :as column}]
  (let [headings (?! (comp/component-options report-instance ::report/column-headings) report-instance)]
    (or (?! column-heading report-instance)
      (?! (get headings qualified-key) report-instance)
      (?! (ao/label column) report-instance)
      (some-> qualified-key name str/capitalize)
      "")))

(defn- sort-state
  "Returns the report's current sort parameters map `{:sort-by k :ascending? bool :sortable-columns set}`."
  [report-instance]
  (get-in (comp/props report-instance) [:ui/parameters ::report/sort]))

(defn- render-column-header
  "Renders one column heading. Columns named in `:sortable-columns` render as a focusable button that
   toggles the report sort (an ▲/▼ arrow marks the active sort column + direction); others render as
   plain header text."
  [report-instance {::attr/keys [qualified-key] :as column}]
  (let [{:keys [sort-by ascending? sortable-columns]} (sort-state report-instance)
        sortable? (contains? (set sortable-columns) qualified-key)
        active?   (= sort-by qualified-key)
        arrow     (cond (not active?) "" ascending? " ▲" :else " ▼")
        label     (str (column-label report-instance column) arrow)
        w         (col-width report-instance)]
    (if sortable?
      (let [hid (keyword "sorth" (str (namespace qualified-key) "_" (name qualified-key)))]
        (button {:id          hid :width w :bold true
                 :color       (if active? :bright-yellow :bright-cyan)
                 :highlight   (e/focused? hid)
                 :on-activate (fn [] (screport/sort-rows! report-instance column))}
          label))
      (text {:width w :bold true :color :bright-cyan} label))))

(defn- row-activation
  "Returns a zero-arg activation fn for `row-props`: open its edit form (a `ro/form-links` entry on any
   column) or, failing that, run the report's first `ro/row-action`."
  [report-instance row-props]
  (let [{::report/keys [columns row-actions]} (comp/component-options report-instance)
        link (some (fn [c] (screport/form-link report-instance row-props (::attr/qualified-key c))) columns)]
    (fn []
      (cond
        link              (form/edit! report-instance (:edit-form link) (:entity-id link))
        (seq row-actions) ((:action (first row-actions)) report-instance row-props)))))

(defn render-table-row
  "Renders one report row as a focusable hbox of column cells. Activating the row (Enter/Space) opens
   its edit form or runs the first row action. Terminals have no separate row-selection concept, so
   focus IS the highlight."
  [report-instance _row-class row-props]
  (let [{::report/keys [columns]} (comp/component-options report-instance)
        {::report/keys [idx]} (comp/get-computed row-props)
        row-id (keyword "row" (str idx))
        w      (col-width report-instance)
        cells  (mapv (fn [column]
                       (text {:width w}
                         (str (screport/formatted-column-value report-instance row-props column))))
                 columns)]
    ;; A focusable hbox (NOT a button): an `:id` + `:on-activate` makes any node focusable and
    ;; Enter/Space-activatable, and an hbox lays out the column cells (a `button` is a text leaf and
    ;; would stringify the cell layout).
    (hbox {:id          row-id
           :highlight   (e/focused? row-id)
           :on-activate (row-activation report-instance row-props)}
      cells)))

(defn render-list-row
  "Renders one report row as a compact focusable summary line: the first column's value in bold,
   followed by the remaining columns joined with ` · `. Activates like a table row."
  [report-instance _row-class row-props]
  (let [{::report/keys [columns]} (comp/component-options report-instance)
        {::report/keys [idx]} (comp/get-computed row-props)
        row-id (keyword "lrow" (str idx))
        vals   (mapv (fn [c] (str (screport/formatted-column-value report-instance row-props c))) columns)
        head   (first vals)
        rest-s (str/join " · " (remove str/blank? (rest vals)))]
    (hbox {:id          row-id
           :height      1
           :highlight   (e/focused? row-id)
           :on-activate (row-activation report-instance row-props)}
      (text {:bold true :color :bright-white} (str " " head))
      (when (seq rest-s) (text {:color :bright-black} (str "   " rest-s))))))

(defn render-standard-controls
  "Renders the report control bar: a row of action buttons followed by the input control rows, each
   rendered via `control/render-control`."
  [report-instance]
  (let [{:keys [action-layout input-layout]} (control/standard-control-layout report-instance)]
    (vbox {}
      (when (seq action-layout)
        (hbox {:height 1}
          (mapv (fn [k] (control/render-control report-instance k)) action-layout)))
      (mapv (fn [row]
              (vbox {}
                (mapv (fn [k] (control/render-control report-instance k)) row)))
        input-layout))))

(defn render-page-nav
  "Renders the pagination bar — Prev / Next buttons flanking a `Page X / Y` indicator — when the
   report paginates and has more than one page. A button at the first/last page is shown disabled."
  [report-instance]
  (when (?! (comp/component-options report-instance ::report/paginate?) report-instance)
    (let [page  (screport/current-page report-instance)
          pages (max 1 (screport/page-count report-instance))]
      (when (> pages 1)
        (let [first? (<= page 1)
              last?  (>= page pages)
              nav    (fn [id shortcut label disabled? activate]
                       (button (cond-> {:id          id
                                        :color       (if disabled? :bright-black :cyan)
                                        :highlight   (and (not disabled?) (e/focused? id))
                                        :on-activate (fn [] (when-not disabled? (activate)))}
                                 (not disabled?) (assoc :shortcut shortcut))
                         label))]
          (hbox {:height 1}
            (nav :report/prev-page [:alt "p"] " ◀ Prev " first?
              (fn [] (screport/prior-page! report-instance)))
            (text {:width 14 :bold true :color :bright-cyan}
              (str "  Page " page " / " pages "  "))
            (nav :report/next-page [:alt "n"] " Next ▶ " last?
              (fn [] (screport/next-page! report-instance)))))))))

(defn- report-frame
  "Renders the common report frame (border, control bar, pagination) wrapping `body`. `body` is a
   one-arg fn `(fn [rows] node)` given the current rows so each style lays them out its own way."
  [report-instance body]
  (let [render-controls (screport/control-renderer report-instance)
        rows            (screport/current-rows report-instance)]
    ;; `:grow 1` makes the report fill the vertical space the root frame hands it, so the rows viewport
    ;; can expand to fill the screen and scroll.
    (vbox {:border? true :color (tuo/option report-instance tuo/border-color) :padding 1 :grow 1}
      (when render-controls (render-controls report-instance))
      (render-page-nav report-instance)
      (line {})
      (body rows))))

(defn- rows-viewport
  "A `:grow 1` scrolling viewport of `row-nodes`, or an empty message. Arrowing through the focusable
   rows autoscrolls it (viewport-follows-focus); PageUp/PageDown page it."
  [rows row-nodes]
  (viewport {:id :report-rows :grow 1 :border? true :color :bright-black}
    (vbox {}
      (if (seq rows) row-nodes (text {:color :bright-black} "No rows.")))))

(defn render-table-report-layout
  "Renders the whole table report: control bar, a column header row, and a scrolling viewport of data
   rows (each via `screport/render-row`)."
  [report-instance]
  (let [{::report/keys [columns]} (comp/component-options report-instance)]
    (report-frame report-instance
      (fn [rows]
        (vbox {:grow 1}
          (hbox {:height 1}
            (mapv (fn [c] (render-column-header report-instance c)) columns))
          (line {})
          (rows-viewport rows
            (map-indexed
              (fn [idx row]
                (screport/render-row report-instance nil
                  (comp/computed row {::report/idx idx
                                      :highlighted? (= idx (screport/currently-selected-row report-instance))})))
              rows)))))))

(defn render-list-report-layout
  "Renders the whole list report: control bar then a scrolling viewport of compact summary rows (each
   via `screport/render-row`, which dispatches to the `:list` row style). No column header."
  [report-instance]
  (report-frame report-instance
    (fn [rows]
      (rows-viewport rows
        (map-indexed
          (fn [idx row]
            (screport/render-row report-instance nil
              (comp/computed row {::report/idx idx
                                  :highlighted? (= idx (screport/currently-selected-row report-instance))})))
          rows)))))
