(ns com.fulcrologic.rad.rendering.tui.picker
  "Shared modal-picker machinery for the fulcro-tui RAD plugin, used by both field pickers
   (enum / to-one ref / to-many ref / autocomplete) and the report/form picker *control*.

   A terminal cannot use React hooks, so the \"which picker is open?\" flag lives in app state under
   `::open-picker` (only one picker is open at a time) and is toggled by the mutations here. An
   autocomplete picker's transient filter string lives under `::autocomplete-filter` (allowed per the
   transient-search-string rule)."
  (:require
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.raw.application :as rapp]
    [com.fulcrologic.fulcro.tui.elements :as e :refer [vbox text button viewport]]
    [com.fulcrologic.fulcro.tui.engine :as engine]
    [com.fulcrologic.rad.tui-options :as tuo]))

(defn value->string
  "Returns a display string for an arbitrary field `value` (empty string for nil)."
  [value]
  (cond
    (nil? value) ""
    (string? value) value
    :else (str value)))

;; ── Picker open/close state (one picker open at a time, kept in app state) ──────────────────────────

(m/defmutation set-open-picker
  "Records which picker (by node id) is currently open; `nil` closes all. When `focus` is supplied, also
   moves keyboard focus to that node id — used on close to return focus to the picker's trigger button
   (otherwise focus resets to the top of the form, since the focused modal option row just disappeared)."
  [{:keys [id focus]}]
  (action [{:keys [state]}]
    (swap! state assoc ::open-picker id)
    (when focus (swap! state assoc ::engine/focus focus))))

(m/defmutation set-autocomplete-filter
  "Records the transient filter string typed into the autocomplete picker `id`."
  [{:keys [id s]}]
  (action [{:keys [state]}] (swap! state assoc-in [::autocomplete-filter id] s)))

(defn picker-open?
  "True when the picker identified by `pick-id` is the currently-open picker."
  [instance pick-id]
  (= pick-id (get (rapp/current-state (comp/any->app instance)) ::open-picker)))

(defn open-picker!
  "Opens the picker `pick-id`, closing any other."
  [instance pick-id]
  (comp/transact! instance [(set-open-picker {:id pick-id})]))

(defn close-picker!
  "Closes any open picker. With `focus-id`, restores focus to that node (the trigger button) so focus
   doesn't jump to the top of the form when the modal's focused option row disappears."
  ([instance] (comp/transact! instance [(set-open-picker {:id nil})]))
  ([instance focus-id] (comp/transact! instance [(set-open-picker {:id nil :focus focus-id})])))

(defn autocomplete-filter
  "The current transient filter string for autocomplete picker `pick-id` (\"\" if none)."
  [instance pick-id]
  (get-in (rapp/current-state (comp/any->app instance)) [::autocomplete-filter pick-id] ""))

(defn modal-id
  "Derives a distinct modal node id from a field's `pick-id` (so the trigger button and the modal don't
   share an id)."
  [pick-id]
  (keyword (namespace pick-id) (str (name pick-id) "-modal")))

(defn option-list
  "Renders the option rows of a picker modal: a scrolling `viewport` of focusable buttons, one per
   `{:text :value}` option. The currently-selected option(s) are check-marked and highlighted (in
   `instance`'s configured `tuo/selected-color`). `value` may not be `name`-able (idents), so row ids
   are index-based. `selected?` is `(fn [value] boolean)`; `on-select` is `(fn [value] ...)`."
  [instance pick-id options selected? on-select]
  (let [sel-color (tuo/option instance tuo/selected-color)]
    (viewport {:id (keyword (namespace pick-id) (str (name pick-id) "-vp")) :grow 1}
      (vbox {}
        (if (seq options)
          (map-indexed
            (fn [i {:keys [text value]}]
              (let [row-id (keyword (namespace pick-id) (str (name pick-id) "-opt-" i))
                    sel?   (selected? value)]
                (button {:id          row-id
                         :highlight   (e/focused? row-id)
                         :color       (if sel? sel-color :bright-white)
                         :on-activate (fn [] (on-select value))}
                  (str (if sel? "✓ " "  ") text))))
            options)
          (text {:color :bright-black} "No options."))))))
