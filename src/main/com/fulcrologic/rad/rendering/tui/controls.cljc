(ns com.fulcrologic.rad.rendering.tui.controls
  "TUI renderers for RAD *controls* — the buttons and inputs on reports/forms/containers that drive
   parameters and actions rather than model data. Each renderer has signature
   `(fn {:keys [instance control-key control]})` (the shape the base-RAD controls-map bridge calls) and
   returns a `com.fulcrologic.fulcro.tui.elements` node.

   These are installed via the `:com.fulcrologic.rad.control/type->style->control` map in
   `com.fulcrologic.rad.rendering.tui.tui-controls`."
  (:require
    [cljc.java-time.local-date :as ld]
    [cljc.java-time.local-date-time :as ldt]
    [cljc.java-time.local-time :as lt]
    [clojure.string :as str]
    [com.fulcrologic.fulcro.algorithms.lambda :as lambda]
    [com.fulcrologic.fulcro.tui.elements :as e :refer [vbox hbox text input button]]
    [com.fulcrologic.rad.control :as control]
    [com.fulcrologic.rad.options-util :refer [?!]]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.rendering.tui.picker :as picker]
    [com.fulcrologic.rad.tui-options :as tuo]
    [com.fulcrologic.rad.type-support.date-time :as dt]))

(defn- control-id [control-key] (keyword "control" (name control-key)))
(defn- visible? [control instance] (let [v (:visible? control)] (or (nil? v) (?! v instance))))
(defn- label-of [control control-key instance] (or (?! (:label control) instance) (name control-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button / string / boolean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-button-control
  "Renders a `:button` control as a fulcro-tui `button`. Activation invokes the control's `:action`,
   called arity-tolerantly (`(fn [this])` or `(fn [this control-key])`)."
  [{:keys [instance control-key control]}]
  (let [{:keys [action disabled? shortcut]} control
        ctl-id (control-id control-key)]
    (when (visible? control instance)
      (button (cond-> {:id          ctl-id
                       :color       (tuo/option instance tuo/action-color)
                       :bold        true
                       :highlight   (e/focused? ctl-id)
                       :on-activate (fn [] (when (and action (not (?! disabled? instance)))
                                             ((lambda/->arity-tolerant action) instance control-key)))}
                shortcut (assoc :shortcut shortcut))
        (str " " (label-of control control-key instance) " ")))))

(defn render-string-control
  "Renders a `:string` control (`:default`/`:search`) as a labelled `input`. Edits call the control's
   `:onChange` (after storing the value via `control/set-parameter!`). The `:search` style prefixes a
   magnifier on the label."
  [{:keys [instance control-key control] :as render-env}]
  (let [{:keys [onChange style]} control
        value (control/current-value instance control-key)
        lbl   (str (when (= :search style) "⌕ ") (label-of control control-key instance))]
    (when (visible? control instance)
      (hbox {:height 1}
        (text {:width (tuo/option instance tuo/field-label-width) :color (tuo/option instance tuo/label-color)} lbl)
        (input {:id        (control-id control-key)
                :grow      1
                :color     (tuo/option instance tuo/value-color)
                :value     (picker/value->string value)
                :on-change (fn [v & _]
                             (control/set-parameter! instance control-key v)
                             (when onChange (onChange instance v)))})))))

(defn render-boolean-control
  "Renders a `:boolean` control as a `[x]`/`[ ]` toggle button. Activation flips the parameter then
   invokes the control's `:onChange` (and `:action`, if any)."
  [{:keys [instance control-key control]}]
  (let [{:keys [onChange action]} control
        value  (boolean (control/current-value instance control-key))
        ctl-id (control-id control-key)]
    (when (visible? control instance)
      (button {:id          ctl-id
               :color       (if value :bright-green :bright-white)
               :highlight   (e/focused? ctl-id)
               :on-activate (fn []
                              (let [nv (not value)]
                                (control/set-parameter! instance control-key nv)
                                (when onChange (onChange instance nv))
                                (when action (action instance nv))))}
        (str (if value " [x] " " [ ] ") (label-of control control-key instance))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picker control (button + modal list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-picker-control
  "Renders a `:picker` control as a button + modal list. Selecting sets the control parameter and runs
   the control's `:action` (e.g. `report/filter-rows!`). Options come from the picker-options cache
   (`po/current-picker-options`); they must be preloaded into that cache at startup (a render-time
   load would re-stamp state every frame and loop)."
  [{:keys [instance control-key control]}]
  (let [{:keys [action]} control
        value   (control/current-value instance control-key)
        options (vec (po/current-picker-options instance control))
        ctl-id  (control-id control-key)
        pick-id (keyword "control-pick" (name control-key))
        cur-lbl (some (fn [opt] (when (= (:value opt) value) (:text opt))) options)]
    (when (visible? control instance)
      (vbox {}
        (hbox {:height 1}
          (text {:width (tuo/option instance tuo/field-label-width) :color (tuo/option instance tuo/label-color)}
            (label-of control control-key instance))
          (button {:id          ctl-id
                   :color       (tuo/option instance tuo/picker-color)
                   :highlight   (e/focused? ctl-id)
                   :on-activate (fn [] (picker/open-picker! instance pick-id))}
            (str " " (or cur-lbl value "(any)") " ▾")))
        (e/modal {:id (picker/modal-id pick-id) :open? (picker/picker-open? instance pick-id)
                  :title (label-of control control-key instance)
                  :width 40 :height 12 :on-dismiss (fn [] (picker/close-picker! instance pick-id))}
          (picker/option-list instance pick-id options
            (fn [v] (= v value))
            (fn [v]
              (control/set-parameter! instance control-key v)
              (picker/close-picker! instance pick-id)
              (when action ((lambda/->arity-tolerant action) instance control-key)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instant controls (date input variants)
;;
;; A terminal edits dates at day granularity (YYYY-MM-DD). The variants differ only in the time-of-day
;; / day-offset applied when turning the typed date into the stored inst, mirroring SUI's
;; controls/instant_inputs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- date-control
  "Renders an `:instant` control as a labelled `YYYY-MM-DD` input. `display` turns the stored inst into
   the date string shown; `parse` turns the typed date string into the inst to store. Both run through
   `control/set-parameter!` + the control's `:onChange`/`:action`."
  [display parse {:keys [instance control-key control]}]
  (let [{:keys [onChange action]} control
        value (control/current-value instance control-key)]
    (when (visible? control instance)
      (hbox {:height 1}
        (text {:width (tuo/option instance tuo/field-label-width) :color (tuo/option instance tuo/label-color)}
          (label-of control control-key instance))
        (input {:id        (control-id control-key)
                :grow      1
                :color     (tuo/option instance tuo/value-color)
                :value     (display value)
                :on-change (fn [v & _]
                             (let [inst (parse v)]
                               (control/set-parameter! instance control-key inst)
                               (when onChange (onChange instance inst))
                               (when action ((lambda/->arity-tolerant action) instance control-key))))})))))

(defn- html-date [value] (if (inst? value) (dt/inst->html-date value) ""))
(defn- pad2 [n] (let [s (str n)] (if (= 1 (count s)) (str "0" s) s)))
(defn- normalized-date-str
  "Returns `v` normalized to a zero-padded `YYYY-MM-DD` string when it is a complete date (tolerating a
   non-padded month/day such as `2020-10-1`), else nil."
  [v]
  (when-let [[_ y m d] (re-matches #"(\d{4})-(\d{1,2})-(\d{1,2})" (str/trim (or v "")))]
    (str y "-" (pad2 m) "-" (pad2 d))))

(defn date-time-control
  "Renders an `:instant :default` control as a date input (midnight on the chosen day)."
  [render-env]
  (date-control html-date (fn [v] (when-let [s (normalized-date-str v)] (dt/html-date->inst s lt/midnight))) render-env))

(defn midnight-on-date-control
  "Renders an `:instant :starting-date` control: stores midnight on the chosen day (inclusive start)."
  [render-env]
  (date-control html-date (fn [v] (when-let [s (normalized-date-str v)] (dt/html-date->inst s lt/midnight))) render-env))

(defn date-at-noon-control
  "Renders an `:instant :date-at-noon` control: stores noon on the chosen day."
  [render-env]
  (date-control html-date (fn [v] (when-let [s (normalized-date-str v)] (dt/html-date->inst s lt/noon))) render-env))

(defn midnight-next-date-control
  "Renders an `:instant :ending-date` control: shows the chosen day but stores midnight of the NEXT day,
   for a proper non-inclusive end instant."
  [render-env]
  (date-control
    (fn [value] (if (inst? value)
                  (-> value dt/inst->local-datetime
                    (ldt/minus-days 1)
                    ldt/to-local-date
                    dt/local-date->html-date-string)
                  ""))
    (fn [v] (when-let [s (normalized-date-str v)]
              (-> (dt/html-date-string->local-date s)
                (ld/plus-days 1)
                (ld/at-time lt/midnight)
                dt/local-datetime->inst)))
    render-env))
