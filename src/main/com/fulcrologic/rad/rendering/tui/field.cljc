(ns com.fulcrologic.rad.rendering.tui.field
  "TUI field renderers for the fulcro-rad-statecharts form engine.

   Field renderers have signature `(fn [env attribute])` (see `com.fulcrologic.rad.statechart.form/render-field`)
   and return a `com.fulcrologic.fulcro.tui.elements` node: typically an `hbox` of a label `text` plus an
   editing element whose `:on-change`/`:on-activate` informs the form statechart (`form/input-changed!`).

   Pickers (enum / to-one ref / to-many ref / autocomplete / scalar :picker) open a focus-trapping modal
   list via the shared machinery in `com.fulcrologic.rad.rendering.tui.picker`. Colors and the label
   column width are configurable through `com.fulcrologic.rad.tui-options`.

   Requiring this ns installs every `fr/render-field` defmethod (the fulcro-rad-statecharts field
   rendering contract is multimethod-based, dispatched on `[attribute-type field-style]`)."
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.raw.application :as rapp]
    [com.fulcrologic.fulcro.tui.elements :as e :refer [vbox hbox text input button]]
    [com.fulcrologic.rad.attributes :as-alias attr]
    [com.fulcrologic.rad.attributes-options :as ao]
    [com.fulcrologic.rad.form :as-alias rform]
    [com.fulcrologic.rad.form-render :as fr]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.rendering.tui.picker :as picker]
    [com.fulcrologic.rad.statechart.form :as form]
    [com.fulcrologic.rad.tui-options :as tuo]
    [com.fulcrologic.rad.type-support.date-time :as dt]
    [com.fulcrologic.rad.type-support.decimal :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- field-node-id
  "A focus/caret id for a field that is unique per form INSTANCE (so the same attribute on multiple
   subform rows — e.g. each line item's `:quantity` — gets a distinct id). `prefix` namespaces the id."
  [form-instance qualified-key prefix]
  (keyword prefix (str (some-> (comp/get-ident form-instance) second)
                    "_" (namespace qualified-key) "_" (name qualified-key))))

(defn- label-text
  "Returns the label string for `attribute`, suffixed with `*` when the attribute is required."
  [field-label qualified-key attribute]
  (str (or field-label (some-> qualified-key name str/capitalize))
    (when (ao/required? attribute) "*")))

(defn- label-cell
  "Renders the leading label column for a field on `form-instance`, colored with the invalid color when
   the field is invalid. The validation MESSAGE is rendered separately by `with-validation`."
  [form-instance {:keys [field-label invalid?]} qualified-key attribute]
  (text {:width (tuo/option form-instance tuo/field-label-width)
         :color (if invalid? (tuo/option form-instance tuo/invalid-color) (tuo/option form-instance tuo/label-color))}
    (label-text field-label qualified-key attribute)))

(defn- with-validation
  "Wraps a field's primary `row` node. When the field is invalid, returns a `vbox` of the row followed
   by a validation-message row (indented under the value column, in the invalid color); otherwise
   returns `row` unchanged. Keeps messages off the label so fields stay aligned and readable."
  [form-instance {:keys [invalid? validation-message]} row]
  (if invalid?
    (vbox {}
      row
      (hbox {:height 1}
        (text {:width (tuo/option form-instance tuo/field-label-width)} "")
        (text {:color (tuo/option form-instance tuo/invalid-color)} (str "↳ " (or validation-message "Invalid")))))
    row))

(defn- parse-long-safe
  "Returns the integer value of string `s`, or nil if it does not parse."
  [s]
  (when (and (string? s) (re-matches #"-?\d+" (str/trim s)))
    #?(:clj (Long/parseLong (str/trim s)) :cljs (js/parseInt s 10))))

(defn- parse-double-safe
  "Returns the double value of string `s`, or nil if it does not parse as a number."
  [s]
  (when (and (string? s) (seq (str/trim s)) (re-matches #"-?\d*\.?\d+" (str/trim s)))
    (try #?(:clj (Double/parseDouble (str/trim s)) :cljs (js/parseFloat (str/trim s)))
         (catch #?(:clj Exception :cljs :default) _ nil))))

(defn- parse-decimal-safe
  "Returns the RAD decimal value of string `s`, or nil if it doesn't parse as a number."
  [s]
  (when (and (string? s) (seq (str/trim s)) (re-matches #"-?\d*\.?\d+" (str/trim s)))
    (try (math/numeric (str/trim s)) (catch #?(:clj Exception :cljs :default) _ nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scalar field renderers (string / text / int / long / double / decimal / multi-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn field-renderer
  "Returns a field render fn (`(fn [env attribute])`) that draws a labelled `input`. `string->model`
   converts the raw edited string into the value stored on the form (defaults to identity);
   `input-attrs` are merged onto the `input` node (e.g. `{:multiline? true :height 4}`).

   On change we set the value **synchronously** with `m/set-value!!` (so the controlled terminal input
   reflects the keystroke on the same render — `input-changed!` alone posts an async statechart event
   and the input would snap back), and ALSO fire `input-changed!` so the form's triggers
   (`:derive-fields`, validation, dependent pickers) run."
  ([] (field-renderer identity {}))
  ([string->model] (field-renderer string->model {}))
  ([string->model input-attrs]
   (fn [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
     (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)]
       (when visible?
         (with-validation form-instance ctx
           (hbox {:height (or (:height input-attrs) 1)}
             (label-cell form-instance ctx qualified-key attribute)
             (input (merge {:id        (field-node-id form-instance qualified-key "field")
                            :grow      1
                            :color     (tuo/option form-instance tuo/value-color)
                            :value     (picker/value->string value)
                            :on-change (fn [v & _]
                                         (when-not read-only?
                                           (let [model (string->model v)]
                                             (m/set-value!! form-instance qualified-key model)
                                             (form/input-changed! env qualified-key model))))}
                      input-attrs)))))))))

(def render-string-field
  "Renders a :string (or :text) attribute as a labelled text input."
  (field-renderer identity))

(def render-multi-line-field
  "Renders a :string :multi-line attribute as a labelled multi-row text area (Enter inserts a newline)."
  (field-renderer identity {:multiline? true :height 4}))

(def render-int-field
  "Renders an :int/:long attribute as a labelled text input, coercing the edited text to a Long when
   valid (a not-yet-numeric in-progress edit is kept as the raw string so typing still shows)."
  (field-renderer (fn [s] (or (parse-long-safe s) s))))

(def render-double-field
  "Renders a :double attribute as a labelled text input, coercing to a double when valid; an
   in-progress edit is kept as the raw string."
  (field-renderer (fn [s] (or (parse-double-safe s) s))))

(def render-decimal-field
  "Renders a :decimal attribute as a labelled text input, coercing to a RAD decimal when valid so
   derived fields (subtotal/total) recompute; an in-progress edit is kept as the raw string."
  (field-renderer (fn [s] (or (parse-decimal-safe s) s))))

(defn render-currency-field
  "Renders a :decimal :USD attribute as a `$`-prefixed labelled input. The stored value is a RAD
   decimal; the input shows `$<amount>` and strips `$`/`,` before parsing back to a decimal (an
   in-progress edit is kept as the raw string)."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        display (cond
                  (string? value) value
                  (some? value)   (str "$" (math/numeric->str value))
                  :else           "")]
    (when visible?
      (with-validation form-instance ctx
        (hbox {:height 1}
          (label-cell form-instance ctx qualified-key attribute)
          (input {:id        (field-node-id form-instance qualified-key "field")
                  :grow      1
                  :color     (tuo/option form-instance tuo/value-color)
                  :value     display
                  :on-change (fn [v & _]
                               (when-not read-only?
                                 (let [clean (str/replace (str v) #"[$,]" "")
                                       model (or (parse-decimal-safe clean) v)]
                                   (m/set-value!! form-instance qualified-key model)
                                   (form/input-changed! env qualified-key model))))}))))))

(defn render-instant-field
  "Renders an :instant attribute as a labelled `YYYY-MM-DD` date input. The stored value is an inst;
   the input shows/edits the html date string and parses it back to an inst when complete (an
   in-progress edit is kept as the raw string so typing still shows)."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        display (cond
                  (string? value) value
                  (inst? value) (dt/inst->html-date value)
                  :else "")]
    (when visible?
      (with-validation form-instance ctx
        (hbox {:height 1}
          (label-cell form-instance ctx qualified-key attribute)
          (input {:id        (field-node-id form-instance qualified-key "field")
                  :grow      1
                  :color     (tuo/option form-instance tuo/value-color)
                  :value     display
                  :on-change (fn [v & _]
                               (when-not read-only?
                                 (let [model (if (re-matches #"\d{4}-\d{2}-\d{2}" (str/trim (or v "")))
                                               (dt/html-date->inst (str/trim v))
                                               v)]
                                   (m/set-value!! form-instance qualified-key model)
                                   (form/input-changed! env qualified-key model))))}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Password fields (masked)
;;
;; The fulcro-tui `input` is controlled and has no native mask, so we display the value as bullets and
;; reconstruct edits by an end-of-string diff of the masked display (covers append + backspace — the
;; realistic terminal password-entry case; mid-string edits while masked are not reconstructed).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(m/defmutation ^:private set-reveal
  "Records whether the masked field `id` is currently revealed (transient UI state)."
  [{:keys [id v]}]
  (action [{:keys [state]}] (swap! state assoc-in [::revealed id] v)))

(defn- revealed?
  "True when masked field `id` is currently revealed."
  [instance id]
  (boolean (get-in (rapp/current-state (comp/any->app instance)) [::revealed id])))

(defn- toggle-reveal! [instance id] (comp/transact! instance [(set-reveal {:id id :v (not (revealed? instance id))})]))

(defn- mask
  "Returns a bullet string the same length as `s`."
  [s]
  (apply str (repeat (count (str s)) "•")))

(defn- apply-masked-edit
  "Reconstructs the real password value from the existing `old` value and the engine's `proposed` edit
   of the masked display: a longer proposal appended literal characters; a shorter one truncated from
   the end."
  [old proposed]
  (let [old (str old) on (count (mask old)) pn (count proposed)]
    (cond
      (> pn on) (str old (subs proposed on))
      (< pn on) (subs old 0 (max 0 pn))
      :else     old)))

(defn- masked-input-field
  "Renders a password field. When `reveal?` is true a focusable show/hide toggle is offered (the
   `:viewable-password` style); otherwise the value is always masked (`:password`)."
  [reveal? {::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        node-id   (field-node-id form-instance qualified-key "pw")
        reveal-id (field-node-id form-instance qualified-key "pwrev")
        shown?    (and reveal? (revealed? form-instance node-id))
        display   (if shown? (picker/value->string value) (mask value))]
    (when visible?
      (with-validation form-instance ctx
        (hbox {:height 1}
          (label-cell form-instance ctx qualified-key attribute)
          (input {:id        node-id
                  :grow      1
                  :color     (tuo/option form-instance tuo/value-color)
                  :value     display
                  :on-change (fn [v & _]
                               (when-not read-only?
                                 (let [model (if shown? v (apply-masked-edit value v))]
                                   (m/set-value!! form-instance qualified-key model)
                                   (form/input-changed! env qualified-key model))))})
          (when reveal?
            (button {:id          reveal-id
                     :color       :bright-black
                     :highlight   (e/focused? reveal-id)
                     :on-activate (fn [] (toggle-reveal! form-instance node-id))}
              (if shown? " hide " " show "))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-boolean-field
  "Renders a :boolean attribute as a focusable `[x]`/`[ ]` toggle button. Activating flips the value."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        toggle-id (field-node-id form-instance qualified-key "bool")]
    (when visible?
      (with-validation form-instance ctx
        (hbox {:height 1}
          (label-cell form-instance ctx qualified-key attribute)
          (button {:id          toggle-id
                   :color       (if value :bright-green :bright-white)
                   :highlight   (e/focused? toggle-id)
                   :on-activate (fn [] (when-not read-only?
                                         (let [nv (not value)]
                                           (m/set-value!! form-instance qualified-key nv)
                                           (form/input-changed! env qualified-key nv))))}
            (str (if value " [x] " " [ ] ") (if value "Yes" "No"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enum / keyword / scalar :picker field (modal picker over enumerated labels)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-enum-field
  "Renders an :enum/:keyword attribute (or any scalar with `:picker` style) as a button showing the
   current label; activating opens a modal list of the enumerated options (`ao/enumerated-labels` /
   `ao/enumerated-values`). Selecting sets the value and closes. Works for non-keyword values (int/
   long/string pickers) — option text falls back to the stringified value when no label is declared."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        labels    (ao/enumerated-labels attribute)
        values    (or (some-> (ao/enumerated-values attribute) vec) (vec (keys labels)))
        opt-label (fn [v] (str (get labels v (picker/value->string v))))
        options   (mapv (fn [v] {:text (opt-label v) :value v}) values)
        pick-id   (field-node-id form-instance qualified-key "enum")
        cur-lbl   (when (some? value) (opt-label value))]
    (when visible?
      (vbox {}
        (with-validation form-instance ctx
          (hbox {:height 1}
            (label-cell form-instance ctx qualified-key attribute)
            (button {:id          pick-id
                     :color       (tuo/option form-instance tuo/picker-color)
                     :highlight   (e/focused? pick-id)
                     :on-activate (fn [] (when-not read-only? (picker/open-picker! form-instance pick-id)))}
              (str " " (or cur-lbl "(choose)") " ▾"))))
        (e/modal {:id (picker/modal-id pick-id) :open? (picker/picker-open? form-instance pick-id)
                  :title (label-text (:field-label ctx) qualified-key attribute)
                  :width 40 :height 12 :on-dismiss (fn [] (picker/close-picker! form-instance pick-id))}
          (picker/option-list form-instance pick-id options
            (fn [v] (= v value))
            (fn [v]
              (m/set-value!! form-instance qualified-key v)
              (form/input-changed! env qualified-key v)
              (picker/close-picker! form-instance pick-id))))))))

(defn render-sorted-set-field
  "Renders a :string :sorted-set attribute as a modal picker over the fixed value set declared via the
   field-style-config `:sorted-set/valid-values`. Stores the chosen string."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        values  (vec (form/field-style-config env attribute :sorted-set/valid-values))
        options (mapv (fn [v] {:text (picker/value->string v) :value v}) values)
        pick-id (field-node-id form-instance qualified-key "sset")]
    (when visible?
      (vbox {}
        (with-validation form-instance ctx
          (hbox {:height 1}
            (label-cell form-instance ctx qualified-key attribute)
            (button {:id          pick-id
                     :color       (tuo/option form-instance tuo/picker-color)
                     :highlight   (e/focused? pick-id)
                     :on-activate (fn [] (when-not read-only? (picker/open-picker! form-instance pick-id)))}
              (str " " (or (picker/value->string value) "(choose)") " ▾"))))
        (e/modal {:id (picker/modal-id pick-id) :open? (picker/picker-open? form-instance pick-id)
                  :title (label-text (:field-label ctx) qualified-key attribute)
                  :width 40 :height 12 :on-dismiss (fn [] (picker/close-picker! form-instance pick-id))}
          (picker/option-list form-instance pick-id options
            (fn [v] (= v value))
            (fn [v]
              (m/set-value!! form-instance qualified-key v)
              (form/input-changed! env qualified-key v)
              (picker/close-picker! form-instance pick-id))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ref pickers (to-one / to-many) — picker-options driven
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ref-value->ident
  "Normalizes a to-one `:ref` field's current value to an ident `[id-key id]` (or nil). A loaded ref
   is a denormalized `{id-key id}` map; a just-selected ref (set via `m/set-value!!`) is a bare ident
   vector. Both must resolve to the same ident so the current selection matches an option."
  [target-id-key value]
  (cond
    (and (vector? value) (= 2 (count value)) (keyword? (first value))) value
    (and (map? value) (some? (get value target-id-key)))               [target-id-key (get value target-id-key)]
    :else nil))

(defn render-ref-pick-one
  "Renders a to-one `:ref` field as a button showing the current selection; activating opens a modal
   list of options from the picker-options cache (`po/current-form-options`). Selecting sets the ref
   ident, fires `input-changed!` (so dependent pickers/derives run), and closes."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        options     (vec (po/current-form-options form-instance attribute))
        current-val (ref-value->ident (ao/target attribute) value)
        current-lbl (some (fn [opt] (when (= (:value opt) current-val) (:text opt))) options)
        pick-id     (field-node-id form-instance qualified-key "pick")]
    (when visible?
      (vbox {}
        (with-validation form-instance ctx
          (hbox {:height 1}
            (label-cell form-instance ctx qualified-key attribute)
            (button {:id          pick-id
                     :color       (tuo/option form-instance tuo/picker-color)
                     :highlight   (e/focused? pick-id)
                     :on-activate (fn [] (when-not read-only? (picker/open-picker! form-instance pick-id)))}
              (str " " (or current-lbl "(choose)") " ▾"))))
        (e/modal {:id (picker/modal-id pick-id) :open? (picker/picker-open? form-instance pick-id)
                  :title (str "Select " (label-text (:field-label ctx) qualified-key attribute))
                  :width 50 :height 14 :on-dismiss (fn [] (picker/close-picker! form-instance pick-id))}
          (picker/option-list form-instance pick-id options
            (fn [v] (= v current-val))
            (fn [v]
              (m/set-value!! form-instance qualified-key v)
              (form/input-changed! env qualified-key v)
              (picker/close-picker! form-instance pick-id))))))))

(defn render-ref-pick-many
  "Renders a to-many `:ref` field. The current selections are listed; a button opens a modal
   multi-select where activating an option toggles its membership. The stored value is a vector of
   idents."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        options   (vec (po/current-form-options form-instance attribute))
        selected  (set (or value []))
        pick-id   (field-node-id form-instance qualified-key "pickm")
        sel-label (fn [v] (some (fn [{:keys [text value]}] (when (= value v) text)) options))
        toggle    (fn [v] (vec (if (contains? selected v) (disj selected v) (conj selected v))))]
    (when visible?
      (vbox {}
        (with-validation form-instance ctx
          (hbox {:height 1}
            (label-cell form-instance ctx qualified-key attribute)
            (button {:id          pick-id
                     :color       (tuo/option form-instance tuo/picker-color)
                     :highlight   (e/focused? pick-id)
                     :on-activate (fn [] (when-not read-only? (picker/open-picker! form-instance pick-id)))}
              (str " + Edit (" (count selected) ") "))))
        (when (seq selected)
          (vbox {}
            (mapv (fn [v] (text {:color :bright-white} (str "  • " (or (sel-label v) (str v))))) selected)))
        (e/modal {:id (picker/modal-id pick-id) :open? (picker/picker-open? form-instance pick-id)
                  :title (str "Select " (label-text (:field-label ctx) qualified-key attribute))
                  :width 50 :height 14 :on-dismiss (fn [] (picker/close-picker! form-instance pick-id))}
          (picker/option-list form-instance pick-id options
            (fn [v] (contains? selected v))
            (fn [v]
              (let [nv (toggle v)]
                (m/set-value!! form-instance qualified-key nv)
                (form/input-changed! env qualified-key nv)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete (type-to-filter modal list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-autocomplete-field
  "Renders a to-one ref/enum field whose modal contains a filter `input` plus a list that narrows as
   you type. The filter string is transient UI state (`picker/autocomplete-filter`). Options come from
   `po/current-form-options` (refs) or `ao/enumerated-labels` (enums)."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
        enum?       (= :enum (ao/type attribute))
        labels      (when enum? (ao/enumerated-labels attribute))
        all-options (if enum?
                      (mapv (fn [v] {:text (str (get labels v (picker/value->string v))) :value v})
                        (or (some-> (ao/enumerated-values attribute) vec) (vec (keys labels))))
                      (vec (po/current-form-options form-instance attribute)))
        current-val (if enum? value (po/current-to-one-value form-instance attribute))
        current-lbl (if enum?
                      (when (some? value) (str (get labels value (picker/value->string value))))
                      (po/current-to-one-label form-instance attribute))
        pick-id     (field-node-id form-instance qualified-key "auto")
        filter-id   (keyword (namespace pick-id) (str (name pick-id) "-filter"))
        flt         (picker/autocomplete-filter form-instance pick-id)
        flt-lc      (str/lower-case (str/trim flt))
        options     (if (seq flt-lc)
                      (filterv (fn [{:keys [text]}] (str/includes? (str/lower-case (str text)) flt-lc)) all-options)
                      all-options)]
    (when visible?
      (vbox {}
        (with-validation form-instance ctx
          (hbox {:height 1}
            (label-cell form-instance ctx qualified-key attribute)
            (button {:id          pick-id
                     :color       (tuo/option form-instance tuo/picker-color)
                     :highlight   (e/focused? pick-id)
                     :on-activate (fn [] (when-not read-only? (picker/open-picker! form-instance pick-id)))}
              (str " " (or current-lbl "(choose)") " ▾"))))
        (e/modal {:id (picker/modal-id pick-id) :open? (picker/picker-open? form-instance pick-id)
                  :title (str "Find " (label-text (:field-label ctx) qualified-key attribute))
                  :width 50 :height 14 :on-dismiss (fn [] (picker/close-picker! form-instance pick-id))}
          (vbox {:grow 1}
            (hbox {:height 1}
              (text {:width 8 :color (tuo/option form-instance tuo/label-color)} "Filter:")
              (input {:id        filter-id
                      :grow      1
                      :color     (tuo/option form-instance tuo/value-color)
                      :value     flt
                      :on-change (fn [v & _] (comp/transact! form-instance
                                               [(picker/set-autocomplete-filter {:id pick-id :s (or v "")})]))}))
            (e/line {})
            (picker/option-list form-instance pick-id options
              (fn [v] (= v current-val))
              (fn [v]
                (m/set-value!! form-instance qualified-key v)
                (form/input-changed! env qualified-key v)
                (comp/transact! form-instance [(picker/set-autocomplete-filter {:id pick-id :s ""})])
                (picker/close-picker! form-instance pick-id)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field renderer registration (fr/render-field, dispatched on [attribute-type field-style])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fr/render-field [:string :default]           [env a] (render-string-field env a))
(defmethod fr/render-field [:text :default]             [env a] (render-string-field env a))
(defmethod fr/render-field [:string :multi-line]        [env a] (render-multi-line-field env a))
(defmethod fr/render-field [:string :password]          [env a] (masked-input-field false env a))
(defmethod fr/render-field [:string :viewable-password] [env a] (masked-input-field true env a))
(defmethod fr/render-field [:string :sorted-set]        [env a] (render-sorted-set-field env a))
(defmethod fr/render-field [:string :picker]            [env a] (render-enum-field env a))
(defmethod fr/render-field [:string :autocomplete]      [env a] (render-autocomplete-field env a))
(defmethod fr/render-field [:int :default]              [env a] (render-int-field env a))
(defmethod fr/render-field [:int :picker]               [env a] (render-enum-field env a))
(defmethod fr/render-field [:long :default]             [env a] (render-int-field env a))
(defmethod fr/render-field [:long :picker]              [env a] (render-enum-field env a))
(defmethod fr/render-field [:double :default]           [env a] (render-double-field env a))
(defmethod fr/render-field [:decimal :default]          [env a] (render-decimal-field env a))
(defmethod fr/render-field [:decimal :USD]              [env a] (render-currency-field env a))
(defmethod fr/render-field [:boolean :default]          [env a] (render-boolean-field env a))
(defmethod fr/render-field [:keyword :default]          [env a] (render-enum-field env a))
(defmethod fr/render-field [:enum :default]             [env a] (render-enum-field env a))
(defmethod fr/render-field [:enum :autocomplete]        [env a] (render-autocomplete-field env a))
(defmethod fr/render-field [:instant :default]          [env a] (render-instant-field env a))
(defmethod fr/render-field [:instant :date-at-noon]     [env a] (render-instant-field env a))
(defmethod fr/render-field [:instant :picker]           [env a] (render-enum-field env a))
(defmethod fr/render-field [:ref :pick-one]             [env a] (render-ref-pick-one env a))
(defmethod fr/render-field [:ref :pick-many]            [env a] (render-ref-pick-many env a))
(defmethod fr/render-field [:ref :autocomplete]         [env a] (render-autocomplete-field env a))
