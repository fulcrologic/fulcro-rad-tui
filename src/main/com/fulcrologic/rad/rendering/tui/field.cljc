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
    [cljc.java-time.local-time :as lt]
    [clojure.string :as str]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.raw.application :as rapp]
    [com.fulcrologic.fulcro.react.hooks :as hooks]
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
;; Buffered (commit-on-blur) inputs
;;
;; A terminal `input` is controlled: its edited text must round-trip on every keystroke. For fields
;; whose MODEL value is not a string (e.g. :instant), parsing on each keystroke is wrong — a partly
;; typed value has no valid parse, and an eagerly-parsed value snaps the display out from under the
;; caret mid-entry (typing `2020-10-1` toward `2020-10-12` would jump to `2020-10-01`). So we hold the
;; in-progress text in transient COMPONENT-LOCAL state (`hooks/use-state`) and commit the PARSED value
;; only when the field loses focus / is submitted; the model then only ever holds a real value (or nil),
;; never a raw editing string — which is what made saves fail.
;;
;; `use-state` works here because fulcro-tui establishes Fulcro's headless hook render-path context
;; during its render walk (so hooks persist across renders despite each render rebuilding the component,
;; and are isolated per render-path — hence per to-many subform row). A transient input buffer is
;; exactly the kind of "transient user string" hooks are appropriate for; no app-db state is involved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn buffered-field-renderer
  "Returns a field render fn (`(fn [env attribute])`) drawing a labelled `input` whose in-progress text
   is held in component-local `hooks/use-state` and committed to the model only on blur/submit, so the
   model only ever holds a parsed value — never a raw editing string. Hook state is per render-path, so
   to-many subform rows each edit independently.

     * `value->display` - `(fn [model-value])` => the input string shown when no edit is in progress.
     * `string->model`  - `(fn [edited-string])` => the model value to store on commit; return the
                          sentinel `::invalid` to reject the commit and keep the user's text for
                          correction (the model is left unchanged).
     * `input-attrs`    - (optional) extra attrs merged onto the `input` node (e.g. `{:height 4}`)."
  ([value->display string->model] (buffered-field-renderer value->display string->model {}))
  ([value->display string->model input-attrs]
   (fn [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
     (let [{:keys [value visible? read-only?] :as ctx} (form/field-context env attribute)
           ;; nil buffer = not currently editing (show the model value); a string = in-progress edit.
           [buf set-buf!] (hooks/use-state nil)
           display        (if (some? buf) buf (value->display value))
           commit!        (fn []
                            (when (and (not read-only?) (some? buf))
                              (let [model (string->model buf)]
                                (when-not (= ::invalid model)
                                  (m/set-value!! form-instance qualified-key model)
                                  (form/input-changed! env qualified-key model)
                                  (set-buf! nil)))))]
       (when visible?
         (with-validation form-instance ctx
           (hbox {:height (or (:height input-attrs) 1)}
             (label-cell form-instance ctx qualified-key attribute)
             (input (merge {:id            (field-node-id form-instance qualified-key "field")
                            :grow          1
                            :color         (tuo/option form-instance tuo/value-color)
                            :value         display
                            :on-change     (fn [v & _] (when-not read-only? (set-buf! (or v ""))))
                            :on-lost-focus (fn [_] (commit!))
                            :on-submit     (fn [_] (commit!))}
                       input-attrs)))))))))

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

;; Numeric fields store a NON-string model (Long / double / RAD decimal), so they are buffered like the
;; date field: the typed text is held in `use-state` and parsed only on blur/submit (so the model never
;; holds a half-typed string — derived fields recompute on commit). `string->model` returns the parsed
;; number, nil for a blank field, or `::invalid` to keep an unparseable in-progress edit for correction.
(defn- commit-number
  "Returns a `string->model` for `buffered-field-renderer`: blank -> nil, else `(parse t)` or `::invalid`."
  [parse]
  (fn [s] (let [t (str/trim (or s ""))] (if (str/blank? t) nil (or (parse t) ::invalid)))))

(defn- num-display [v] (if (some? v) (str v) ""))

(def render-int-field
  "Renders an :int/:long attribute as a buffered text input, committing a Long on blur."
  (buffered-field-renderer num-display (commit-number parse-long-safe)))

(def render-double-field
  "Renders a :double attribute as a buffered text input, committing a double on blur."
  (buffered-field-renderer num-display (commit-number parse-double-safe)))

(def render-decimal-field
  "Renders a :decimal attribute as a buffered text input, committing a RAD decimal on blur."
  (buffered-field-renderer
    (fn [v] (if (some? v) (math/numeric->str v) ""))
    (commit-number parse-decimal-safe)))

(def render-currency-field
  "Renders a :decimal :USD attribute as a `$`-prefixed buffered input: shows `$<amount>`, strips `$`/`,`
   before parsing, and commits a RAD decimal on blur."
  (buffered-field-renderer
    (fn [v] (if (some? v) (str "$" (math/numeric->str v)) ""))
    (commit-number (fn [s] (parse-decimal-safe (str/replace (or s "") #"[$,]" ""))))))

(defn- pad2
  "Left-pads the string of `n` to width 2 with a leading zero (e.g. `\"1\"` -> `\"01\"`)."
  [n]
  (let [s (str n)] (if (= 1 (count s)) (str "0" s) s)))

(defn- date-string->inst
  "Parses a typed date string `s` into an inst at `local-time` on that day, tolerating non-zero-padded
   month/day (e.g. `2020-10-1`). Returns nil for a blank string and the `::invalid` sentinel for a
   non-blank string that is not yet a complete `YYYY-M-D` date (so an in-progress edit is not
   committed). Used as the `string->model` for `buffered-field-renderer`."
  [s local-time]
  (let [t (str/trim (or s ""))]
    (cond
      (str/blank? t)                          nil
      (re-matches #"\d{4}-\d{1,2}-\d{1,2}" t) (let [[y m d] (str/split t #"-")]
                                                (dt/html-date->inst (str y "-" (pad2 m) "-" (pad2 d)) local-time))
      :else                                   ::invalid)))

(defn- instant-field-renderer
  "Returns an :instant field renderer (`(fn [env attribute])`): a buffered (commit-on-blur) `YYYY-MM-DD`
   date input that stores an inst at `local-time` on the chosen day (`lt/midnight` for the default /
   start-of-day styles, `lt/noon` for `:date-at-noon`). The typed text is held transiently while
   editing and parsed to an inst only on blur/submit, so the model never holds a partial string."
  [local-time]
  (buffered-field-renderer
    (fn [value] (if (inst? value) (dt/inst->html-date value) ""))
    (fn [s] (date-string->inst s local-time))))

(def render-instant-field
  "Renders an :instant attribute as a labelled `YYYY-MM-DD` date input storing midnight on the chosen day."
  (instant-field-renderer lt/midnight))

(def render-date-at-noon-field
  "Renders an :instant :date-at-noon attribute as a date input storing noon on the chosen day."
  (instant-field-renderer lt/noon))

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
(defmethod fr/render-field [:instant :date-at-noon]     [env a] (render-date-at-noon-field env a))
(defmethod fr/render-field [:instant :picker]           [env a] (render-enum-field env a))
(defmethod fr/render-field [:ref :pick-one]             [env a] (render-ref-pick-one env a))
(defmethod fr/render-field [:ref :pick-many]            [env a] (render-ref-pick-many env a))
(defmethod fr/render-field [:ref :autocomplete]         [env a] (render-autocomplete-field env a))
