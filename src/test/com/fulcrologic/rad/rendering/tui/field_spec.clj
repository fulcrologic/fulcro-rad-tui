(ns com.fulcrologic.rad.rendering.tui.field-spec
  "Standalone (no-DB) rendering tests for the TUI RAD field renderers: a create `ThingForm` whose
   attributes cover every supported type/style is rendered to a `string-terminal`, and we assert on the
   painted screen, on value-display logic (poke a value into state, re-render), and on a couple of
   keyboard-driven interactions (boolean toggle, enum picker open + select)."
  (:require
    [cljc.java-time.local-time :as lt]
    [com.fulcrologic.rad.rendering.tui.field :as field]
    [com.fulcrologic.rad.rendering.tui.test-support :as ts]
    [com.fulcrologic.rad.type-support.date-time :as dt]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [fulcro-spec.core :refer [=> assertions specification]]))

(specification "field structure — every type/style renders its affordance"
  (ts/quiet
    (let [app    (ts/form-app!)
          screen (ts/screen-text app)
          has?   (fn [re] (boolean (re-find re screen)))]
      (assertions
        "renders the form title + Save/Undo/Cancel actions"
        (and (has? #"Edit Thing") (has? #"Save") (has? #"Undo") (has? #"Cancel")) => true
        "marks a required field with an asterisk (Name*)"
        (has? #"Name\*") => true
        "renders scalar string / numeric labels (Bio, Qty, Big, Rating, Price, Cost)"
        (and (has? #"Bio") (has? #"Qty") (has? #"Big") (has? #"Rating") (has? #"Price") (has? #"Cost")) => true
        "renders the viewable-password reveal toggle"
        (has? #"show") => true
        "renders the sorted-set field as a picker (▾)"
        (has? #"Size") => true
        "renders enum/keyword/ref pickers as (choose) ▾ triggers"
        (has? #"\(choose\) ▾") => true
        "renders the boolean field defaulting to [x] Yes (fo/default-value)"
        (has? #"\[x\] Yes") => true
        "renders the to-many subform with its child field + Add/Delete"
        (and (has? #"Label\*") (has? #"\+ Add") (has? #"- Delete")) => true))))

(specification "field value display"
  (ts/quiet
    (let [app (ts/form-app!)]
      (ts/set-field! app :thing/pw "hunter2")
      (ts/set-field! app :thing/token "secretz")
      (ts/set-field! app :thing/cost (math/numeric "42.50"))
      (ts/set-field! app :thing/price (math/numeric "5.00"))
      (ts/set-field! app :thing/qty 7)
      (ts/set-field! app :thing/priority 3)
      (ts/set-field! app :thing/role :role/admin)
      (ts/set-field! app :thing/size "M")
      (let [screen (ts/screen-text app)
            has?   (fn [re] (boolean (re-find re screen)))]
        (assertions
          "masks a :password value as bullets (not the plaintext)"
          (and (has? #"•••••••") (not (has? #"hunter2"))) => true
          "masks a :viewable-password value while hidden"
          (not (has? #"secretz")) => true
          "renders a :decimal :USD value with a leading $"
          (has? #"\$42.50") => true
          "renders a plain :decimal value"
          (has? #"5.00") => true
          "renders a numeric value"
          (has? #"\b7\b") => true
          "renders the :int :picker current label (not the raw value)"
          (has? #"High ▾") => true
          "renders the :enum current label"
          (has? #"Administrator ▾") => true
          "renders the :sorted-set current value"
          (has? #"M ▾") => true)))))

(specification "field interaction — boolean toggle"
  (ts/quiet
    (let [app (ts/form-app!)]
      (ts/tab-to-ns! app "bool")
      (ts/enter! app)
      (assertions
        "activating the boolean toggle flips [x] Yes to [ ] No"
        (boolean (re-find #"\[ \] No" (ts/screen-text app))) => true))))

(specification "date-string->inst parsing"
  (dt/with-timezone "America/Los_Angeles"
    (let [p @#'field/date-string->inst]
      (assertions
        "returns nil for a blank string"
        (p "" lt/midnight) => nil
        "rejects an in-progress (incomplete) date with the ::invalid sentinel so it is not committed"
        (p "2020-10-" lt/midnight) => ::field/invalid
        "parses a complete zero-padded date to an inst"
        (inst? (p "2020-10-02" lt/midnight)) => true
        "tolerates a non-zero-padded month/day (e.g. 2020-10-1)"
        (inst? (p "2020-10-1" lt/midnight)) => true
        "stores a different time-of-day for midnight vs noon styles"
        (= (p "2020-10-02" lt/midnight) (p "2020-10-02" lt/noon)) => false))))

(specification "instant field — buffered commit on blur"
  (ts/quiet
    (let [app     (ts/form-app!)
          created (ts/field-id app :thing/created)]
      (ts/tab-to-id! app created)
      (ts/type-str! app "2020-10-1")
      (assertions
        "shows the in-progress text exactly as typed (single-digit day allowed)"
        (boolean (re-find #"2020-10-1" (ts/screen-text app))) => true
        "leaves the model value unset while the field is still being edited (no raw string in model)"
        (ts/field-value app :thing/created) => nil)
      (ts/tab! app)                                         ; blur -> commit
      (assertions
        "commits a real inst (never a raw string) to the model on blur"
        (inst? (ts/field-value app :thing/created)) => true))))

(specification "numeric field — buffered commit on blur"
  (ts/quiet
    (let [app (ts/form-app!)
          qty (ts/field-id app :thing/qty)]
      (ts/tab-to-id! app qty)
      (ts/type-str! app "42")
      (assertions
        "shows the typed digits while editing"
        (boolean (re-find #"\b42\b" (ts/screen-text app))) => true
        "leaves the model value unset while still editing (no raw string in model)"
        (ts/field-value app :thing/qty) => nil)
      (ts/tab! app)                                         ; blur -> commit
      (assertions
        "commits a parsed Long (not a string) on blur"
        (ts/field-value app :thing/qty) => 42))))

(specification "field interaction — enum picker open + select"
  (ts/quiet
    (let [app (ts/form-app!)]
      (ts/tab-to-ns! app "enum")
      (ts/enter! app)
      (let [modal (ts/screen-text app)]
        (ts/enter! app)                                     ; select the focused (first) option
        (assertions
          "opening the picker shows the enumerated options in a modal"
          (and (boolean (re-find #"Low" modal)) (boolean (re-find #"High" modal))) => true
          "selecting an option updates the trigger label and closes the modal"
          (boolean (re-find #"Low ▾" (ts/screen-text app))) => true)))))
