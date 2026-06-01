(ns com.fulcrologic.rad.rendering.tui.field-spec
  "Standalone (no-DB) rendering tests for the TUI RAD field renderers: a create `ThingForm` whose
   attributes cover every supported type/style is rendered to a `string-terminal`, and we assert on the
   painted screen, on value-display logic (poke a value into state, re-render), and on a couple of
   keyboard-driven interactions (boolean toggle, enum picker open + select)."
  (:require
    [com.fulcrologic.rad.rendering.tui.test-support :as ts]
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
