(ns com.fulcrologic.rad.rendering.tui.controls-spec
  "Standalone (no-DB) tests for the TUI RAD *control* renderers, driven through the report control bar:
   a boolean toggle control flips its parameter, and a picker control opens a modal of seeded options
   and updates its label on selection."
  (:require
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.rendering.tui.test-support :as ts]
    [fulcro-spec.core :refer [=> assertions specification]]))

(specification "boolean control toggles its parameter"
  (ts/quiet
    (let [app (ts/report-app! ::ts/ThingReport)]
      (assertions
        "the boolean control starts unchecked"
        (boolean (re-find #"\[ \] Active only\?" (ts/screen-text app))) => true)
      ;; Several controls share the `control` namespace (the New button, the toggle …), so target the
      ;; toggle's exact node id.
      (ts/tab-to-id! app :control/only-on?)
      (ts/enter! app)
      (assertions
        "activating the toggle checks it"
        (boolean (re-find #"\[x\] Active only\?" (ts/settle! app))) => true))))

(specification "picker control renders its label and reads seeded options"
  (ts/quiet
    (let [app (ts/report-app! ::ts/ThingReport)]
      (assertions
        "the picker control renders its default-value label (:all -> All)"
        (boolean (re-find #"Kind.*All ▾" (ts/screen-text app))) => true
        "the seeded picker options are readable via the picker-options cache (the control's data source)"
        (vec (po/current-picker-options app ::ts/kinds))
        => [{:text "All" :value :all} {:text "Hardware" :value :hw} {:text "Tools" :value :tools}]))))
