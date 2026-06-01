(ns com.fulcrologic.rad.rendering.tui.report-spec
  "Standalone (no-DB) rendering tests for the TUI RAD report renderers: a `:default` table report and a
   `:list` report, both fed canned rows by a mock loopback remote. Asserts on column headers + sort
   arrow, the Yes/No and $ column formatters, the control bar (button / search string / boolean /
   instant / picker), the compact list layout, and a keyboard-driven sort toggle."
  (:require
    [com.fulcrologic.rad.rendering.tui.test-support :as ts]
    [fulcro-spec.core :refer [=> assertions specification]]))

(specification "table report layout"
  (ts/quiet
    (let [app    (ts/report-app! ::ts/ThingReport)
          screen (ts/screen-text app)
          has?   (fn [re] (boolean (re-find re screen)))]
      (assertions
        "renders the column headings (custom heading + defaults)"
        (and (has? #"Thing Name") (has? #"Active\?") (has? #"Cost")) => true
        "marks the active sort column with a direction arrow"
        (has? #"Thing Name ▲") => true
        "renders the boolean column via the Yes/No formatter (not raw true/false)"
        (and (has? #"\bYes\b") (has? #"\bNo\b") (not (has? #"\btrue\b"))) => true
        "renders the cost column via the $ formatter"
        (has? #"\$9.99") => true
        "lists the seeded rows"
        (and (has? #"Anvil") (has? #"Bellows") (has? #"Crucible")) => true))))

(specification "report control bar — all control types render"
  (ts/quiet
    (let [app    (ts/report-app! ::ts/ThingReport)
          screen (ts/screen-text app)
          has?   (fn [re] (boolean (re-find re screen)))]
      (assertions
        "renders a :button action control"
        (has? #"New Thing") => true
        "renders a :string :search control (magnifier prefix on the label)"
        (has? #"⌕ Filter") => true
        "renders a :boolean control as a toggle"
        (has? #"\[[ x]\] Active only\?") => true
        "renders an :instant control"
        (has? #"Since") => true
        "renders a :picker control showing its current label (default :all -> All)"
        (has? #"Kind.*All ▾") => true))))

(specification "list report layout"
  (ts/quiet
    (let [app    (ts/report-app! ::ts/ThingList)
          screen (ts/screen-text app)
          has?   (fn [re] (boolean (re-find re screen)))]
      (assertions
        "renders the seeded rows as compact one-line summaries"
        (and (has? #"Anvil") (has? #"Bellows") (has? #"Crucible")) => true
        "joins the trailing columns with a middot, applying formatters ($, Yes/No)"
        (and (has? #"\$9.99 · Yes") (has? #"\$19.50 · No")) => true
        "does NOT render a table column-header row"
        (not (has? #"Thing Name")) => true))))

(specification "report interaction — sortable header toggles direction"
  (ts/quiet
    (let [app (ts/report-app! ::ts/ThingReport)]
      (assertions
        "starts ascending on the initial sort column"
        (boolean (re-find #"Thing Name ▲" (ts/screen-text app))) => true)
      ;; Focus the Name sort header specifically (the Active? header is also a `sorth` node).
      (ts/tab-to-id! app :sorth/thing_name)
      (ts/enter! app)
      (assertions
        "activating the focused sort header flips the direction arrow to descending"
        (boolean (re-find #"Thing Name ▼" (ts/wait-for app #"Thing Name ▼"))) => true))))
