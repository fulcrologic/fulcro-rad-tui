(ns com.fulcrologic.rad.rendering.tui.tui-controls
  "Entry point for the fulcro-tui RAD rendering plugin. Requiring this ns installs every renderer (form
   fields & structure, reports, generic controls — via their defmethods) and exposes `all-controls`,
   the map you install with `com.fulcrologic.rad.application/install-ui-controls!` (same shape as the
   semantic-ui plugin's `all-controls`). Each leaf is a render fn returning a
   `com.fulcrologic.fulcro.tui.elements` node instead of DOM.

   ```
   (require '[com.fulcrologic.rad.rendering.tui.tui-controls :as tui])
   (rad-application/install-ui-controls! app tui/all-controls)
   ```

   Form *fields* and *structure* install themselves through the fulcro-rad-statecharts multimethod
   contract (`fr/render-field`, `scform/render-element`, `fr/render-form` …) when their namespaces load
   — they are NOT in `all-controls`. `all-controls` carries the report layouts, the generic controls,
   and the container layout, which ARE consulted through the controls-map (the base-RAD report-render
   bridge and `control/render-control`)."
  (:require
    ;; Required for side effects:
    ;;  * com.fulcrologic.rad.report installs the `:default` methods of the report render multimethods
    ;;    (rr/render-report|render-row|render-controls) that bridge to the installed controls map.
    ;;  * com.fulcrologic.rad.form is the form engine the structural defmethods extend.
    ;;  * field/form register the fr/render-field, scform/render-element and fr/render-* defmethods.
    [com.fulcrologic.rad.report]
    [com.fulcrologic.rad.form]
    [com.fulcrologic.rad.rendering.tui.container :as container]
    [com.fulcrologic.rad.rendering.tui.controls :as controls]
    [com.fulcrologic.rad.rendering.tui.field]
    [com.fulcrologic.rad.rendering.tui.form]
    [com.fulcrologic.rad.rendering.tui.report :as report]))

(def all-controls
  "The fulcro-tui RAD control set. Install with `rad-application/install-ui-controls!` before mounting.
   Renders RAD reports (`:default` table + `:list`), generic form/report controls, and containers as
   fulcro-tui element nodes. Form *fields* and *structural* elements are installed via defmethods
   (loaded as a side effect of requiring this ns), not via this map."
  {;; ── Report renderers ──────────────────────────────────────────────────────
   :com.fulcrologic.rad.report/style->layout
   {:default report/render-table-report-layout
    :list    report/render-list-report-layout}

   :com.fulcrologic.rad.report/row-style->row-layout
   {:default report/render-table-row
    :list    report/render-list-row}

   :com.fulcrologic.rad.report/control-style->control
   {:default report/render-standard-controls}

   ;; ── Container layout (see container ns wiring note) ─────────────────────────
   :com.fulcrologic.rad.container/style->layout
   {:default container/render-container-layout}

   ;; ── Generic controls (buttons / inputs / toggles / pickers / dates) ─────────
   :com.fulcrologic.rad.control/type->style->control
   {:button  {:default controls/render-button-control}
    :string  {:default controls/render-string-control
              :search  controls/render-string-control}
    :boolean {:default controls/render-boolean-control
              :toggle  controls/render-boolean-control}
    :picker  {:default controls/render-picker-control}
    :instant {:default       controls/date-time-control
              :starting-date controls/midnight-on-date-control
              :ending-date   controls/midnight-next-date-control
              :date-at-noon  controls/date-at-noon-control}}})
