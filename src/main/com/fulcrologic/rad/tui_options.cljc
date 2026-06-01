(ns com.fulcrologic.rad.tui-options
  "Documented option keys for rendering-specific customization when using the fulcro-tui RAD plugin
   (`com.fulcrologic.rad.rendering.tui.*`) as your renderer. This mirrors
   `com.fulcrologic.rad.semantic-ui-options` for the terminal: instead of CSS classes the options are
   cell widths and `com.fulcrologic.fulcro.tui.elements` color keywords (`:cyan`, `:bright-white`, …).

   ALL options MUST appear under the rendering-options key:

   ```
   (ns ...
     (:require
        [com.fulcrologic.rad.tui-options :as tuo]
        ...))

   (defsc-form Form [this props]
     {tuo/rendering-options {tuo/field-label-width 20}})
   ```

   Most options can be given a global default with `set-global-rendering-options!`."
  (:require
    [com.fulcrologic.fulcro.components :as comp]))

(def rendering-options
  "Top-level key for specifying rendering options. All fulcro-tui RAD customization options MUST
   appear under this key (on a form/report/container, or globally)."
  ::rendering-options)

;; ── Layout widths (cells) ───────────────────────────────────────────────────

(def field-label-width
  "Integer cell width of a form field's leading label column. Defaults to 16."
  ::field-label-width)

(def report-column-width
  "Integer cell width of each report table column. Defaults to 18."
  ::report-column-width)

;; ── Colors (`com.fulcrologic.fulcro.tui.elements` color keywords) ─────────────

(def label-color
  "Color of field/control labels. Defaults to `:cyan`."
  ::label-color)

(def value-color
  "Color of editable field values. Defaults to `:bright-white`."
  ::value-color)

(def picker-color
  "Color of picker trigger buttons (enum/ref/autocomplete). Defaults to `:bright-magenta`."
  ::picker-color)

(def title-color
  "Color of the form/report title. Defaults to `:bright-cyan`."
  ::title-color)

(def border-color
  "Color of the outer form/report border. Defaults to `:cyan`."
  ::border-color)

(def invalid-color
  "Color used to flag invalid fields and validation messages. Defaults to `:bright-red`."
  ::invalid-color)

(def action-color
  "Color of action buttons (Save, report action controls). Defaults to `:green`."
  ::action-color)

(def selected-color
  "Color marking the selected option(s) inside a picker modal. Defaults to `:bright-green`."
  ::selected-color)

(def ^:private defaults
  {field-label-width  16
   report-column-width 18
   label-color        :cyan
   value-color        :bright-white
   picker-color       :bright-magenta
   title-color        :bright-cyan
   border-color       :cyan
   invalid-color      :bright-red
   action-color       :green
   selected-color     :bright-green})

(defn get-rendering-options
  "Returns the rendering options for a mounted component `c` (global defaults merged under the
   component's own `rendering-options`). With keys `ks`, returns the value at that path.

   WARNING: if `c` is a class, global overrides are not honored."
  [c & ks]
  (let [global  (some-> (comp/any->app c)
                  :com.fulcrologic.fulcro.application/runtime-atom
                  deref
                  ::rendering-options)
        options (merge global (comp/component-options c rendering-options))]
    (if (seq ks)
      (get-in options (vec ks))
      options)))

(defn option
  "Returns the rendering `option-key` for component `c`, falling back to this plugin's documented
   default (and finally to `fallback`, when given, for options with no built-in default)."
  ([c option-key] (option c option-key (get defaults option-key)))
  ([c option-key fallback]
   (let [v (get (get-rendering-options c) option-key)]
     (if (nil? v) fallback v))))

(defn set-global-rendering-options!
  "Sets rendering `options` on `app` as defaults. `options` MUST NOT contain the `rendering-options`
   key itself — pass the option keys/values directly (e.g. `{tuo/field-label-width 20}`)."
  [app options]
  (swap! (:com.fulcrologic.fulcro.application/runtime-atom app)
    assoc ::rendering-options options))
