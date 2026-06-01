(ns com.fulcrologic.rad.rendering.tui.form-spec
  "Standalone (no-DB) rendering tests for the TUI RAD form *structure*: the form container/title/action
   buttons, the field body, the to-many subform (with Add/Delete), and report-row → edit-form
   navigation through a `ro/form-links` column."
  (:require
    [clojure.string :as str]
    [com.fulcrologic.rad.rendering.tui.test-support :as ts]
    [fulcro-spec.core :refer [=> assertions specification]]))

(defn- count-matches [re s] (count (re-seq re s)))

(specification "form structure"
  (ts/quiet
    (let [app    (ts/form-app!)
          screen (ts/screen-text app)
          has?   (fn [re] (boolean (re-find re screen)))]
      (assertions
        "renders the form container title"
        (has? #"Edit Thing") => true
        "renders the pinned Save / Undo / Cancel action buttons"
        (and (has? #"Save") (has? #"Undo") (has? #"Cancel")) => true
        "renders the scalar field body (a representative field)"
        (has? #"Name\*") => true
        "renders the declared to-many subform block with its child field"
        (and (has? #"tags") (has? #"Label\*")) => true
        "renders subform Add and Delete controls"
        (and (has? #"\+ Add") (has? #"- Delete")) => true))))

(specification "subform interaction — Add appends a child row"
  (ts/quiet
    (let [app (ts/form-app!)]
      (assertions
        "starts with one subform child (one Label* field)"
        (count-matches #"Label\*" (ts/screen-text app)) => 1)
      (ts/tab-to-id! app :add/thing_tags)
      (ts/enter! app)
      (assertions
        "activating + Add renders a second subform child row"
        (count-matches #"Label\*" (ts/settle! app)) => 2))))

(specification "report row → edit form navigation (ro/form-links)"
  (ts/quiet
    (let [app (ts/report-app! ::ts/ThingReport)]
      ;; The first focusable report row is reached by tabbing to the `row` focus group.
      (ts/tab-to-ns! app "row")
      (ts/enter! app)
      (let [screen (ts/wait-for app #"Edit Thing")]
        (assertions
          "activating a row whose column is a form-link opens that row's edit form"
          (boolean (re-find #"Edit Thing" screen)) => true
          "the edit form loads the row's data (the picked row's name shows)"
          (boolean (re-find #"Anvil|Bellows|Crucible" screen)) => true)))))
