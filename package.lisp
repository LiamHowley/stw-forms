(defpackage stw.form
  (:use :cl :template)
  (:nicknames :form)
  (:import-from
   :stw.util
   :map-tree-depth-first
   :awhen
   :aif
   :self
   :ensure-list
   :ensure-string
   :get-float
   :explode-string
   :keyword->symbol)
  (:import-from
   :closer-mop
   :slot-definition-name
   :slot-definition-type
   :slot-definition-initargs)
  (:import-from
   :contextl
   :define-layered-function
   :define-layered-method
   :define-layered-class
   :deflayer
   :with-active-layers
   :call-next-layered-method
   :partial-class-base-initargs
   :singleton-class)
  (:import-from
   :cl-comp
   :slot-definition-class
   :serialized-p
   :serialize
   :define-base-class
   :map-filtered-slots
   :filter-slots-by-type
   :find-slot-definition
   :initialize-in-context
   :object-to-plist)
  (:import-from
   :xml.parse
   :branch-node
   :child-nodes
   :the-content
   :*indent*
   :indent-string
   :serialize-object
   :query-select
   :query-select-all)
  (:import-from
   :html.parse
   :serialize-object
   :define-html-node
   :html-parse-id
   :html-class
   :html-parse-for
   :html-parse-value
   :html-parse-input-list
   :html-parse-input-min
   :html-parse-input-max
   :html-parse-name
   :html-parse-multiple
   :html-parse-disabled
   :html-parse-required
   :html-parse-pattern
   :html-parse-value
   :html-parse-minlength
   :html-parse-maxlength
   :html-parse-checked
   :input-type
   :div
   :h5
   :form
   :fieldset
   :legend
   :label
   :text-node)
  (:import-from
   :sanitize
   :sanitize
   :text-only)
  (:import-from
   :local-time
   :timestamp<=
   :parse-timestring
   :format-timestring
   :+asctime-format+)
  (:export
   :base-form-class
   :form-class
   :form-slot-definition

   :form-layer
   :define-form
   :render-template
   :novalidate
   :validate-form
   :validate-fields
   :validate-field
   :assign-user-input

   ;; conditions
   :validate-form-error
   :validate-field-error
   :rendering-error
   :signal-convert
   :store-slot
   :field-errors
   :field-values

   ;; fieldtypes/ancilliary definitions
   :text
   :password
   :email
   :url
   :tel
   :submit
   :button
   :number-field
   :color
   :date
   :datetime-local
   :time-field
   :checkbox
   :radio
   :file
   :select
   :datalist
   :textarea
   :output
   :option

   :grouped-list
   :grouped-table
   :grouped-row
   :grouped-row-heading
   :form-row
   :row-headings
   :grouped-field

   ;;csrf
   :csrf-p
   :generate-csrf-token

   ;; methods
   :default-arguments
   :retrieve-options))
