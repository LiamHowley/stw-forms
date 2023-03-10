(in-package stw.form)

;;; mixins

(defclass field! ()
  ((parent-field :initarg :parent-field)))

(defclass error! ()
  ((error-msg :initarg :error-msg
	      :initform nil)))

(defclass sanitize! ()
  ((sanitize :initarg :sanitize
	     :initform (find-class 'text-only))))

(defclass text! () ())

;;; fields types 

(defmacro define-field (name &body body)
  (let ((supers (car body))
	(rest (cdr body)))
    `(progn
       (defclass ,name ,supers ())
       (defmethod initialize-instance :after ((field ,name) &key)
	 (with-slots (input-type) field
	   (setf input-type (format nil "~(~a~)" (class-name (class-of field))))
	   ,@rest)))))

(defclass input (html.parse:input sanitize! error! field!) ())

(define-field text (input text!) ())

(defclass password (text)
  ((special-chars :initarg :special-chars :initform nil)
   (use-numbers :initarg :use-numbers :initform nil)
   (capitalize :initarg :capitalize :initform nil)))

(define-field email (text) ())

(define-field url (text) ())

(define-field tel (text) ())

(define-field submit (input) ())

(define-field button (submit) ())

(define-field number-field (input)
  (setf input-type "number"))

(define-field color (input) ())

(define-field date (input) ())

(define-field datetime-local (date) ())

(define-field time-field (datetime-local)
  (setf input-type "time"))

(define-field checkbox (input) ())

(define-field radio (checkbox) ())

(define-field file (input) ())


(defclass dropdown (sanitize! error! field!)
  ())

(defclass select (html.parse:select dropdown field!)
  ())

(defclass datalist (html.parse:datalist dropdown field!)
  ())

(defclass option (html.parse:option)
  ((output :initarg :output :initform nil)))

(defclass textarea (html.parse:textarea text! sanitize! error! field!) ())

(defclass output (html.parse:output sanitize! error!) ())


(defmethod xml.parse:class->element ((class input))
  "input")

(defmethod xml.parse:class->element ((class text))
  "input")

(defmethod xml.parse:class->element ((class select))
  "select")

(defmethod xml.parse:class->element ((class datalist))
  "datalist")

(defmethod xml.parse:class->element ((class textarea))
  "textarea")

(defmethod initialize-instance :after ((field input) &key)
  (with-slots (html-class input-type) field
    (unless input-type
      (setf input-type "text"))
    (setf html-class (append '("form-field" "input-field") html-class))))

(defmethod initialize-instance :after ((field password) &key)
  (setf (slot-value field 'input-type) "password"))

(defmethod initialize-instance :after ((field select) &key multiple-valuep)
  (when multiple-valuep
    (setf (html-parse-multiple field) t)))


(defclass field-container (div) ()
  (:documentation "Internal class to target the field container without
clobbering divs throughout a template."))

(defmethod xml.parse:class->element ((class field-container))
  "div")


(defclass grouped-list (div field!)
  ((parent-field :initarg :parent-field :initform nil)
   (name :initarg :name :initform nil))
  (:documentation "Wrapper class around a list of fields."))

(defmethod xml.parse:class->element ((class grouped-list))
  "div")

(defmethod shared-initialize :after ((class grouped-list) slot-names &key)
  (with-slots (parent-field name) class
    (unless name
      (when parent-field
	(setf name (car (slot-definition-initargs parent-field)))))))


(defclass grouped-row-heading (div)
  ((parent-field :initarg :parent-field :initform nil)
   (name :initarg :name :initform nil))
  (:documentation "Wrapper class around a row of fields in a table."))

(defmethod xml.parse:class->element ((class grouped-row-heading))
  "div")

(defmethod shared-initialize :after ((class grouped-list) slot-names &key)
  (with-slots (parent-field name) class
    (unless name
      (when parent-field
	(setf name (car (slot-definition-initargs parent-field)))))))


(defclass grouped-row (grouped-list)
  ()
  (:documentation "Wrapper class around a table of fields."))

(defmethod xml.parse:class->element ((class grouped-row))
  "div")


(defclass grouped-table (grouped-list)
  ((headings :initarg :headings)
   (rows :initarg :rows))
  (:documentation "Wrapper class around a table of fields."))

(defmethod xml.parse:class->element ((class grouped-table))
  "div")


(defclass error-message (div)
  ((parent-field :initarg :parent-field :reader parent-field))
  (:documentation "Internal class to target any error messages without
clobbering divs throughout a template."))

(defmethod xml.parse:class->element ((class error-message))
  "div")

(defclass row-headings ()
  ((row :initarg :row :initform nil)))

(defclass form-row ()
  ((heading :initarg :heading :initform nil)
   (name :initarg :name :initform nil)
   (fields :initarg :fields :initform nil))
  (:documentation "Contains the slots heading, and fields; the former
to act as label to a row of fields, and the latter a list of fields of 
type GROUPED-FIELD. Typically used in conjunction with the generic function
RETRIEVE-OPTIONS."))

(defclass grouped-field (input)
  ((label :initarg :label :initform nil))
  (:documentation "Class where slots correspond to field variables to 
aid in rendering a list/row of fields. Typically used in conjunction 
with the generic function RETRIEVE-OPTIONS."))
