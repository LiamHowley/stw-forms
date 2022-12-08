(in-package stw.form)

(defclass error! ()
  ((error-msg :initarg :error-msg
	      :initform nil)))

(defclass sanitize! ()
  ((sanitize :initarg :sanitize
	     :initform +default+)))

(defclass input (html.parse:input sanitize! error!)
  ())

(defclass password (input) ())

(defclass submit (input) ())

(defclass number-field (input) ())

(defclass checkbox (input) ())

(defclass radio (checkbox) ())

(defclass file (input) ())

(defclass dropdown (sanitize! error!)
  ((options
    :initarg :options
    :accessor dropdown-options
    :documentation "Options is for use in datalist and select fields, 
to allow for a rendering key that matches the initarg of a slot of type 
FORM-SLOT-DEFINITION")))

(defclass select (html.parse:select dropdown)
  ())

(defclass datalist (html.parse:select dropdown)
  ())

(defclass textarea (html.parse:textarea sanitize! error!) ())

(defclass output (html.parse:output sanitize! error!) ())


(defmethod xml.parse:class->element ((class input))
  "input")

(defmethod xml.parse:class->element ((class select))
  "select")

(defmethod xml.parse:class->element ((class datalist))
  "datalist")

(defmethod initialize-instance :after ((field input) &key)
  (with-slots (html-class input-type) field
    (unless input-type
      (setf input-type "text"))
    (setf html-class (append '("form-field" "input-field") html-class))))

(defmethod initialize-instance :after ((field submit) &key)
  (with-slots (id input-type) field
    (setf input-type "submit")))

(defmethod initialize-instance :after ((field checkbox) &key)
  (with-slots (input-type) field
    (setf input-type "checkbox")))

(defmethod initialize-instance :after ((field radio) &key)
  (with-slots (input-type) field
    (setf input-type "radio")))

(defmethod initialize-instance :after ((field file) &key)
  (with-slots (input-type) field
    (setf input-type "file")))

(defmethod initialize-instance :after ((field select) &key multiple-valuep)
  (when multiple-valuep
    (setf (html-parse-multiple field) t)))

(defmethod initialize-instance :after ((field datalist) &key multiple-valuep)
  (when multiple-valuep
    (setf (html-parse-multiple field) t)))

(defclass field-container (div) ()
  (:documentation "Internal class to target the field container without
clobbering divs throughout a template."))

(defmethod xml.parse:class->element ((class field-container))
  "div")
