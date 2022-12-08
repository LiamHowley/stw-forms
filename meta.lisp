(in-package stw.form)


(defvar *base-form-class-initargs*
  `(:extends :csrf :novalidate :action :method :enctype :autocomplete :rel :accept-charset :name))

(defvar *form-slot-definition-initargs*
  `(:input-type :name :value :list :step :options :min :max :size :formnovalidate :required :maxlength :minlength :pattern :accept :file :multiple :readonly :disabled :accept :alt :autocomplete :autofocus :capture :checked :dirname :form :formaction :formenctype :formmethod :formnovalidate :formtarget :pattern :placeholder :usemap :width :height :for :label :label-placement))


(defclass form-context-class (template-context-class)
  ())

(deflayer form-layer (template-layer)
  ()
  (:metaclass form-context-class))


(define-layered-class base-form-class
  :in form-layer (base-template-class)
  ((csrf :initarg :csrf :initform nil :accessor csrf)
   (extends :initarg :extends :initform nil :accessor extends)
   (form-attributes
    :initform nil
    :reader form-class-attributes
    :initarg :name
    :initarg :novalidate
    :initarg :action
    :initarg :method
    :initarg :enctype
    :initarg :autocomplete
    :initarg :rel
    :initarg :accept
    :initarg :accept-charset)))


(defmethod partial-class-base-initargs append ((class base-form-class))
  *base-form-class-initargs*)


(defclass form-slot-definition (template-slot-definition)
  ((sanitize :initarg :sanitize :initform +default+)
   (fieldtype :initarg :fieldtype :initform 'input-field :reader slot-definition-fieldtype)
   (fields
    :initform nil
    :reader slot-definition-field-attributes
    :initarg :input-type
    :initarg :name
    :initarg :value
    :initarg :list
    :initarg :step
    :initarg :options
    :initarg :min
    :initarg :max
    :initarg :size
    :initarg :formnovalidate
    :initarg :required
    :initarg :maxlength
    :initarg :minlength
    :initarg :pattern
    :initarg :accept
    :initarg :file
    :initarg :multiple
    :initarg :readonly
    :initarg :disabled
    :initarg :accept
    :initarg :alt
    :initarg :autocomplete
    :initarg :autofocus
    :initarg :capture
    :initarg :checked
    :initarg :dirname
    :initarg :form
    :initarg :formaction
    :initarg :formenctype
    :initarg :formmethod
    :initarg :formnovalidate
    :initarg :formtarget
    :initarg :pattern
    :initarg :placeholder
    :initarg :usemap
    :initarg :width
    :initarg :height)
   (label
    :initarg :for
    :initarg :label
    :initarg :label-placement)))


(defmethod slot-definition-class ((layer-metaclass form-context-class))
  'form-slot-definition)

(define-layered-class form-class
  :in-layer form-layer (base-form-class template-class)
  ())



(defmacro define-form-class (name &body body)
  "Wrapper on DEFINE-BASE-CLASS."
  (unless (serialized-p (car body))
    (push 'serialize (car body)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-base-class ,name
       :in form-layer
       ,@body
       (:metaclass form-class))))


(define-layered-method render-template
  :in form-layer ((form base-form-class) &optional stream args)
  (with-slots (template csrf) form
    (apply #'djula:render-template* template stream :csrf csrf args)))


(defun string->symbol (string)
  (intern (string-upcase string) 'stw.form))

(defun keyword->symbol (key)
  (declare (inline string->symbol))
  (string->symbol (symbol-name key)))

(defun multiple-valuep (slot)
  "Does the slot have a specified type of array, list or cons?"
  (member (slot-definition-type slot) `(array list cons)))


(define-layered-method initialize-in-context
  :in form-layer
  ((slot form-slot-definition) &rest rest &key input-type &allow-other-keys)
  (let ((rest* (loop
		 for (key value) on rest by #'cddr
		 if input-type
		   collect :type and
		 collect input-type
		 else 
		   when (member key *form-slot-definition-initargs* :test #'eq)
		     collect key
		     and collect (ensure-string value))))
    (with-slots (fieldtype fields) slot
      (let* ((fieldclass-sym (typecase fieldtype
			       (keyword (keyword->symbol fieldtype))
			       (symbol fieldtype)
			       (string (string->symbol fieldtype))))
	     (field (make-instance fieldclass-sym
				   ;; for datalist and select
				   :multiple-valuep (multiple-valuep slot) :allow-other-keys t)))
	(setf fields (apply #'initialize-form-fields slot field rest*))))))


(define-layered-function add-label (slot label &rest args &key &allow-other-keys)
  (:method
      :in form-layer
      ((slot form-slot-definition) (class label) &rest args &key label for &allow-other-keys)
    (apply #'reinitialize-instance class
	   :for (if for for (format nil "~(~a-~a~)" (slot-definition-name slot) (slot-definition-fieldtype slot)))
	   :child-nodes (list (make-instance 'text-node :text label))
	   :allow-other-keys t
	   args)))


(define-layered-function initialize-form-fields (slot field &rest args &key &allow-other-keys)

  (:method
      :in form-layer
      :around (slot field &rest args &key &allow-other-keys)
    (declare (ignore args))
    (make-instance 'field-container
		   :class "form-field-container"
		   :id (format nil "~(~a-~a~)" (slot-definition-name slot) (class-name (class-of field)))
		   :child-nodes (call-next-method)))

  (:method
      :in form-layer ((slot form-slot-definition) field &rest args &key label label-placement &allow-other-keys)
    (let ((label (when label
		   (apply #'add-label slot (make-instance 'label) args)))
	  (placement (case label-placement
		       ((:bottom :right)
			:rtl)
		       (t
			:ltr)))
	  (slot-name (slot-definition-name slot)))
      (apply #'reinitialize-instance field
	     :allow-other-keys t
	     :stw-reader nil
	     args)
      (setf (html-class field) (ensure-list (html-class field))
	    (html-parse-name field) (format nil "~(~a~)" slot-name))
      (pushnew "form-field" (html-class field) :test #'string-equal)
      ;; default id
      (unless (html-parse-id field) 
	(setf (html-parse-id field)
	      (if label
		  (html-parse-for label)
		  (format nil "~(~a-~a~)" slot-name (slot-definition-fieldtype slot)))))
      (let ((fields
	      (if label
		  (list label field)
		  (list field))))
	(if (eq placement :ltr)
	    fields
	    (reverse fields)))))

  (:method
      :in form-layer ((slot form-slot-definition) (field input) &rest args &key value &allow-other-keys)
    (declare (ignore args))
    (prog1 (call-next-method)
      (unless (html-parse-value field)
	(setf (html-parse-value field) 
	      (if value value (format nil "{{ ~(~a~) }}" (car (slot-definition-initargs slot))))))))

  (:method
      :in form-layer ((slot form-slot-definition) (field checkbox) &rest args &key &allow-other-keys)
    (declare (ignore args))
    (prog1 (call-next-method)
      (setf (html-parse-value field) (format nil "~(~a~)" (car (slot-definition-initargs slot))))))

  (:method
      :in form-layer ((slot form-slot-definition) (field datalist) &rest args &key &allow-other-keys)
    (let* ((name (format nil "~(~a~)" (slot-definition-name slot)))
	   (input (apply #'call-next-method slot (make-instance 'input :name name) args)))
      (setf (slot-value field 'options) (format nil "~(~a~)" (car (slot-definition-initargs slot))))
      (list input (apply #'reinitialize-instance 'datalist :id (html-parse-input-list input) args))))

  (:method
      :in form-layer ((slot form-slot-definition) (field select) &rest args &key &allow-other-keys)
    (declare (ignore args))
    (prog1 (call-next-method)
      (setf (slot-value field 'options) (format nil "~(~a~)" (car (slot-definition-initargs slot)))
	    (html-parse-name field) (format nil "~(~a~)" (slot-definition-name slot))))))


(define-layered-method initialize-in-context
  :in form-layer ((class base-form-class) &rest rest &key template extends &allow-other-keys)
  (let ((rest* (loop
		 for (key value) on rest by #'cddr
		 when (member key (cddr *base-form-class-initargs*) :test #'eq)
		   collect key
		   and collect (ensure-string value))))
    (awhen (apply #'asdf:system-relative-pathname template)
      (let* ((form (apply #'make-instance 'form :stw-reader nil rest*))
	     (slots (map-filtered-slots class
					#'(lambda (slot)
					    (typep slot 'form-slot-definition))))
	     (child-nodes (loop for slot in slots
				for fields = (slot-value slot 'fields)
				when fields collect fields)))
	;; add csrf token
	(awhen (slot-value class 'csrf)
	  (push (make-instance 'input :type "hidden" :name "csrf-token" :class '("hidden") :value self) child-nodes))

	;; add child-nodes
	(setf (slot-value form 'child-nodes) child-nodes)
	(with-open-file (stream self :direction :output :if-does-not-exist :create :if-exists :supersede)
	  (let ((*indent* 0))
	    (when  extends
	      (format stream "{% extends ~a %}" extends))
	    (serialize-object form stream *indent* t))))
      (let ((template (slot-value class 'template)))
	(when (consp template)
	  (setf (slot-value class 'template) self
		(compiled-template class) template))))))



(defun set-options (indent stream)
  (indent-string indent stream)
  (write-string "{% for option in options %}" stream)
  (indent-string indent stream)
  (write-string "<option value='{{ option.value }}' {{ option.disabled }}{{ option.selected }}>{{ option.output }}</option>" stream)
  (indent-string indent stream)
  (write-string "{% endfor %}" stream))

(defmethod serialize-object :around ((object field-container) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (let ((select-p (some #'(lambda (child)
			     (typep child 'select))
			(slot-value object 'child-nodes))))
    (cond (select-p
	   (indent-string indent stream)
	   (write-string "{% if options %}" stream)
	   (call-next-method)
	   (indent-string indent stream)
	   (write-string "{% endif %}" stream))
	  (t
	   (call-next-method)))))

(defmethod serialize-object ((object select) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (write-char #\> stream)
  (set-options (+ 3 indent) stream))

(defmethod serialize-object ((object datalist) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (write-char #\> stream)
  (set-options (+ 3 indent) stream))