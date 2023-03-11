(in-package stw.form)


(defvar *base-form-class-initargs*
  `(:extends :csrf-p :novalidate :action :method :enctype :autocomplete :rel :accept-charset :name))

(defvar *form-slot-definition-initargs*
  `(:input-type :input-name :value :list :step :options :min :max :size :formnovalidate :required :maxlength :minlength :pattern :accept :file :multiple :readonly :disabled :accept :alt :autocomplete :autofocus :capture :checked :dirname :form :formaction :formenctype :formmethod :formnovalidate :formtarget :pattern :placeholder :usemap :width :height :for :label :label-placement :use-numbers :special-chars :capitalize))


(defclass form-context-class (template-context-class)
  ())

(deflayer form-layer (template-layer)
  ()
  (:metaclass form-context-class))


(define-layered-class base-form-class
  :in form-layer (base-template-class)
  ((csrf-p :initarg :csrf-p :initform t :reader csrf-p)
   (extends :initarg :extends :initform nil :accessor extends)
   (novalidate :initarg :novalidate :initform nil :reader novalidate)
   (form-attributes
    :initform nil
    :reader form-class-attributes
    :initarg :name
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
  ((sanitize :initarg :sanitize :initform (find-class 'text-only))
   (fieldtype :initarg :fieldtype :type symbol :initform 'text :reader slot-definition-fieldtype)
   (fields
    :initform nil
    :reader slot-definition-field-attributes
    :initarg :input-type
    :initarg :input-name
    :initarg :value
    :initarg :list
    :initarg :step
    :initarg :options
    :initarg :min
    :initarg :max
    :initarg :size
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
   (rules
    :documentation "Helper for validation of new passwords."
    :initarg :special-chars
    :initarg :use-numbers
    :initarg :capitalize)
   (label
    :initarg :for
    :initarg :label
    :initarg :label-placement
    :initform nil)))

(defmethod slot-definition-class ((layer-metaclass form-context-class))
  'form-slot-definition)

(defmethod slot-unbound ((class base-form-class) instance slot-name)
  (let ((slot (find-slot-definition (class-of instance) slot-name 'form-slot-definition)))
    (with-slots (fields fieldtype) slot
      (let ((field
	      (query-select fields
			    #'(lambda (node)
				(typep node fieldtype)))))
	(when (and (slot-exists-p field 'required)
		   (slot-value field 'required))
	  (validate-field-error "~a is a required field" slot-name))
	nil))))


(define-layered-class form-class
  :in-layer form-layer (base-form-class template-class)
  ())


(defclass error-fields ()
  ((error-fields :initform nil
		 :initarg :error-fields
		 :documentation "Errors are bound to the form-instance as form-fields and are typically cached.")))


(defmacro define-form (name &body body)
  "Wrapper on DEFINE-BASE-CLASS."
  (unless (serialized-p (car body))
    (push 'serialize (car body)))
  (push 'error-fields (car body))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-base-class ,name
       :in form-layer
       ,@body
       (:metaclass form-class))))


(defun multiple-valuep (slot)
  "Does the slot have a specified type of array, list or cons?"
  (member (slot-definition-type slot) `(array list cons)))


(define-layered-method initialize-in-context
  :in form-layer
  ((slot form-slot-definition) &rest rest &key &allow-other-keys)
  (let ((rest* (loop
		 for (key value) on rest by #'cddr
		 when (member key *form-slot-definition-initargs* :test #'eq)
		   collect key
		   and collect value)))
    (with-slots (fieldtype fields) slot
      (let* ((fieldclass-sym (typecase fieldtype
			       (keyword (keyword->symbol fieldtype))
			       (symbol fieldtype)
			       (string (string->symbol fieldtype))))
	     (field (make-instance fieldclass-sym
				   ;; for select
				   :multiple-valuep (multiple-valuep slot)
				   :parent-field slot
				   :allow-other-keys t)))
	(setf fields (typecase field
		       ((or grouped-list grouped-table)
			(field-container slot field (apply #'initialize-grouped-fields slot field rest*)))
		       (t
			(apply #'set-field slot field :class "form-field" rest*)
			(apply #'field-container slot field (apply #'initialize-form-field slot field rest*)))))))))


(defmethod add-label ((slot form-slot-definition) field &rest args &key label for label-placement &allow-other-keys)
  (if label
      (merge-label-and-field
       (let ((slot-name (slot-definition-name slot))
	     (fieldtype (slot-definition-fieldtype slot)))
	 (apply #'make-instance 'label
		:for (if for for (format nil "~(~a-~a~)" slot-name fieldtype))
		:child-nodes (list (make-instance 'text-node :text label))
		:allow-other-keys t
		args))
       (case label-placement
	 ((:bottom :right)
	  :rtl)
	 (t
	  :ltr))
       field)
      (list field)))


(defmethod field-container ((slot form-slot-definition) field &rest fields)
  (let ((slot-name (slot-definition-name slot))
	(errors 
	  (unless (eq (slot-value slot 'fieldtype) 'submit)
	    (make-instance 'error-message
			   :class '("error-message")
			   :parent-field slot))))
    (make-instance 'field-container
		   :class "form-field-container"
		   :parent-node slot
		   :id (format nil "~(~a-~a~)-container" slot-name (class-name (class-of field)))
		   :child-nodes (append fields (ensure-list errors)))))


(defmethod merge-label-and-field (label placement field)
  (setf (html-parse-id field) (html-parse-for label))
  (let ((fields
	  (list label (make-instance 'div
				     :class "labelled-field"
				     :child-nodes (list field)))))
    (if (eq placement :ltr)
	fields
	(reverse fields))))


(defmethod set-field ((slot form-slot-definition) field &rest args &key class id &allow-other-keys)
  "Ensure basic defaults are set."
  (let ((slot-name (slot-definition-name slot)))
    (apply #'reinitialize-instance field
	   :parent-field slot
	   :class (let ((field-class (ensure-list (html-class field))))
		    (pushnew class field-class :test #'string-equal))
	   :name (format nil "~(~a~)" slot-name)
	   :id (if id id nil)
	   :allow-other-keys t
	   args)))


(define-layered-function initialize-grouped-fields (slot field &rest args &key &allow-other-keys)
  (:method
      :in form-layer ((slot form-slot-definition) (field grouped-list) &rest args &key input-name input-type &allow-other-keys)
    (setf (getf args :label) "{{ field.label }}"
	  (getf args :for) "{{ field.id }}"
	  (getf args :value) "{{ field.value }}")
    (let* ((slot-name (format nil "~(~a~)" (slot-definition-name slot)))
	   (input-class (aif input-type
			     (find-class (intern (string-upcase self)))
			     (find-class 'text)))
	   (input (apply #'initialize-form-field slot
			 (make-instance input-class
					:name (or input-name (format nil "~(~a[{{ field.name }}]~)" slot-name))
					:parent-field slot)
			 args)))
      (setf (slot-value field 'child-nodes) input)
      (apply #'add-label slot field args)
      (apply #'set-field slot field :class "grouped-fields-list" args)
      field))

  (:method
      :in form-layer ((slot form-slot-definition) (field grouped-row-heading) &rest args &key &allow-other-keys)
    (setf (getf args :class) "row-heading")
    (let ((heading-container (make-instance 'div :class '("column-heading")))
	  (style (make-instance 'h5 :class "heading"))
	  (heading (make-instance 'text-node :text "{{ heading.row }}")))
      (setf (slot-value field 'child-nodes) (list heading-container)
	    (slot-value heading-container 'child-nodes) (list style)
	    (slot-value style 'child-nodes) (list heading)))
    (apply #'set-field slot field :class "grouped-fields-row" args)
    field)

  (:method
      :in form-layer ((slot form-slot-definition) (field grouped-row) &rest args &key input-name input-type &allow-other-keys)
    (setf (getf args :for) "{{ field.id }}"
	  (getf args :value) "{{ field.value }}")
    (let* ((slot-name (format nil "~(~a~)" (slot-definition-name slot)))
	   (input-class (aif input-type
			     (find-class (intern (string-upcase self)))
			     (find-class 'text)))
	   (name (case (class-name input-class)
		   (radio (or input-name (format nil "~(~a[{{ row.name }}]~)" slot-name)))
		   (checkbox (or input-name (format nil "~(~a[{{ row.name }}][]~)" slot-name)))))
	   (input (apply #'initialize-form-field slot
			 (make-instance input-class
					:name name
					:parent-field slot)
			 args)))
      (setf (slot-value field 'child-nodes) input)
      (apply #'set-field slot field :class "grouped-fields-row" args)
      field))

  (:method
      :in form-layer ((slot form-slot-definition) (field grouped-table) &rest args &key &allow-other-keys)
    (let ((grouped-row-heading (make-instance 'grouped-row-heading :parent-field slot))
	  (grouped-row (make-instance 'grouped-row)))
      (setf (slot-value field 'child-nodes)
	    (list (apply #'initialize-grouped-fields slot grouped-row-heading :name "heading" args)
		  (apply #'initialize-grouped-fields slot grouped-row :name "row" args)))
;;      (apply #'add-label slot field args)
      (apply #'set-field slot field :class "group-fields-table" args))))



(define-layered-function initialize-form-field (slot field &rest args &key &allow-other-keys)

  (:method
      :in form-layer ((slot form-slot-definition) field &rest args &key &allow-other-keys)
    (apply #'add-label slot field args))

  (:method
      :in form-layer ((slot form-slot-definition) (field input) &rest args &key value &allow-other-keys)
    (declare (ignore args))
    (prog1 (call-next-method)
      (unless (html-parse-value field)
	(setf (html-parse-value field) 
	      (if value value (format nil "{{ ~(~a~) }}" (car (slot-definition-initargs slot))))))))

  (:method
      :in form-layer ((slot form-slot-definition) (field checkbox) &rest args &key value &allow-other-keys)
    (declare (ignore args))
    (prog1 (list (make-instance 'div :class '("checkbox-wrap") :child-nodes (call-next-method)))
      (unless value
	(setf (html-parse-value field) (html-parse-name field)))))

  (:method 
      :in form-layer ((slot form-slot-definition) (field radio) &rest args &key &allow-other-keys)
    (declare (ignore args))
    (let ((wrapped-field (call-next-method)))
      (setf (html-class (car wrapped-field)) '("radio-field-wrap"))
      wrapped-field))

  (:method
      :in form-layer ((slot form-slot-definition) (field textarea) &rest args &key &allow-other-keys)
    (declare (ignore args))
    (prog1 (call-next-method)
      (setf (slot-value field 'the-content) (format nil "{{ ~(~a~) }}" (car (slot-definition-initargs slot))))))

  (:method
      :in form-layer ((slot form-slot-definition) (field datalist) &rest args &key list &allow-other-keys)
    (let* ((input-class (aif (getf args :input-type)
			     (find-class (intern (string-upcase self)))
			     (find-class 'text)))
	   (name (format nil "~(~a~)" (slot-definition-name slot)))
	   (list (if list list (format nil "~a-list" name)))
	   (input (apply #'call-next-layered-method slot (make-instance input-class :name name :parent-field slot :list list)
			 args)))
      `(,@input ,(apply #'reinitialize-instance field 
			:allow-other-keys t
			:parent-field slot
			:id (html-parse-input-list
			     (query-select input
					   #'(lambda (node)
					       (typep node 'input))))
			args))))

  (:method
      :in form-layer ((slot form-slot-definition) (field select) &rest args &key &allow-other-keys)
    (declare (ignore args))
    (prog1 (call-next-method)
      (let ((slot-name (slot-definition-name slot)))
	(setf (html-parse-name field) (format nil "~(~a~)" slot-name)
	      (html-parse-id field) (format nil "~(~a-select~)" slot-name))))))





(define-layered-function retrieve-options (class slot-name &key value)
  (:documentation "Retrieve the options for datalist and select fields.")
  (:method
      :in form-layer (class slot-name &key value)
    (declare (ignore class slot-name value))
    nil))


(define-layered-method initialize-in-context
  :in form-layer ((class base-form-class) &rest rest &key name template extends method &allow-other-keys)
  (flet ((non-compliant-method-p ()
	   (or (string-equal method% "put")
	       (string-equal method% "delete"))))
    (let ((method* (when (non-compliant-method-p)
		     method))
	  (rest* (loop
		   for (key value) on rest by #'cddr
		   when (member key (cddr *base-form-class-initargs*) :test #'eq)
		     collect key
		     and collect (ensure-string value))))
      (when (non-compliant-method-p)
	(setf method "post"))
      (awhen (apply #'asdf:system-relative-pathname template)
	(let* ((form (apply #'make-instance 'form
			    :name (format nil "~(~a~)" (if name name (class-name class)))
			    :method method
			    rest*))
	       (slots (map-filtered-slots class
					  #'(lambda (slot)
					      (typep slot 'form-slot-definition))))
	       (child-nodes (loop for slot in slots
				  for fields = (slot-value slot 'fields)
				  when fields collect fields)))

	  ;; add hidden http verb as workaround
	  ;; for html form method restrictions.
	  (when method*
	    (push (make-instance 'input :type "hidden" :name "method" :class '("hidden") :value method*) child-nodes))

	  ;; add csrf token
	  (when (slot-value class 'csrf-p)
	    (push (make-instance 'input :type "hidden" :name "csrf-token" :class '("hidden") :value "{{ csrf-token }}") child-nodes))

	  ;; add child-nodes
	  (setf (slot-value form 'child-nodes) child-nodes)
	  (with-open-file (stream self :direction :output :if-does-not-exist :create :if-exists :supersede)
	    (let ((*indent* 0))
	      (when extends
		(format stream "{% extends ~a %}" extends))
	      (serialize-object form stream *indent* t))))
	(let ((template (slot-value class 'template)))
	  (when (consp template)
	    (setf (slot-value class 'template) self)
	    (compile-template class)))))))
