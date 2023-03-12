(in-package stw.form)


(define-layered-function assign-user-input (form values &rest rest &key)
  (:documentation "Accepts a BASE-FORM-CLASS and an alist of (<fieldname> . <value>). Calls
VALIDATE-FORM with supplied arguments and catches any returning errors. Returns the values of
an instance of BASE-FORM-CLASS with assigned values, and a boolean to indicate the presence
of errors in the slot ERROR-FIELDS. Errors assigned to the slot ERROR-FIELDS are formatted
as a plist to be accepted by RENDER-TEMPLATE, so that a redirect with relevant values and 
errors can be automated.")
  (:method
      :in form-layer 
      ((form base-form-class) values &rest rest &key include-fields exclude-fields csrf-token-server csrf-token-client)
    (declare (ignore include-fields exclude-fields csrf-token-server csrf-token-client))
    (handler-case (values
		   (awhen (apply #'validate-form form values rest)
		     (apply #'make-instance form self))
		   nil)
      (validate-form-error (c)
	(values
	 (apply #'make-instance form 
		:error-fields (field-errors c)
		(field-values c))
	 t)))))


(define-layered-function retrieve-fields (class &key)
  (:documentation "RETRIEVE-FIELDS returns an alist of (slotname . <field objects>).")

  (:method
      :in-layer form-layer
      ((class base-form-class) &key include-fields exclude-fields)
    (mapcan #'(lambda (slot)
		(let ((fieldname (slot-definition-name slot)))
		  (unless (or (and include-fields
				   (not (member fieldname include-fields)))
			      (and exclude-fields
				   (member fieldname exclude-fields)))
		    (list (cons fieldname
				(query-select-all
				 (slot-value slot 'fields)
				 #'(lambda (node)
				     (typep node 'field!))))))))
	    (filter-slots-by-type class 'form-slot-definition))))


(define-layered-function validate-form (class values &key)
  (:documentation "VALIDATE-FORM is primarily concerned with verifying the requirement and equality of
any CSRF token. It subsequently calls VALIDATE-FIELDS. As truncated forms may have been rendered
the params INCLUDE-FIELDS and EXCLUDE-FIELDS are available to pass on to VALIDATE-FIELDS. Forms with
errors will return a VALIDATE-FORM-ERROR condition."))

(define-layered-function validate-fields (class values &key)
  (:documentation "VALIDATE-FIELDS processes each field in a loop after first testing for presence in 
either INCLUDE-FIELDS or EXCLUDE-FIELDS list or if a field is defined as disabled. 

Each call to VALIDATE-FIELD will result in an error or signal condition. Error's are assigned to the relevant 
field's ERROR-MSG attribute, so that it can be returned along with the rendered field. If a field validates, 
any bound error messages are cleared."))

(defgeneric validate-field (class value &key)
  (:documentation "VALIDATE-FIELD provides a condition STORE-SLOT for use when a fields state and value are 
required by a subsequent field. An example would be for password renewal forms, where passwords provided must match.

The condition STORE-SLOT when invoked is bound in VALIDATE-FIELDS as a list, and made available as the arg
STORED-FIELDS for subsequent calls to VALIDATE-FIELD"))



(define-layered-method validate-form
  :in-layer form-layer ((class base-form-class) values &key csrf-token-server csrf-token-client include-fields exclude-fields)
  (when (slot-value class 'csrf-p)
    (unless (and csrf-token-server csrf-token-client
		 (equalp csrf-token-server csrf-token-client))
      (validate-form-error nil nil "The form ~a does not contain a valid csrf token."
			   (class-name class))))
  (validate-fields class
		   values
		   :include-fields include-fields
		   :exclude-fields exclude-fields))


(define-layered-method validate-fields
  :in-layer form-layer ((class base-form-class) values &key include-fields exclude-fields &aux stored-fields)
  (declare (special stored-fields))
  (let ((fields (retrieve-fields class
				 :include-fields include-fields 
				 :exclude-fields exclude-fields))
	(novalidate (slot-value class 'novalidate))
	(error-fields)
	(field-values))
    (loop
      for (fieldname . fields) in fields
      for field = (car fields)
      for value = (unless (typep field 'checkbox)
		    (awhen (assoc fieldname values :test #'string-equal)
		      (if (dotted-p self) (cdr self) (cadr self))))
      do (with-slots (parent-field) field
	   (let ((initarg (car (slot-definition-initargs parent-field))))
	     (setf (getf field-values initarg) value)
	     (handler-bind ((validate-field-error
			      #'(lambda (c)
				  (let* ((name (format nil "~a-ERRORS" initarg))
					 (key (intern name 'keyword)))
				    (if (getf error-fields key)
					(pushnew (the-message c) (getf error-fields key) :test #'string=)
					(setf (getf error-fields key) (list (the-message c)))))
				  (continue)))
			    (signal-convert
			      #'(lambda (c)
				  (setf (cdr (assoc fieldname values :test #'string-equal)) (value c))))
			    (store-slot
			      #'(lambda (c)
				  (push (cons (fieldname c) (stored-value c)) stored-fields))))
	       (unless (or novalidate (typep field 'submit))
		 (typecase field
		   ((or select grouped-list grouped-table)
		    (validate-field field value :options (retrieve-options class (slot-definition-name parent-field))))
		   (t
		    (validate-field field value))))))))
    (if error-fields
	(validate-form-error error-fields field-values
			     "The form ~a contains errors."
			     (with-active-layers (form-layer)
			       (class-name class)))
	field-values)))

(defun string-is-numberp (string)
  (loop for char across string
	always (or (digit-char-p char)
		   (char= char #\.))))


(defmethod validate-field ((field number-field) value &key)
  (when (html-parse-disabled field)
    (validate-form-error error-fields nil "Cannot validate form. Disabled fields returned"))
  (with-accessors ((required html-parse-required)
		   (input-name html-parse-name)
		   (min-d html-parse-minlength)
		   (max-d html-parse-maxlength))
      field
    (requiredp field required value)
    (when value
      (let ((length (length value)))
	(setf value (loop 
		      with func = #'parse-integer
		      for i from 0 below length
		      for char = (aref string i)
		      for numberic = (or (digit-char-p char)
					 (char= char #\.))
		      while result
		      when (char= char #\.)
			do (setf type #'get-float)
		      finally (if (= (1- length) i)
				  (return (funcall func value))
				  (validate-field-error "~a is not a number" value)))))
      (cond ((and max-d (> value max-d))
	     (validate-field-error "The number ~a must be less than ~a" value max-d))
	    ((and min-d (< value min-d))
	     (validate-field-error "The number ~a must be greater than ~a" value min-d)))
      (signal-convert value))))



(defmethod validate-field ((field text!) value &key)
  (when (html-parse-disabled field)
    (validate-form-error error-fields nil "Cannot validate form. Disabled fields returned"))
  (with-slots (sanitize parent-field) field
    (with-accessors ((name html-parse-input-name)
		     (required html-parse-required)
		     (minlength html-parse-minlength)
		     (maxlength html-parse-maxlength))
	field
      (requiredp field required value)
      (when value
	(let ((fieldname (slot-definition-name parent-field)))
	  (unless (stringp value)
	    (error "The field: ~S is a text field; ~S is not valid text" fieldname value))
	  (cond ((and maxlength (> (length value) maxlength))
		 (validate-field-error "~a must be less than ~a characters long" fieldname maxlength))
		((and minlength (< (length value) minlength))
		 (validate-field-error "~a must be at least ~a characters long" fieldname minlength))))))))



;; Pattern: Browsers consider an empty string as valid and 
;; use the REQUIRED attribute to invalidate empty strings. Additionally
;; the whole string must match with or without ^ $. Validate field adjusts
;; any pattern to accommodate both quirks.

(defmethod validate-field ((field text) value &key)
  (with-slots (parent-field sanitize) field
    (call-next-method)
    (when value
      (let ((pattern (html-parse-pattern field)))
	(when pattern
	  (unless (or (string= "" value)
		      (cl-ppcre:scan (concatenate 'string "^" (string-trim '(#\$ #\^) pattern) "$") value))
	    (validate-field-error "The ~s value ~s does not match the expected pattern" (slot-definition-name parent-field) value))))
      (signal-convert (sanitize value sanitize)))))


(defmethod validate-field ((field email) value &key)
  (flet ((call-error ()
	   (validate-field-error "The value ~s is not a valid email address" value)))
    (let ((values (ensure-list value)))
      (if values
	  (loop
	    for email in values
	    do (call-next-method field email)
	    do (when value
		 (let ((has-@)
		       (valid-chars '(#\! #\? #\# #\$ #\% #\’ #\` #\+ #\- #\* #\/ #\= #\~ #\^ #\_ #\{ #\} #\| #\.)))
		   (loop
		     for char across value
		     for valid-char = (or (alphanumericp char)
					  (member char valid-chars :test #'char=)
					  (char= char #\@)
					  (char= char #\.))
		     unless valid-char
		       do (call-error)
		       and do (return)
		     do (cond ((and (char= char #\@) has-@)
			       (call-error))
			      ((char= char #\@)
			       (setf has-@ t)))
		     finally (unless has-@
			       (call-error))))))
	  (call-next-method)))))


(defmethod validate-field ((field color) value &key)
  (when (html-parse-disabled field)
    (validate-form-error error-fields nil "Cannot validate form. Disabled fields returned"))
  (with-slots (parent-field) field
    (flet ((call-error ()
	     (validate-field-error "The ~s value ~s is not a valid colour." (slot-definition-name parent-field) value)))
      (let ((length (length value)))
	(unless (or (<= length 7)
		    (char= (aref value 0) #\x))
	  (call-error))
	(loop
	  for i from 1 below (length value)
	  for char = (aref value i)
	  unless (or (digit-char-p char)
		     (member char '(#\a #\b #\c #\d #\e #\f :test #'char=)))
	    do (call-error))))))
	

(defmethod validate-field ((field date) value &key)
  (when (html-parse-disabled field)
    (validate-form-error error-fields nil "Cannot validate form. Disabled fields returned"))
  (unless (parse-timestring value :fail-on-error nil) 
    (validate-field-error "The value ~s is not a valid timestring." value))
  (with-accessors ((min-time html-parse-input-min)
		   (max-time html-parse-input-max))
      field
    (let ((min-time (if (typep min-time 'local-time:timestamp)
			min-time
			(parse-timestring min-time)))
	  (max-time (if (typep max-time 'local-time:timestamp)
			max-time
			(parse-timestring max-time))))
      (unless (timestamp< min-time value max-time)
	(validate-field-error "The value ~s is not within a valid range. Please choose a date/time between ~s and ~s."
			      value
			      (format-timestring nil min-time :format +asctime-format+)
			      (format-timestring nil max-time :format +asctime-format+))))))


(defmethod validate-field ((field datetime-local) value &key)
  (call-next-layered-method field (concatenate 'string value "00")))


(defmethod validate-field ((field checkbox) value &key)
  (when (html-parse-disabled field)
    (validate-form-error error-fields nil "Cannot validate form. Disabled fields returned"))
  (awhen (html-parse-checked field)
    (unless (string-equal self value)
      (validate-field-error "The value ~s and the returned value ~s do not match." value self))))


(defmethod validate-field ((field select) value &key options)
  (when (html-parse-disabled field)
    (validate-form-error error-fields nil "Cannot validate form. Disabled fields returned"))
  (with-accessors ((input-name html-parse-input-name)
		   (required html-parse-required)
		   (multiple html-parse-multiple))
      field
    (requiredp field required value)
    (when value
      (unless multiple
	(setf value (ensure-list value)))
      (loop for v in value
	    do (unless (ensure-option-value v options)
		 (validate-field-error "The value ~a does not correspond to any option values." v))))))



(defmethod validate-field ((field file) value &key)
  (when (html-parse-disabled field)
    (validate-form-error error-fields nil "Cannot validate form. Disabled fields returned"))
  (with-slots (accept files min-size max-size) field
    (when (and required (string= "" value))
      (validate-field-error "~s is a required field" input-name))
    (loop
      for file in value
      for filename = (accept-file-p field file accept)
      when filename
	do (check-size-constraints field file min-size max-size))))



(defmethod validate-field ((field password) value &key)
  (declare (special stored-fields))
  (when (html-parse-disabled field)
    (validate-form-error error-fields nil "Cannot validate form. Disabled fields returned"))
  (with-slots (parent-field sanitize rules) field
    (requiredp field (html-parse-required field) value)
    (let ((fieldname (symbol-name (slot-definition-name parent-field))))
      (with-accessors ((minlength html-parse-minlength)
		       (maxlength html-parse-maxlength))
	  field
	(cond ((string-equal fieldname "password")
	       (call-next-method))
	      ((string-equal fieldname "new-password")
	       (store-slot fieldname value)
	       (validate-password-rules field value))
	      ((string-equal fieldname "repeat-password")
	       (awhen (assoc "NEW-PASSWORD" stored-fields :test #'string=)
		 (unless (string= value (cdr self))
		   (validate-field-error "passwords do not match")))))
	(when value
	  (signal-convert (sanitize value sanitize)))))))



(defmethod validate-password-rules ((field password) password)
  (with-accessors ((minlength html-parse-minlength)
		   (maxlength html-parse-maxlength))
      field
    (when (and minlength
	       (< (length password) minlength))
      (validate-field-error "Password must be at least ~a characters long" minlength))
    (when (and maxlength
	       (> (length password) maxlength))
      (validate-field-error "Password must be less than ~a characters long" maxlength)))
  (with-slots (special-chars use-numbers capitalize) field
    (when (or special-chars use-numbers capitalize)
      (let ((rules `(,(unless special-chars t)
		     ,(unless use-numbers t)
		     ,(unless capitalize t))))
	(loop
	  for char across password
	  when (and special-chars
		    (member char '(#\# #\$ #\! #\^ #\£ #\€ #\% #\* #\~ #\@) :test #'char=))
	    do (setf (car rules) t)
	  when (and use-numbers (digit-char-p char))
	    do (setf (cadr rules) t)
	  when (and capitalize (upper-case-p char))
	    do (setf (caddr rules) t)
	  finally 
	     (unless (car rules)
		    (validate-field-error "Password must contain at least one special character # $ ! ^ £ € % * ~~ @"))
		  (unless (cadr rules)
		    (validate-field-error "Password must contain at least one number."))
		  (unless (caddr rules)
		    (validate-field-error "Password must contain at least one capital letter.")))))))

	
(defmethod validate-field
    ((field grouped-list) value &key options)
  (let ((value (ensure-list value)))
    (when value
      (setf value (ensure-list value))
      (loop for v in value
	    do (unless (ensure-option-value v options)
		 (validate-field-error "The value ~a does not correspond to any option values." v))))))



(defmethod validate-field
    ((field grouped-table) value &key options)
  (let ((fieldtype (type-of (query-select field #'(lambda (node)
						    (or (typep node 'radio)
							(typep node 'checkbox)))))))
    (loop
      for (a . b) in value
      when (and (consp b)
		(eq fieldtype 'radio))
	do (validate-field-error "Multiple values returned for ~s. Only one expected. Invalid form data." a)
	and do (return)
      do (loop
	   for item in (ensure-list b)
	   do (unless (ensure-option-value
		       item
		       (typecase options
			 (cons (awhen (getf options :rows)
				 (loop
				   for row in self
				   when (string-equal (getf self :name) a)
				     do (return (getf self :fields)))))
			 (grouped-table
			  (loop
			    for row in (slot-value options 'rows)
			    if (string-equal a (slot-value row 'name))
			      do (return (slot-value row 'fields))))))
		(validate-field-error "The value ~a does not correspond to any option values." item))))))



(defun ensure-option-value (value options)
  "Recursively walk a list of objects and test for the presence of value in field"
  (labels ((recurse (value options)
	     (unless (null options)
	       (if (consp (car options))
		   (recurse value (car options))
		   (let ((option (car options)))
		     (when (or (and (typep (class-of option) 'standard-class)
				    (string-equal value (html-parse-value option)))
			       (and (typep option 'string)
				    (string-equal value option)))
		       (return-from ensure-option-value t))))
	       (recurse value (cdr options)))))
    (recurse value options))
  nil)



(defun requiredp (field required value)
  (unless field
    (error "~a is not a field" field))
  (if required
      (with-slots (parent-field) field
	(cond ((consp value)
	       t)
	      ((or (null value)
		   (string= "" value))
	       (validate-field-error "~a is a required field" (slot-definition-name parent-field))
	       nil)
	      (t t)))
      t))


(defun accept-file-p (field file accept &aux (filename (gethash "filename" (cadr file))))
  (unless (member (mimes:mime filename) accept :test #'string-equal)
    (validate-field-error "the file ~a must be of an accepted type. Accepted types include ~{~a~^, ~}" (namestring filename) accept))
  filename)


(defun check-size-constraints (field file min-size max-size)
  (let ((size
	  (typecase (car file)
	    (file-stream
	     (file-length (car file)))
	    (flexi-streams:vector-stream
	     (flex::vector-stream-end (car file))))))
    (cond ((and min-size (< size min-size))
	   (validate-field-error "The file ~a is less than the minimum file size of ~dKB" (/ min-size 1000 )))
	  ((and max-size (> size max-size))
	   (validate-field-error "The file ~a is greater than the maximum allowed file size of ~dMB" (/ max-size 1000000))))))
