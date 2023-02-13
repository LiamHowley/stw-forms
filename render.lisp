(in-package stw.form)


(define-layered-method render-template
  :in form-layer ((form base-form-class) &optional stream args)
  (with-slots (template) form
    (apply #'djula:render-template* template stream args)))



(define-layered-method render-template
  :in form-layer ((instance serialize) &optional stream (args (default-arguments instance)))
  (awhen (slot-value instance 'error-fields)
    (setf args `(,@args ,@self)))
  (render-template (class-of instance) stream args))


(defmethod default-arguments ((instance serialize) &optional csrf)
  (let ((class (class-of instance)))
    (with-slots (csrf-p) class
      (when csrf-p
	(unless csrf
	  (restart-case
	      (rendering-error "Form requires a csrf token to render")
	    (use-value (token)
	      (setf csrf (list token)))))
	(setf csrf (ensure-list csrf))
	(push :csrf-token csrf)))
    (append
     csrf
     (object-to-plist
      instance
      :map #'(lambda (object slot)
	       (let ((slot-name (slot-definition-name slot)))
		 (flet ((value ()
			  (when (slot-boundp object slot-name)
			    (slot-value object slot-name))))
		   (typecase slot
		     (form-slot-definition
		      (let ((fieldtype (slot-value slot 'fieldtype)))
			(case fieldtype
			  (datalist (retrieve-options (class-of object) slot-name))
			  (select (retrieve-options (class-of object) slot-name :value (value)))
			  (grouped-list (retrieve-options (class-of object) slot-name :value (value)))
			  (grouped-table (retrieve-options (class-of object) slot-name :value (value)))
			  (t (value)))))
		     (t
		      (value))))))))))


(defun generate-csrf-token ()
  (qbase64:encode-bytes
   (make-array '60
	       :initial-contents
	       (loop
		 for i from 1 to 60
		 collect (random 9)))))
