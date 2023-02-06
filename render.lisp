(in-package stw.form)


(define-layered-method render-template
  :in form-layer ((form base-form-class) &optional stream args)
  (with-slots (template csrf) form
    (apply #'djula:render-template* template stream :csrf csrf args)))


(define-layered-method render-template
  :in form-layer
  ((instance serialize)
   &optional stream
   (args (object-to-plist
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
			      (t (value)))))
			 (t
			  (value)))))))))
  (awhen (slot-value instance 'error-fields)
    (setf args `(,@args ,@self)))
  (render-template (class-of instance) stream args))
