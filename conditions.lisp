(in-package stw.form)


(define-condition validate-form-error (simple-error)
  ((field-errors :initarg :field-errors :initform nil :reader field-errors)
   (field-values :initarg :field-values :initform nil :reader field-values))
  (:report (lambda (c s)
	     (declare (ignore c))
	     (format s "Form contains errors."))))

(defun validate-form-error (field-errors field-values format-control &rest format-args)
  (error 'validate-form-error
	 :field-errors field-errors
	 :field-values field-values
	 :format-control format-control
	 :format-arguments format-args))



(define-condition validate-field-error (simple-error)())

(defun validate-field-error (format-control &rest format-args)
  (cerror "Store error data and continue"
	  'validate-field-error
	  :format-control format-control
	  :format-arguments format-args))



(define-condition signal-convert (simple-condition)
  ((value :initarg :value :reader value)))

(define-condition store-slot (simple-condition)
  ((stored-value :initarg :stored-value :reader stored-value)
   (fieldname :initarg :fieldname :reader fieldname)))

(defun signal-convert (value)
  (signal 'signal-convert
	  :value value))

(defun store-slot (fieldname stored-value)
  (signal 'store-slot
	  :stored-value stored-value 
	  :fieldname fieldname))

(defun the-message (c)
  (apply #'format nil (simple-condition-format-control c)
	 (simple-condition-format-arguments c)))
