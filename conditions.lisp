(in-package stw.form)


(define-condition validate-form-error (simple-error)
  ((field-errors :initarg :field-errors :initform nil)))

(defun validate-form-error (field-errors format-control &rest format-args)
  (error "Form contains errors."
	  'validate-field-error
	  :field-errors field-errors
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
  ((slot-name :initarg :slot-name :reader slot-name)
   (stored-slot :initarg :stored-slot :reader stored-slot)))

(defun signal-convert (value)
  (signal 'signal-convert
	  :value value))

(defun store-slot (slot-name stored-slot)
  (signal 'store-slot
	  :slot-name slot-name 
	  :stored-slot stored-slot))

(defun the-message (c)
  (apply #'format nil (simple-condition-format-control c)
	 (simple-condition-format-arguments c)))
