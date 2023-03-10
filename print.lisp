(in-package stw.form)


(defmethod serialize-object ((object grouped-table) (stream stream) &optional indent (include-children t))
  (declare (ignore include-children))
  (with-slots (child-nodes name) object
    (write-char #\> stream)
    (serialize-object (car child-nodes) stream (when indent (+ 3 indent)) t)
    (format stream "{% for row in ~(~a~).rows %} " name)
    (serialize-object (cadr child-nodes) stream (when indent (+ 3 indent)) t)
    (write-string "{% endfor %}" stream)))

(defmethod serialize-object ((object grouped-row-heading) (stream stream) &optional indent (include-children t))
  (declare (ignore include-children))
  (with-slots (child-nodes name) object
    (write-char #\> stream)
    (format stream "{% for heading in ~(~a~).headings %} " name)
    (serialize-object (car child-nodes) stream (when indent (+ 3 indent)) t)
    (write-string "{% endfor %}" stream)))

(defmethod serialize-object ((object grouped-row) (stream stream) &optional indent (include-children t))
  (declare (ignore include-children))
  (with-slots (name) object
    (setf name "row.fields")
    (call-next-method)))

(defmethod serialize-object ((object grouped-list) (stream stream) &optional indent (include-children t))
  (declare (ignore include-children))
  (let* ((serialized-object (xml.parse:serialize object
						 :indent indent
						 :serializer #'serialize-grouped-list))
	 (spliced (explode-string serialized-object '("input "))))
    (write-string (car spliced) stream)
    (write-string (cadr spliced) stream)
    (write-string "{{ field.checked }} " stream)
    (write-string "{{ field.disabled }} " stream)
    (write-string (caddr spliced) stream)))

(defmethod serialize-grouped-list :around ((object grouped-list) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (write-char #\> stream)
  (call-next-method))

(defmethod serialize-grouped-list ((object grouped-row) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (when indent
    (indent-string (+ 3 indent) stream))
  (write-string "<h5>{{ row.heading }}</h5>" stream)
  (call-next-method))

(defmethod serialize-grouped-list ((object grouped-list) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (format stream "{% for field in ~(~a~) %} " (slot-value object 'name))
  (loop
    for child in (slot-value object 'child-nodes)
    do (serialize-object child stream (when indent (+ 3 indent)) t))
  (write-string "{% endfor %}" stream))


(defmethod serialize-object :around ((object field-container) (stream stream) &optional indent include-children)
  (declare (ignore include-children indent))
  (let ((select (query-select object #'(lambda (child)
					 (typep child 'select)))))
    (cond (select
	   (let* ((parent-slot (slot-value select 'parent-field))
		  (name (car (slot-definition-initargs parent-slot))))
	     (write-string "{% if " stream)
	     (format stream "~(~a~)" name)
	     (write-string " %}" stream)
	     (call-next-method)
	     (write-string "{% endif %}" stream)))
	  (t
	   (call-next-method)))))

(defmethod serialize-object ((object error-message) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (write-char #\> stream)
  (let* ((parent-slot (slot-value object 'parent-field))
	 (name (car (slot-definition-initargs parent-slot))))
    (format stream "{% for message in ~(~a-errors~) %}" name)
    (indent-string (+ 3 indent) stream)
    (write-string "<p>{{ message }}</p>" stream)
    (write-string "{% endfor %}" stream)))

(defmethod serialize-object ((object select) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (write-char #\> stream)
  (let* ((parent-field (slot-value object 'parent-field))
	 (name (format nil "~(~a~)" (car (slot-definition-initargs parent-field)))))
    (set-options (+ 3 indent) name stream)))

(defmethod serialize-object ((object datalist) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (write-char #\> stream)
  (let* ((parent-field (slot-value object 'parent-field))
	 (name (format nil "~(~a~)" (car (slot-definition-initargs parent-field)))))
    (set-options (+ 3 indent) name stream)))

(defun set-options (indent name stream)
  (write-string "{% for option in " stream)
  (write-string name stream)
  (write-string " %}"stream)
  (indent-string indent stream)
  (write-string "<option value='{{ option.value }}'{{ option.disabled }}{{ option.selected }}>{{ option.output }}</option>" stream)
  (write-string "{% endfor %}" stream))


(defmethod serialize-object ((object textarea) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (write-char #\> stream)
  (let ((text (slot-value object 'the-content)))
    (when text
      (write-string text stream)))
  (indent-string indent stream)
  (write-string "</textarea>" stream))
