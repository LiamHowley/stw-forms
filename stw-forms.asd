(defsystem "stw-forms"
  :author "Liam Howley <liam.howley@thespanningtreeweb.ie>"
  :description "Context oriented CLOS to html form map, with automatic template generation, rendering and validation."
  :license "MIT"
  :depends-on ("stw-templates"
	       "stw-xml-parse"
	       "stw-html-parse"
	       "stw-utils"
	       "stw-sanitize"
	       "closer-mop"
	       "contextl"
	       "cl-comp"
	       "cl-ppcre"
	       "local-time"
	       "trivial-mimes"
	       "flexi-streams"
	       "qbase64")
  :serial t
  :components ((:file "package")
	       (:file "field-definitions")
	       (:file "conditions")
	       (:file "meta")
	       (:file "render")
	       (:file "print")
	       (:file "validate"))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "docs/README.org"))
  :in-order-to ((test-op (load-op :stw-forms-test))))
	       
