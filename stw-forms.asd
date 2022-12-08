(defsystem "stw-forms"
  :author "Liam Howley <liam.howley@thespanningtreeweb.ie>"
  :license "MIT"
  :depends-on ("stw-templates"
	       "stw-xml-parse"
	       "stw-html-parse"
	       "stw-utils"
	       "closer-mop"
	       "contextl"
	       "cl-comp"
	       "cl-ppcre"
	       "sanitize"
	       "trivial-mimes")
  :serial t
  :components ((:file "package")
	       (:file "field-definitions")
	       (:file "meta")))
	       
