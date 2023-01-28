(defsystem #:stw-forms-test
    :description "Tests for rendering and validating forms"
    :depends-on ("stw-forms" "contextl" "parachute")
    :components ((:file "test"))
    :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :form.test)))
