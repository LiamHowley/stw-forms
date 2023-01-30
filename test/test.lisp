(defpackage form.test
  (:use :cl
	:parachute
	:stw.form)
  (:import-from :cl-comp
		:filter-slots-by-type)
  (:import-from :contextl
		:define-layered-method
		:with-active-layers)
  (:shadowing-import-from
   :stw.form
   :output)
  (:export :sanity-test))

(in-package form.test)

(define-test all-tests)

;;;; setting up

(define-form login ()
  ((name :required t)
   (password :fieldtype password :required t)
   (submit :fieldtype submit))
  (:template . ("stw-forms" "test/templates/log-in.html"))
  (:method . :GET)
  (:csrf . "A CSRF token")
  (:action . "/login"))

(define-form account ()
  ((name :label "Name"
	 :required t
	 :autofocus t)
   (email :fieldtype email
	  :label "Email"
	  :required t)
   (profile :fieldtype textarea
	    :label "Profile"
	    :required t
	    :placeholder "blah")
   (visiblity :fieldtype datalist
	      :list "options"
	      :input-name "visibility"
	      :label "Make account visible to group ...")
   (privileges :fieldtype select
	       :input-name "type")
   (active :fieldtype checkbox
	   :label "Active")
   (password :fieldtype password
	     :required t
	     :maxlength 10
	     :minlength 6 
	     :label "Password")
   (submit :fieldtype submit 
	   :initform "Create Account"
	   :accessor submit))
  (:csrf . "csrf-token")
  (:template . ("stw-forms" "test/templates/account-form.html")))

(define-form new-password ()
  ((password :fieldtype password
	     :required t
	     :label "Password")
   (new-password :fieldtype password
		 :required t
		 :maxlength 10
		 :minlength 6 
		 :special-chars t
		 :use-numbers t
		 :capitalize t
		 :label "Password")
   (repeat-password :fieldtype password
		    :required t
		    :label "Password")
   (submit :fieldtype submit 
	   :initform "New Password"
	   :accessor submit))
  (:template . ("stw-forms" "test/templates/change-password-form.html")))


(define-test sanity
    :parent all-tests
    (with-active-layers (form-layer)
      (of-type form-class (find-class 'login))
      (of-type djula::compiled-template (slot-value (find-class 'login) 'stw.form::template))
      (is string= "A CSRF token" (csrf (find-class 'login)))
      (false (novalidate (find-class 'login)))))


(define-test render1
  :parent all-tests
  (with-active-layers (form-layer)
    (is string-equal "<form method='get' action='/login' name='login'>
   <input class='form-field input-field hidden' type='hidden' name='csrf-token' value='A CSRF token' />
   <div id='name-text-container' class='form-field-container'>
      <input id='name-text' class='form-field input-field' type='text' name='name' required value='' />
      <div class='error-message'>
      </div>
   </div>
   <div id='password-password-container' class='form-field-container'>
      <input id='password-password' class='form-field input-field' type='password' name='password' required value='' />
      <div class='error-message'>
      </div>
   </div>
   <div id='submit-submit-container' class='form-field-container'>
      <input id='submit-submit' class='form-field input-field' type='submit' name='submit' value='Log In' />
      <div class='error-message'>
      </div>
   </div>
</form>"
	(render-template (make-instance 'login :submit "Log In"))
	(is string-equal "<form method='get' action='/login' name='login'>
   <input class='form-field input-field hidden' type='hidden' name='csrf-token' value='A CSRF token' />
   <div id='name-text-container' class='form-field-container'>
      <input id='name-text' class='form-field input-field' type='text' name='name' required value='Liam' />
      <div class='error-message'>
      </div>
   </div>
   <div id='password-password-container' class='form-field-container'>
      <input id='password-password' class='form-field input-field' type='password' name='password' required value='12345678' />
      <div class='error-message'>
      </div>
   </div>
   <div id='submit-submit-container' class='form-field-container'>
      <input id='submit-submit' class='form-field input-field' type='submit' name='submit' value='Log In' />
      <div class='error-message'>
      </div>
   </div>
</form>"
	    (render-template (make-instance 'login :submit "Log In" :name "Liam" :password "12345678"))))))

(define-layered-method retrieve-options :in form-layer ((class stw.form::form-class) (slot-name (eql 'privileges)) &key value)
  (declare (ignore value))
  (list (make-instance 'option :value "admin" :output "Administration")
	(make-instance 'option :value "toplevel" :output "Top Secret" :selected " selected")
	(make-instance 'option :value "eyesonly" :output "Eyes Only")))


(define-test render2
  :parent all-tests
  (with-active-layers (form-layer)
    (is string-equal "<form name='account'>
   <input class='form-field input-field hidden' type='hidden' name='csrf-token' value='csrf-token' />
   <div id='name-text-container' class='form-field-container'>
      <label for='name-text'>Name
      </label>
      <div class='labelled-field'>
         <input id='name-text' class='form-field input-field' type='text' autofocus name='name' required value='' />
      </div>
      <div class='error-message'>
      </div>
   </div>
   <div id='email-email-container' class='form-field-container'>
      <label for='email-email'>Email
      </label>
      <div class='labelled-field'>
         <input id='email-email' class='form-field input-field' type='email' name='email' required value='' />
      </div>
      <div class='error-message'>
      </div>
   </div>
   <div id='profile-textarea-container' class='form-field-container'>
      <label for='profile-textarea'>Profile
      </label>
      <div class='labelled-field'>
         <textarea id='profile-textarea' class='form-field' name='profile' placeholder='blah' required>
         </textarea>
      </div>
      <div class='error-message'>
      </div>
   </div>
   <div id='visiblity-datalist-container' class='form-field-container'>
      <label for='visiblity-datalist'>Make account visible to group ...
      </label>
      <div class='labelled-field'>
         <input id='visiblity-datalist' class='form-field input-field' type='text' list='options' name='visiblity' />
      </div>
      <datalist id='options'>
         
      </datalist>
      <div class='error-message'>
      </div>
   </div>
   
   <div id='privileges-select-container' class='form-field-container'>
      <select id='privileges-select' class='form-field' name='privileges'>
         
         <option value='admin'>Administration</option>
         
         <option value='toplevel' selected>Top Secret</option>
         
         <option value='eyesonly'>Eyes Only</option>
         
      </select>
      <div class='error-message'>
      </div>
   </div>
   
   <div id='active-checkbox-container' class='form-field-container'>
      <div class='checkbox-wrap'>
         <label for='active-checkbox'>Active
         </label>
         <div class='labelled-field'>
            <input id='active-checkbox' class='form-field input-field' type='checkbox' name='active' value='active' />
         </div>
      </div>
      <div class='error-message'>
      </div>
   </div>
   <div id='password-password-container' class='form-field-container'>
      <label for='password-password'>Password
      </label>
      <div class='labelled-field'>
         <input id='password-password' class='form-field input-field' type='password' maxlength='10' minlength='6' name='password' required value='' />
      </div>
      <div class='error-message'>
      </div>
   </div>
   <div id='submit-submit-container' class='form-field-container'>
      <input id='submit-submit' class='form-field input-field' type='submit' name='submit' value='Create Account' />
      <div class='error-message'>
      </div>
   </div>
</form>"
	(render-template (make-instance 'account)))))

(define-test validate
  :parent all-tests
  (with-active-layers (form-layer)
    ;; sanity
    (true (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "foobarbaz")) :csrf-token "csrf-token"))
    ;; required email
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (profile . "bar") (password . "foobarbaz")) :csrf-token "csrf-token"))

    ;; invalid email
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (email . "foobar.baz") (profile . "bar") (password . "foobarbaz")) :csrf-token "csrf-token"))

    ;;fail on csrf token absence
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "foobarbaz"))))
    ;;fail on invalid csrf token
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "foobarbaz")) :csrf-token "invalid-csrf-token"))

    ;; required name
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (profile . "bar") (email . "foo@bar.baz") (password . "foobarbaz")) :csrf-token "csrf-token"))
    ;; required password
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (email . "foo@bar.baz") (profile . "bar")) :csrf-token "csrf-token"))
    ;; invalid option "topsecret"
    (fail (validate-form (find-class 'account) '((privileges . ("topsecret")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "foobarbaz")) :csrf-token "csrf-token"))

    ;; fails minlength on password
    (fail (validate-form (find-class 'account) '((privileges . ("toplevel")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "baz")) :csrf-token "csrf-token"))

    ;; fails maxlength on password
    (fail (validate-form (find-class 'account) '((privileges . ("eyesonly")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "foobarbaz!G")) :csrf-token "csrf-token"))))

(define-test validate-password
  :parent all-tests
  (with-active-layers (form-layer)
    ;; sanity
    (true (validate-form (find-class 'new-password) '((password . "1234567") (new-password . "a$8Cd3f") (repeat-password . "a$8Cd3f"))))

    ;; new password and repeat-password are not equal
    (fail (validate-form (find-class 'new-password) '((password . "1234567") (new-password . "a$8Cd3f") (repeat-password . "ab8Cd3f"))))

    ;; new password is too long
    (fail (validate-form (find-class 'new-password) '((password . "1234567") (new-password . "a$8Cd3fa$8Cd3f") (repeat-password . "a$8Cd3fa$8Cd3f"))))

    ;; new password is too short
    (fail (validate-form (find-class 'new-password) '((password . "1234567") (new-password . "a$8C") (repeat-password . "a$8C"))))

    ;; new password has no capital letters
    (fail (validate-form (find-class 'new-password) '((password . "1234567") (new-password . "a$8cd3f") (repeat-password . "a$8cd3f"))))

    ;; new password has no special chars
    (fail (validate-form (find-class 'new-password) '((password . "1234567") (new-password . "aw8Cd3f") (repeat-password . "aw8Cd3f"))))

    ;; new password has no numbers
    (fail (validate-form (find-class 'new-password) '((password . "1234567") (new-password . "a$BCdef") (repeat-password . "a$BCdef"))))))
