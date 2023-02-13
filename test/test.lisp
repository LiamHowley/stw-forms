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
  (:csrf-p . t)
  (:action . "/login"))

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
  (:csrf-p . nil)
  (:template . ("stw-forms" "test/templates/change-password-form.html")))

(define-test sanity
  :parent all-tests
  (with-active-layers (form-layer)
    (of-type form-class (find-class 'login))
    (of-type djula::compiled-template (slot-value (find-class 'login) 'stw.form::template))
    (true (csrf-p (find-class 'login)))
    (false (novalidate (find-class 'login)))))


(define-test render1
  :parent all-tests
  (with-active-layers (form-layer)
    (fail (render-template (make-instance 'login :submit "Log In")) 'rendering-error)
    (is string-equal "<form method='get' action='/login' name='login'>
   <input class='form-field input-field hidden' type='hidden' name='csrf-token' value='A CSRF token' />
   <div id='name-text-container' class='form-field-container'>
      <input class='form-field input-field' type='text' name='name' required value='' />
      <div class='error-message'>
      </div>
   </div>
   <div id='password-password-container' class='form-field-container'>
      <input class='form-field input-field' type='password' name='password' required value='' />
      <div class='error-message'>
      </div>
   </div>
   <div id='submit-submit-container' class='form-field-container'>
      <input class='form-field input-field' type='submit' name='submit' value='Log In' />
   </div>
</form>"
	(let ((instance (make-instance 'login :submit "Log In")))
	  (render-template instance nil (default-arguments instance "A CSRF token"))))
    (is string-equal "<form method='get' action='/login' name='login'>
   <input class='form-field input-field hidden' type='hidden' name='csrf-token' value='A CSRF token' />
   <div id='name-text-container' class='form-field-container'>
      <input class='form-field input-field' type='text' name='name' required value='Liam' />
      <div class='error-message'>
      </div>
   </div>
   <div id='password-password-container' class='form-field-container'>
      <input class='form-field input-field' type='password' name='password' required value='12345678' />
      <div class='error-message'>
      </div>
   </div>
   <div id='submit-submit-container' class='form-field-container'>
      <input class='form-field input-field' type='submit' name='submit' value='Log In' />
   </div>
</form>"
	(let ((instance (make-instance 'login :submit "Log In" :name "Liam" :password "12345678")))
	  (render-template instance nil (default-arguments instance "A CSRF token"))))))


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
  (:csrf-p . nil)
  (:template . ("stw-forms" "test/templates/account-form.html")))

(define-layered-method retrieve-options :in form-layer ((class stw.form::form-class) (slot-name (eql 'privileges)) &key value)
  (declare (ignore value))
  (list (make-instance 'option :value "admin" :output "Administration")
	(make-instance 'option :value "toplevel" :output "Top Secret" :selected " selected")
	(make-instance 'option :value "eyesonly" :output "Eyes Only")))


(define-test render2
  :parent all-tests
  (with-active-layers (form-layer)
    (is string-equal "<form name='account'>
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
      <datalist id='options' class='form-field'>
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
      <input class='form-field input-field' type='submit' name='submit' value='Create Account' />
   </div>
</form>"
	(render-template (make-instance 'account)))))


(define-form group-membership ()
  ((groups :fieldtype grouped-list
	   :input-type checkbox)
   (submit :fieldtype submit 
	   :initform "Submit"
	   :accessor submit))
  (:csrf-p . nil)
  (:template . ("stw-forms" "test/templates/group-membership-form.html")))

(define-layered-method retrieve-options :in form-layer ((class stw.form::form-class) (slot-name (eql 'groups)) &key value)
  (declare (ignore value))
  '((:value "admin" :id "admin" :label "Admin")
    (:value "topsecret" :id "topsecret" :label "Top Secret" :checked "checked")
    (:value "eyesonly" :id "eyesonly" :label "Eyes Only")))

(define-test render3
  :parent all-tests
  (with-active-layers (form-layer)
    (is string-equal "<form name='group-membership'>
   <div id='groups-grouped-list-container' class='form-field-container'>
      <div class='grouped-fields-list'> 
         <div class='checkbox-wrap'>
            <label for='admin'>Admin
            </label>
            <div class='labelled-field'>
               <input   id='admin' class='form-field input-field' type='checkbox' name='groups[]' value='admin' />
            </div>
         </div> 
         <div class='checkbox-wrap'>
            <label for='topsecret'>Top Secret
            </label>
            <div class='labelled-field'>
               <input checked  id='topsecret' class='form-field input-field' type='checkbox' name='groups[]' value='topsecret' />
            </div>
         </div> 
         <div class='checkbox-wrap'>
            <label for='eyesonly'>Eyes Only
            </label>
            <div class='labelled-field'>
               <input   id='eyesonly' class='form-field input-field' type='checkbox' name='groups[]' value='eyesonly' />
            </div>
         </div>
      </div>
      <div class='error-message'>
      </div>
   </div>
   <div id='submit-submit-container' class='form-field-container'>
      <input class='form-field input-field' type='submit' name='submit' value='Submit' />
   </div>
</form>"
	(render-template (make-instance 'group-membership)))))

(define-layered-method retrieve-options :in form-layer ((class stw.form::form-class) (slot-name (eql 'groups)) &key value)
  (declare (ignore value))
  '((:value "admin" :id "admin" :label "Admin")
    (:value "topsecret" :id "topsecret" :label "Top Secret" :checked "checked")
    (:value "eyesonly" :id "eyesonly" :label "Eyes Only")))


(define-form group-status ()
  ((status :fieldtype grouped-table
	   :input-type radio)
   (submit :fieldtype submit 
	   :initform "Submit"
	   :accessor submit))
  (:csrf-p . nil)
  (:template . ("stw-forms" "test/templates/group-status-form.html")))


(define-layered-method retrieve-options :in form-layer ((class stw.form::form-class) (slot-name (eql 'status)) &key value)
  (declare (ignore value))
  (make-instance 'grouped-table
		 :headings `(,(make-instance 'row-headings :row "Admin")
			     ,(make-instance 'row-headings :row "Eyes Only"))
		 :rows `(,(make-instance 'form-row
					 :heading "Accounts"
					 :name "accounts"
					 :fields `(,(make-instance 'grouped-field
								   :id "admin"
								   :checked "checked"
								   :name "admin"
								   :value "admin")
						   ,(make-instance 'grouped-field
								   :id "eyesonly"
								   :name "eyesonly"
								   :value "eyesonly")))
			 ,(make-instance 'form-row
					 :heading "Security"
					 :name "security"
					 :fields `(,(make-instance 'grouped-field
								   :id "admin"
								   :checked "checked" 
								   :name "admin"
								   :value "Admin")
						   ,(make-instance 'grouped-field
								   :id "eyesonly"
								   :name "eyesonly"
								   :value "eyesonly"))))))


(define-test render4
  :parent all-tests
  (with-active-layers (form-layer)
    (is string-equal "<form name='group-status'>
   <div id='status-grouped-table-container' class='form-field-container'>
      <div class='group-fields-table'>
         <div class='grouped-fields-row'> 
            <div class='column-heading'>
               <h5 class='heading'>Admin
               </h5>
            </div> 
            <div class='column-heading'>
               <h5 class='heading'>Eyes Only
               </h5>
            </div>
         </div> 
         <div class='grouped-fields-row'>
            <h5>Accounts</h5> 
            <div class='radio-field-wrap'>
               <input checked  class='form-field input-field' type='radio' name='status[accounts]' value='admin' />
            </div> 
            <div class='radio-field-wrap'>
               <input   class='form-field input-field' type='radio' name='status[accounts]' value='eyesonly' />
            </div>
         </div> 
         <div class='grouped-fields-row'>
            <h5>Security</h5> 
            <div class='radio-field-wrap'>
               <input checked  class='form-field input-field' type='radio' name='status[security]' value='Admin' />
            </div> 
            <div class='radio-field-wrap'>
               <input   class='form-field input-field' type='radio' name='status[security]' value='eyesonly' />
            </div>
         </div>
      </div>
      <div class='error-message'>
      </div>
   </div>
   <div id='submit-submit-container' class='form-field-container'>
      <input class='form-field input-field' type='submit' name='submit' value='Submit' />
   </div>
</form>"
	(render-template (make-instance 'group-status)))))


(define-test validate
  :parent all-tests
  (with-active-layers (form-layer)
    ;; sanity
    (true (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "foobarbaz"))))
    ;; required email
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (profile . "bar") (password . "foobarbaz"))))

    ;; invalid email
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (email . "foobar.baz") (profile . "bar") (password . "foobarbaz"))))

    ;; required name
    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (profile . "bar") (email . "foo@bar.baz") (password . "foobarbaz")) :csrf-token "csrf-token"))
    ;; required password

    (fail (validate-form (find-class 'account) '((privileges . ("admin")) (name . "foo") (email . "foo@bar.baz") (profile . "bar")) :csrf-token "csrf-token"))

    ;; invalid option "topsecret"
    (fail (validate-form (find-class 'account) '((privileges . ("topsecret")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "foobarbaz")) :csrf-token "csrf-token"))

    ;; fails minlength on password
    (fail (validate-form (find-class 'account) '((privileges . ("toplevel")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "baz")) :csrf-token "csrf-token"))

    ;; fails maxlength on password
    (fail (validate-form (find-class 'account) '((privileges . ("eyesonly")) (name . "foo") (email . "foo@bar.baz") (profile . "bar") (password . "foobarbaz!G")) :csrf-token "csrf-token"))

    (true (validate-form (find-class 'group-membership) '((groups . ("admin" "topsecret" "eyesonly")))))

    (true (validate-form (find-class 'group-membership) '((groups . ("admin")))))

    (true (validate-form (find-class 'group-membership) '((groups . ("topsecret")))))

    (true (validate-form (find-class 'group-membership) '((groups . ("eyesonly")))))

    (fail (validate-form (find-class 'group-membership) '((groups . ("admin" "topsecret" "eyesonly" "root")))))

    (fail (validate-form (find-class 'group-membership) '((groups . ("admin" "topsecret" "youreyesonly")))))

    (true (validate-form (find-class 'group-status) '((status (accounts . "admin") (security . "eyesonly")))))

    (true (validate-form (find-class 'group-status) '((status (accounts . "eyesonly") (security . "admin")))))

    (fail (validate-form (find-class 'group-status) '((status (accounts . "topsecret") (security . "admin")))))

    (fail (validate-form (find-class 'group-status) '((status (accounts . "eyesonly") (security . "topsecret")))))))



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


(define-test validate-csrf
  :parent all-tests
  (with-active-layers (form-layer)
    (let ((values '((submit . "Log In") (name . "Liam") (password  . "12345678"))))
      ;; no valid csrf token
      (fail (validate-form (find-class 'login) values))
      ;; only server side  csrf token
      (fail (validate-form (find-class 'login) values :csrf-token-server "test token"))
      ;; only client side  csrf token
      (fail (validate-form (find-class 'login) values :csrf-token-client "test token"))
      ;; non-matching tokens
      (fail (validate-form (find-class 'login) values :csrf-token-server "test token" :csrf-token-client "wrong token"))
      ;; matching csrf-tokens
      (true (validate-form (find-class 'login) values :csrf-token-server "test token" :csrf-token-client "test token")))))
