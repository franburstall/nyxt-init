;; Rudimentary support for filling in credentials on a login page
;; 
;; Usage: call `fill-credentials'
;; 
;; Limitations:
;; 1. Only supports unix pass right now
;; 2. Detection of the input elements is super-primitive but is a
;; work-in-progress as we gather more examples.
;; 3. Two page logins are a bit patchy: google works fine; MS less so.
;; It would be good to understand how data-bind works.  The two page
;; stuff would be smoother if we kept a history of logins so that we
;; could pre-populate the fill-credentials prompt with the last login.
;;
;; TODO:
;; 1. Fix all limitations!

;; first we need to grab data from the password-interface
(in-package :password)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'get-login))
(defgeneric get-login (password-interface &key password-name service)
  (:documentation "Return specific login for PASSWORD-NAME and SERVICE as string."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'get-password))
(defgeneric get-password (password-interface &key password-name service)
  (:documentation "Return specific password for PASSWORD-NAME and SERVICE as string."))

;; implementations for password-store

(defmethod get-login ((password-interface password-store-interface) &key password-name service)
  (declare (ignore service))
  (let ((login-line (second
		     (str:lines
		      (execute password-interface (list "show" password-name)
			:output '(:string :stripped t))))))
    (cond ((and login-line (str:starts-with? "login: " login-line))
	   (str:substring (length "login: ") nil login-line))
	  (t
	   (nyxt:echo-warning "No username found for ~a." password-name)
	   ""))))

(defmethod get-password ((password-interface password-store-interface) &key password-name service)
  (declare (ignore service))
  (let ((password (car
		   (str:lines
		    (execute password-interface (list "show" password-name)
		      :output '(:string :stripped t))))))
    (when (str:empty? password)
      (nyxt:echo-warning "Empty password for ~a." password-name))
    password))

;; now provide a user-level command to do the job
(in-package :nyxt)

;; utilities to insert or focus various input fields

;; TODO: warn if we don't find the target element
(define-parenscript insert-in-elt (s selector)
  (let ((elt (nyxt/ps:qs document (ps:lisp selector))))
    (unless (null elt) (setf (ps:chain elt value) (ps:lisp s)))))

(define-parenscript find-elt (selector)
  (let ((elt (nyxt/ps:qs document (ps:lisp selector))))
    (if (null elt) (ps:lisp nil) (ps:lisp t))))

(defun elt-p (selector)
  "Non-nil when SELECTOR matches an element in the current buffer."
  (equal "true" (find-elt selector)))

;; TODO: more complete heuristics for finding the username field
(defparameter *username-selectors* (list
				    "input[autocomplete=\"username\"]" ;best practice
				    "input[type=\"email\"]" ;SO, MS
				    "input[name=\"user\"]"  ;nextcloud
				    "input[name=\"username\"]" ;U of B SSO
				    "input[name=\"uname\"]")
  "List of selectors that match usernames in password dialogues.")


(defun insert-login (s)
  "Insert string S in first plausible username field."
  (let ((selector (str:join ", " *username-selectors*)))
    (insert-in-elt s selector)))

(defun insert-pass (s)
  "Insert string S in first plausible password field."
  (let ((selector (str:join ", " (list
				  "input[autocomplete=\"current-password\"]" ;best practice
				  "input[type=\"password\"]" ;very common
				  ))))
    (insert-in-elt s selector)))


(define-parenscript focus-element (selector)
  (let ((elt (nyxt/ps:qs document (ps:lisp selector))))
    (unless (null elt)
      (ps:chain elt (focus)))))

(defun focus-submit ()
  (focus-element "input[type=\"submit\"]"))

;; user-level entry point
(define-command fill-credentials (&optional (buffer (current-buffer)))
  "Fill in login credentials in BUFFER."
  (nyxt::password-debug-info)
  (if (password-interface buffer)
      (nyxt::with-password (password-interface buffer)
	(let ((nyxt::password-name
		(first
		 (prompt
		  :input (quri:uri-domain (url buffer))
		  :sources
		  (list
		   (make-instance 'nyxt::password-source :buffer buffer
							 :actions nil
							 :password-instance
				  (password-interface buffer)))))))
	  (nyxt::insert-login (password::get-login (password-interface buffer)
						   :password-name nyxt::password-name))
	  (nyxt::insert-pass (password::get-password (password-interface buffer)
						     :password-name nyxt::password-name))
	  (nyxt::focus-submit)))
      (echo-warning "No password manager found.")))

;; Suitable function for buffer-loaded-hook
(defun fill-credentials-if-login-present (buffer)
  "Fire `fill-credentials' if page has a login field"
  (declare (ignore buffer))
  (if (elt-p (str:join "," *username-selectors*))
      (fill-credentials)))

