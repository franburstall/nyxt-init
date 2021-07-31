;; Rudimentary support for filling in credentials on a login page
;; 
;; Usage: call `fill-credentials'
;; 
;; Limitations:
;; 1. Detection of the input elements is super-primitive but is a
;; work-in-progress as we gather more examples.
;; 2. Two page logins are a bit patchy: google works fine; MS less so.
;; It would be good to understand how data-bind works.  The two page
;; stuff would be smoother if we kept a history of logins so that we
;; could pre-populate the fill-credentials prompt with the last login.
;;
;; TODO:
;; 1. Fix all limitations!


(in-package :nyxt)

;; Grab data from password-interface
;;
;; We go via the clipboard to exploit existing methods on
;; password-interfaces as suggested by aartaka

(defun get-login (password-interface &key password-name service)
  "Return specific login for PASSWORD-NAME and SERVICE as string."
  (password:clip-username password-interface
			  :password-name password-name
			  :service service)
  (trivial-clipboard:text))

(defun get-password (password-interface &key password-name service)
  "Return specific login for PASSWORD-NAME and SERVICE as string."
  (password:clip-password password-interface
			  :password-name password-name
			  :service service)
  (trivial-clipboard:text))

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
  ;; `find-elt` returns T or :NULL hence the following unlispy lisp:
  (eq t (find-elt selector)))

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
	  (nyxt::insert-login (nyxt::get-login (password-interface buffer)
						   :password-name nyxt::password-name))
	  (nyxt::insert-pass (nyxt::get-password (password-interface buffer)
						     :password-name nyxt::password-name))
	  (nyxt::focus-submit)))
      (echo-warning "No password manager found.")))

;; Suitable function for buffer-loaded-hook
(defun fill-credentials-if-login-present (buffer)
  "Fire `fill-credentials' if page has a login field"
  (declare (ignore buffer))
  (if (elt-p (str:join "," *username-selectors*))
      (fill-credentials)))

