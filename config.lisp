(in-package #:nyxt-user)

(define-nyxt-user-system-and-load nyxt-user/load-config-files
  :components ("fill-credentials"
	       "buffer-tags"
	       "style"
	       "status"))

(reset-asdf-registries)

#-quicklisp
(let ((quicklisp-init
	(merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :slynk)

(defmacro load-after-system* (system file)
    `(define-nyxt-user-system-and-load ,(gensym "NYXT-USER/")
     :depends-on (,system) :components (,file)))

(load-after-system* :slynk "slynk.lisp")
;; (load-after-system* :nx-search-engines "search-engines.lisp")

(define-configuration browser
  ((default-new-buffer-url "http://people.bath.ac.uk/feb/surfing.html")
   (theme (make-instance 'theme:theme
                         :dark-p t
                         :background-color "#212121"
                         :on-background-color "#bdbdb3"
                         :primary-color "rgb(170, 170, 170)"
                         :on-primary-color "black"
                         :secondary-color "#313131"
                         :on-secondary-color "#818181"
                         :accent-color "#4184e4"
                         :on-accent-color "black"))))

(define-configuration (buffer)
  ((default-modes `(
		    buffer-tag-mode
		    nyxt/emacs-mode:emacs-mode
		    ,@%slot-value%))
   ;; (download-engine :renderer)
   ))

(define-configuration (web-buffer)
  ((override-map (let ((map (make-keymap "override-map")))
		   (define-key map
		     "M-x" 'execute-command
		     "M-n" 'switch-buffer-next
		     "M-p" 'switch-buffer-previous
		     "M-tab" 'switch-buffer-last
		     "C-h e" 'nyxt/message-mode:list-messages
		     "C-x b" (lambda-command my/switch-buffer ()
			       "Switch-buffer with expected buffer-order."
			       (switch-buffer :current-is-last-p t)))))
   ;; (default-new-buffer-url "http://people.bath.ac.uk/feb/surfing.html")
   ;; (default-modes `(auto-mode ,@%slot-value%))
   (buffer-loaded-hook
    (reduce #'hooks:add-hook
	    (list #'fill-cred::fill-credentials-if-login-present)
	    :initial-value %slot-value%))
   ))
;; prompt-buffer

(define-configuration prompt-buffer
  ((hide-single-source-header-p t)))

;; turn off follow mode for buffers: it screws the access-time order
(define-configuration buffer-source
  ((prompter:selection-actions-enabled-p nil)))

;; auto-fill system information
(define-configuration nyxt/autofill-mode:autofill-mode
  ((nyxt/autofill-mode:autofills (list (nyxt/autofill-mode:make-autofill :name "system-information"
				   :fill #'nyxt::system-information)))))

;; password interface
(define-configuration (nyxt/password-mode:password-mode)
  ((nyxt/password-mode:password-interface (make-instance 'password:password-store-interface))))
