(in-package #:nyxt-user)


(load (nyxt-init-file "style.lisp"))
(load (nyxt-init-file "status.lisp"))
(load (nyxt-init-file "fill-credentials.lisp"))

(load-after-system :slynk (nyxt-init-file "slynk.lisp"))

(define-configuration (buffer)
  ((default-modes `(emacs-mode ,@%slot-default%))
   ;; (download-engine :renderer)
   (password-interface (make-instance 'password:user-password-store-interface))
   (override-map (let ((map (make-keymap "override-map")))
		   (define-key map
		     "M-x" 'execute-command
		     "M-n" 'switch-buffer-next
		     "M-p" 'switch-buffer-previous)))))

(define-configuration web-buffer
  ((default-new-buffer-url "http://people.bath.ac.uk/feb/surfing.html")
   (default-modes `(auto-mode ,@%slot-default%))
   (buffer-loaded-hook
    (reduce #'hooks:add-hook
	    (mapcar #'nyxt::make-handler-buffer (list #'fill-credentials-if-login-present))
	    :initial-value %slot-default%))))

;; prompt-buffer
(define-configuration prompt-buffer
  ((hide-single-source-header-p t)))

;; turn off follow mode for buffers: it screws the access-time order
(define-configuration buffer-source
  ((prompter:follow-p nil)))
