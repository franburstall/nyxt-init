(in-package #:nyxt-user)

(load (nyxt-init-file "style.lisp"))

(load-after-system :slynk (nyxt-init-file "slynk.lisp"))

(define-configuration (buffer)
  ((default-modes `(emacs-mode ,@%slot-default%))
   ;; (download-engine :renderer)
   (password-interface (make-instance 'password:user-password-store-interface))))

(define-configuration (internal-buffer prompt-buffer editor-buffer)
  ((default-modes `(emacs-mode ,@%slot-default%))))

(define-configuration (web-buffer nosave-buffer)
  ((default-modes `(emacs-mode
		    auto-mode
		    ,@%slot-default%))))

(define-configuration web-buffer
  ((default-new-buffer-url "http://people.bath.ac.uk/feb/surfing.html")))

;; prompt-buffer
(define-configuration prompt-buffer
  ((hide-single-source-header-p t)))

