(in-package #:nyxt-user)


(load (nyxt-init-file "style.lisp"))
(load (nyxt-init-file "status.lisp"))
(load (nyxt-init-file "password.lisp"))

(load-after-system :slynk (nyxt-init-file "slynk.lisp"))

(define-configuration (buffer)
  ((default-modes `(emacs-mode ,@%slot-default%))
   ;; (download-engine :renderer)
   (password-interface (make-instance 'password:user-password-store-interface))))

(define-configuration web-buffer
    ((default-new-buffer-url "http://people.bath.ac.uk/feb/surfing.html")
     (default-modes `(auto-mode ,@%slot-default%))))

;; prompt-buffer
(define-configuration prompt-buffer
  ((hide-single-source-header-p t)))

;; turn off follow mode for buffers: it screws the access-time order
(define-configuration buffer-source
  ((prompter:follow-p nil)))
