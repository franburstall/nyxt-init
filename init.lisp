(in-package #:nyxt-user)

(load (nyxt-init-file "style.lisp"))

(load-after-system :slynk (nyxt-init-file "slynk.lisp"))

(define-configuration (buffer)
  ((default-modes `(emacs-mode ,@%slot-default%))
   ;; (download-engine :renderer)
   (password-interface (make-instance 'password:user-password-store-interface))))

(define-configuration web-buffer
  ((default-new-buffer-url "http://people.bath.ac.uk/feb/surfing.html")))

;; prompt-buffer
(define-configuration prompt-buffer
  ((hide-single-source-header-p t)))

;; turn off follow mode for buffers: it screws the access-time order
;; Current fails: see issue #1430
;; (define-configuration buffer-source
;;   ((prompter:follow-p nil)))
