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



;; (define-configuration prompt-buffer
;;   ((hide-single-source-header-p t)))

;; (define-configuration prompter:prompter
;;   ((prompter:hide-attribute-header-p :single)))


;; (defun my-status-style (&key (mode-background-color "rgb(120,120,120)"))
;;   (cl-css:css
;;    `((body
;;       :background "rgb(160, 160, 160)"
;;       :font-size "14px"
;;       :color "rgb(32, 32, 32)"
;;       :padding 0
;;       :margin 0
;;       :line-height "20px")
;;      (".arrow"
;;       :width "10px"
;;       :height "20px")
;;      (".arrow-right"
;;       :clip-path "polygon(0 100%, 100% 50%, 0 0)")
;;      (".arrow-left"
;;       :clip-path "polygon(0 50%, 100% 100%, 100% 0)")
;;      ("#container"
;;       :display "grid"
;;       ;; Columns: controls, arrow, url, arrow, modes
;;       :grid-template-columns "115px 10px auto 10px auto"
;;       :overflow-y "hidden")
;;      ("#controls"
;;       :background-color "rgb(80,80,80)"
;;       :padding-left "5px"
;;       :overflow "hidden"
;;       :white-space "nowrap")
;;      ("#url"
;;       :background-color "rgb(160,160,160)"
;;       :min-width "100px"
;;       :text-overflow "ellipsis"
;;       :overflow-x "hidden"
;;       :white-space "nowrap"
;;       :padding-left "15px"
;;       :padding-right "10px"
;;       :margin-left "-10px")
;;      ("#modes"
;;       :background-color ,mode-background-color
;;       :color "rgb(230, 230, 230)"
;;       :text-align "right"
;;       :padding-right "5px"
;;       ;; Uncomment the following to trim the mode list.
;;       ;; :text-overflow "ellipsis"
;;       ;; :overflow-x "hidden"
;;       :white-space "nowrap")
;;      (.button
;;       :color "rgb(230, 230, 230)"
;;       :text-decoration "none"
;;       :padding-left "2px"
;;       :padding-right "2px"
;;       :margin-left "2px"
;;       :margin-right "2px")
;;      (|.button:hover|
;;       :color "black"))))

;; (defun my-format-status (window)
;;   (let ((buffer (current-buffer window)))
;;     (if (or (internal-buffer-p buffer)
;;             (find-submode buffer 'proxy-mode))
;;         (setf (style (status-buffer window))
;;               (my-status-style))
;;         (setf (style (status-buffer window))
;;               (my-status-style :mode-background-color "rgb(255,0,0)")))
;;     (markup:markup
;;      (:div :id "container"
;;            (:div :id "controls"
;;                  (markup:raw (format-status-buttons)))
;;            (:div :class "arrow arrow-right"
;;                  :style "background-color:rgb(80,80,80)" "")
;;            (:div :id "url"
;;                  (markup:raw
;;                   (format-status-load-status buffer)
;;                   (format-status-url buffer)))
;;            (:div :class "arrow arrow-left"
;;                  :style "background-color:rgb(220,120,120);background-color:rgb(120,120,120)" "")
;;            (:div :id "modes"
;;                  (format-status-modes buffer))))))

;; (define-configuration window
;;   ((status-formatter #'my-format-status)))
