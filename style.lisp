;; style for nyxt.  Mostly stolen from
;; https://github.com/aartaka/nyxt-config/blob/master/style.lisp

(in-package #:nyxt-user)

;; generalities: background, text colour, buttons and links
;; (define-configuration (internal-buffer prompt-buffer editor-buffer panel-buffer)
;;   ((style
;;     (str:concat
;;      %slot-default%
;;      (cl-css:css
;;       '((body
;; 	 :background-color "#212121"
;; 	 :color "#bdbdb3")
;; 	(hr
;; 	 :color "darkgray")
;; 	(|a:link|
;; 	 :color "#00C0FF")
;; 	(\.button
;; 	 :color "#333333"
;; 	 :background-color "#404040")))))))

;; message area
#+nyxt-2
(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#212121"
         :color "#bdbdb3")))))))

;; prompt
;; Q do we want to see modes here? (I think not)
#+nyxt-2
(define-configuration prompt-buffer
  ((style (str:concat
	   %slot-default%
	   (cl-css:css
	    '((body
	       :background-color "#212121"
	       :color "#bdbdb3")
	      ("#prompt-area"
	       :background-color "#212121")
	      ("#input"
	       :background-color "#404040"
	       :color "#bcbcb3")
	      (".source-name"
	       :color "#4184e4"
	       :background-color "#212121")
	      (".source-content"
	       :background-color "#212121"
	       :width "95%"
	       :border-spacing "4px 1px")
	      (".source-content th"
	       :border "1px solid #bdbdb3"
	       :background-color "#212121"
	       :font-weight "bold")
	      (".source-content td"
	       :text-overflow "ellipsis")
	      ("#selection"
	       :background-color "#212121"
	       :color "#00C0FF")
	      (\.marked
	       :background-color "#212121"
	       :font-weight "bold"
	       :color "#C000FF")
	      (\.selected :background-color "#212121"
			  :color "#4184e4")))))))

;; status bar: adapted from https://gitlab.com/ambrevar/dotfiles/-/blob/master/.config/nyxt/init.lisp
#+nyxt-2 (defun my-status-style ()
  (cl-css:css
   '((body
      :background "#151515"
      :font-size "14px"
      :color "#bdbdb3"
      :padding 0
      :margin 0
      :line-height "20px")
     (".arrow"
      :width "10px"
      :height "20px")
     (".arrow-right"
      :clip-path "polygon(0 100%, 100% 50%, 0 0)")
     (".arrow-left"
      :clip-path "polygon(0 50%, 100% 100%, 100% 0)")
     ("#container"
      :display "grid"
      ;; Columns: controls, arrow, url, tag, arrow, modes
      :grid-template-columns "50px 10px auto 25px 10px 20px"
      :overflow-y "hidden")
     ("#controls"
      :background-color "#313131"
      :padding-left "5px"
      :overflow "hidden"
      :white-space "nowrap")
     ("#url, #url a.button"
      :background-color "#151515"
      :color "#ffc000"
      :min-width "100px"
      :text-overflow "ellipsis"
      :overflow-x "hidden"
      :white-space "nowrap"
      :padding-left "15px"
      :padding-right "10px"
      :margin-left "-10px")
     ("#url a.button:hover"
      :color "#ffff80")
     ("#tag"
      :color "#00C0FF")
     ("#modes"
      :background-color "#313131"
      :color "#818181"
      :text-align "right"
      :padding-right "5px"
      :text-overflow "ellipsis"
      :overflow-x "hidden"
      :white-space "nowrap")
     (".button, .has-history"
      :text-decoration "none"
      :padding-left "2px"
      :padding-right "2px"
      :margin-left "2px"
      :margin-right "2px")
     (|\.button:hover|
      :color "white")
     (".button"
      :color "#bdbdb3")
     (.has-history
      :color "#ffc000")
     (|\.has-history:hover|
      :color "#ffff80"))))

#+nyxt-3
(defun my-status-style ()
  (theme:themed-css (theme *browser*)
    (body
      :font-size "14px"
      :padding 0
      :margin 0
      :line-height "25px")
     (\.loader
      :border-width "2px"
      :border-style "solid"
      :border-color "transparent"
      :border-top-color theme:accent
      :border-left-color theme:accent
      :border-radius "50%"
      :display "inline-block"
      :width "7px"
      :height "7px"
      :animation "spin 1s linear infinite")
     ("@keyframes spin"
      ("0%" :transform "rotate(0deg)")
      ("100%" :transform "rotate(360deg)"))
     (".arrow-right"
      :clip-path "polygon(0 0, calc(100% - 10px) 0, 100% 50%, calc(100% - 10px) 100%, 0 100%)"
      :margin-right "-10px")
     (".arrow-left"
      :clip-path "polygon(10px 0, 100% 0, 100% 100%, 10px 100%, 0% 50%)"
      :margin-left "-10px")
     ("#container"
      :display "grid"
      ;; Columns: controls, url, tag, modes
      :grid-template-columns "55px auto 25px 30px"
      :overflow-y "hidden")
     ("#controls"
      :background-color theme:secondary
      :color theme:on-secondary
      :padding-left "5px"
      :overflow "hidden"
      :white-space "nowrap")
     ("#url"
      :background-color theme:primary
      :color theme:on-primary
      :min-width "100px"
      :text-overflow "ellipsis"
      :overflow-x "hidden"
      :white-space "nowrap"
      :padding-left "15px"
      :padding-right "10px"
      :margin-left "0px")
     ("#url a.button"
      :color "inherit")
     ("#tag"
      :color theme:accent
      :background theme:secondary
      :margin-right 0px
      :padding-right 0px
      :text-align "right")
     ("#modes" 
      :background-color theme:secondary
      :color theme:on-secondary
      :text-align "right"
      :padding-right "5px"
      :text-overflow "ellipsis"
      :overflow-x "hidden"
      :white-space "nowrap")
     (".button, .has-history"
      :text-decoration "none"
      :padding-left "2px"
      :padding-right "2px"
      :margin-left "2px"
      :margin-right "2px")
     (|\.button:hover|
      :opacity 0.6)
     (".button"
      :color "inherit")
     (".has-history"
      :color theme:accent)))


;; special case: branch markers on the history tree
(define-configuration nyxt/history-tree-mode::history-tree-mode
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '(("ul li::before"
	 :background-color "#4184e4")
	("ul li::after"
	 :background-color "#4184e4")
	("ul li:only-child::before"
	 :background-color "#4184e4")))))))

;; lisp repl
(define-configuration nyxt/repl-mode:repl-mode
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#212121"
         :color "#bdbdb3")))))))
