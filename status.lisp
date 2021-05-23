;; less busy status bar for nyxt
(in-package #:nyxt-user)

;; visual indication of whether there is any history
;;
;; detect history (based on https://discourse.atlas.engineer/t/q-how-to-find-out-if-buffer-has-history/108)
(defun forward-history-p (&optional (buffer (current-buffer)))
  (with-data-unsafe (history (history-path buffer))
    ;; the following errors out on a new buffer or window
    (ignore-errors (htree:children (htree:current-owner-node history)))))

(defun backward-history-p (&optional (buffer (current-buffer)))
  (with-data-unsafe (history (history-path buffer))
    ;; the following errors out on a new buffer or window
    (ignore-errors (htree:all-parents history))))

;; we could do more here.  Example: change the forwards command
;; if the history branches here.
(defun my-format-status-buttons ()
  (markup:markup
   (:a :class (if (backward-history-p) "has-history" "button")
       :title "Backwards" :href (lisp-url '(nyxt/web-mode:history-backwards)) "❮")
   (:a :class (if (forward-history-p) "has-history" "button")
       :title "Forwards" :href (lisp-url '(nyxt/web-mode:history-forwards)) "❯")))

(defun my-format-status (window)
  (let ((buffer (current-buffer window)))
    (setf (style (status-buffer window)) (my-status-style))
    (markup:markup
     (:div :id "container"
           (:div :id "controls"
                 (markup:raw (my-format-status-buttons)))
           (:div :class "arrow arrow-right"
                 :style "background-color:rgb(21,21,21);background-color:rgb(49,49,49)"  "")
           (:div :id "url"
                 (markup:raw
                  (format-status-load-status buffer)
                  (format-status-url buffer)))
           (:div :class "arrow arrow-left"
                 :style "background-color:rgb(21,21,21);background-color:rgb(49,49,49)" "")
           (:div :id "modes"
		 :title (format-status-modes buffer window)
		 "--")))))
;; TODO
;; better format status url (truncate the url)?


(define-configuration window 
  ((status-formatter #'my-format-status)))
