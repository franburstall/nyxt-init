;; less busy status bar for nyxt
(in-package #:nyxt-user)

;; visual indication of whether there is any history
;;
;; detect history (based on https://discourse.atlas.engineer/t/q-how-to-find-out-if-buffer-has-history/108)
(defun forward-history-p (&optional (buffer (current-buffer)))
  (nfiles:with-file-content (history (history-file buffer))
    ;; ignore-errors for new buffer or window:
    (ignore-errors (htree:children (htree:current (htree:owner history (id buffer)))))))

(defun backward-history-p (&optional (buffer (current-buffer)))
  (nfiles:with-file-content (history (history-file buffer))
    ;; ignore-errors for new buffer or window:
    (ignore-errors (htree:all-parents history :owner (id buffer)))))

;; we could do more here.  Example: change the forwards command
;; if the history branches here.
(defun my-format-status-buttons ()
  (spinneret:with-html-string
   (:a :class (if (backward-history-p) "has-history" "button")
       :title "Backwards" ;; (nyxt-url 'nyxt/history-mode:history-backwards)
       "❮")
   (:a :class (if (forward-history-p) "has-history" "button")
       :title "Forwards" ;; (nyxt-url 'nyxt/history-mode:history-forwards)
       "❯")))

;; truncate url; copy url on click and see full url on hover
(defun my-format-status-url (buffer)
  (let ((url (render-url (url buffer))))
    (spinneret:with-html-string
     (:a :class "button"
	 :title url
	 ;; :href (nyxt-url 'nyxt:copy-url)
	 (if (str:emptyp url)
	     (title buffer)
	     (format nil " ~a — ~a~@[ [~d]~]"
		     (str:prune 50 url :ellipsis "…")
		     (title buffer)
		     (when (find (url buffer) (remove buffer (buffer-list))
                                      :test #'url-equal :key #'url)
		       (id buffer))))))))

(defun format-status-tag (buffer)
  "Format the buffer tag, if any."
  (alexandria:if-let ((tag (show-buffer-tag buffer)))
    (format nil "[~d]" tag)
    ""))

#+nyxt-2
(defun my-format-status (window)
  (let ((buffer (current-buffer window)))
    (setf (style (status-buffer window)) (my-status-style))
    (spinneret:with-html-string
      (:div :id "container"
            (:div :id "controls"
                  (:raw (my-format-status-buttons)))
            (:div :class "arrow arrow-right"
                  :style "background-color:rgb(21,21,21);background-color:rgb(49,49,49)"  "")
            (:div :id "url"
                  (:raw
                   (format-status-load-status buffer)
                   (my-format-status-url buffer)))
	    (:div :id "tag"
		  (:raw
		   (format-status-tag buffer)))
            (:div :class "arrow arrow-left"
                  :style "background-color:rgb(21,21,21);background-color:rgb(49,49,49)" "")
            (:div :id "modes"
		  :title (nyxt::modes-string buffer)
		  "--")))))

#+nyxt-3
(defmethod format-status ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (setf (style status) (my-status-style))
    (spinneret:with-html-string
      (:div :id "container"
            (:div :id "controls" :class "arrow-right"
                  (:raw (my-format-status-buttons)))
            ;; (:div :class "arrow arrow-right"
            ;;       :style "background-color:rgb(21,21,21);background-color:rgb(49,49,49)"  "")
            (:div :id "url"
                  (:raw
                   (format-status-load-status status)
                   (my-format-status-url buffer)))
	    (:div :id "tag" :class "arrow-left"
		  (:raw
		   (format-status-tag buffer)))
            ;; (:div :class "arrow arrow-left"
            ;;       :style "background-color:rgb(21,21,21);background-color:rgb(49,49,49)" "")
            (:div :id "modes"
		  :title (nyxt::modes-string buffer)
		  "--")))))

#+nyxt-2
(define-configuration window 
  ((status-formatter #'my-format-status)))
