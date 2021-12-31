;; quick and dirty tags for buffers
;;
;; Usage: enable buffer-tag-mode and then, for 0<=n<=9, hit
;; C-M-n to give the current-buffer tag n (removing it from any
;; previous owner)
;; C-n to switch to the buffer with tag n.

;; TODO:
;; - ensure buffer has only one tag (the most recent)?
;; - next/prev tagged buffer?  Not sure if this is really necessary.
;; - serialise on exit?  Would need to store buffer ids rather than buffers.

(in-package #:nyxt-user)

(defparameter *tagged-buffers* (make-array 10 :initial-element nil))

(defun switch-buffer-by-tag (tag)
  "Switch to buffer with tag TAG, if it exists."
  (let* ((buf (elt *tagged-buffers* tag))
	 (live-p (member buf (buffer-list))))
    (if (and buf live-p)
	(set-current-buffer buf)
	(progn  (when buf (setf (aref *tagged-buffers* tag) nil)) ; empty the tag
		(echo-warning (format nil "No live buffer with tag ~d." tag))))))

(defun set-buffer-tag (tag)
  "Assign TAG to current buffer."
  (setf (aref *tagged-buffers* tag) (current-buffer))
  (nyxt::print-status)
  (echo "Tag ~d set." tag))

(defun show-buffer-tag (buffer)
  "Return BUFFER's tag or nil."
  (position buffer *tagged-buffers*))

(defvar buffer-tag-mode-map (make-keymap "buffer-tag-mode-map"))

(defmacro make-key-binds (i)
  `(progn
     (define-key buffer-tag-mode-map
	 (format nil "C-M-~d" ,i) (make-command ,(make-symbol (format nil "set-buffer-tag-~d" i)) ()
				    ,(format nil "Give current buffer tag ~d." i) (set-buffer-tag ,i)))
     (define-key buffer-tag-mode-map
	 (format nil "C-~d" ,i) (make-command ,(make-symbol (format nil "switch-to-buffer-~d" i)) ()
				  ,(format nil "Switch to buffer with tag ~d." i) (switch-buffer-by-tag ,i)))))

;; there must be a good way to do this but
;; (dotimes (i 10) (make-key-binds i))
;; is apparently not it, sigh.
(make-key-binds 0)
(make-key-binds 1)
(make-key-binds 2)
(make-key-binds 3)
(make-key-binds 4)
(make-key-binds 5)
(make-key-binds 6)
(make-key-binds 7)
(make-key-binds 8)
(make-key-binds 9)

(define-mode buffer-tag-mode ()
  "Mode to allow quick and dirty buffer tagging.

For n from 0 to 9, do C-M-n to give the current buffer tag n
and C-n to then switch to it."
  ((keymap-scheme (keymap:make-scheme
                   scheme:cua buffer-tag-mode-map
                   scheme:emacs buffer-tag-mode-map
                   scheme:vi-normal buffer-tag-mode-map))))
