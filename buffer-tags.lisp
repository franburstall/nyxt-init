;; quick and dirty tags for buffers
;;
;; Usage: enable buffer-tag-mode and then, for 0<=n<=9, hit
;; C-M-n to give the current-buffer tag n (removing it from any
;; previous owner)
;; C-n to switch to the buffer with tag n.

;; TODO:
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
  (let ((buf (current-buffer)))
    (dotimes (i 10)
      (when (eq buf (aref *tagged-buffers* i))
	(setf (aref *tagged-buffers* i) nil)))
    (setf (aref *tagged-buffers* tag) buf))
  (nyxt::print-status)
  (echo "Tag ~d set." tag))

(defun show-buffer-tag (buffer)
  "Return BUFFER's tag or nil."
  (position buffer *tagged-buffers*))

(defvar buffer-tag-mode-map (make-keymap "buffer-tag-mode-map"))

(defmacro make-key-binds (max)
  "Create key binds for set-buffer-tag-* and friends."
  (let (forms)
    (dotimes (i max)
      (push
       `(define-key buffer-tag-mode-map
	    (format nil "C-M-~d" ,i) (lambda-command ,(make-symbol (format nil "SET-BUFFER-TAG-~d" i)) ()
				       ,(format nil "Give current buffer tag ~d." i) (set-buffer-tag ,i)))
       forms)
      (push
       `(define-key buffer-tag-mode-map
	    (format nil "C-~d" ,i) (lambda-command ,(make-symbol (format nil "SWITCH-TO-BUFFER-~d" i)) ()
				     ,(format nil "Switch to buffer with tag ~d." i) (switch-buffer-by-tag ,i)))
       forms))
    `(progn ,@forms)))

(make-key-binds 10)

(define-mode buffer-tag-mode ()
  "Mode to allow quick and dirty buffer tagging.

For n from 0 to 9, do C-M-n to give the current buffer tag n
and C-n to then switch to it."
  ((keyscheme-map (keymaps:make-keyscheme-map
                   nyxt/keyscheme:cua buffer-tag-mode-map
                   nyxt/keyscheme:emacs buffer-tag-mode-map
                   nyxt/keyscheme:vi-normal buffer-tag-mode-map))))
