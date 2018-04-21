(in-package #:daft)

;;------------------------------------------------------------

(defun recieve-layout-event (event)
  (print "layout!"))

(defun recieve-size-event (event)
  (print "size!"))

(defvar *attached-to-window* nil)

(defun init-window-listener ()
  ;; (unless *attached-to-window*
  ;;   (skitter:listen-to (make-event-listener 'recieve-window-event)
  ;;                      (window 0)
  ;;                      :layout)
  ;;   (skitter:listen-to (make-event-listener 'recieve-size-event)
  ;;                      (window 0)
  ;;                      :layout)
  ;;   (setf *attached-to-window* t))
  )

(defvar *resize-listeners* nil)
(defvar *bah-last-res* (v! 0 0))

(defun bah-resize-hack ()
  (let ((res (surface-resolution (current-surface))))
    (when (not (v2:= res *bah-last-res*))
      (setf (viewport-resolution (current-viewport)) res)
      (map nil #'funcall *resize-listeners*))))

(defun add-window-resize-listener (listener)
  (if (or (functionp listener)
          (and listener (fboundp listener)))
      (unless (find listener *resize-listeners*)
        (push listener *resize-listeners*))
      (warn "~a is a bad thing to be a window-resize-listener"
            listener)))

(defun remove-window-resize-listener (listener)
  (setf *resize-listeners*
        (remove listener *resize-listeners*)))

;;------------------------------------------------------------
