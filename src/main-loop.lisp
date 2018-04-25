(in-package :daft)

;;------------------------------------------------------------

(defvar *stepper*
  (make-stepper (seconds 1f0)))
(defvar *wip* 0)
(defvar *fps* 0)

(defun+ step-engine ()
  ;; Update FPS
  (incf *wip*)
  (when (funcall *stepper*)
    (setf *fps* *wip*
          *wip* 0))

  ;; I was lazy so we are checking window size
  ;; every frame
  (bah-resize-hack)
  (let ((scene *current-scene*)
        (res (viewport-resolution (current-viewport))))
    (setf (clear-color) (slot-value scene 'background-color))
    (clear)
    (update-actor-kinds scene)
    (draw-actor-kinds scene res)
    (livesupport:continuable
      (livesupport:update-repl-link))
    (mark-actors-clean scene)
    (ensure-god)
    (run-end-of-frame-tasks)
    (rotate-actor-kind-state scene)
    ;; (nineveh:draw-tex
    ;;  (collision-sampler
    ;;   (get-actor-kind-by-name *current-scene* 'bomber-chap::block-tile)))
    (swap)
    (clear-this-frames-timer-data)
    (decay-events)))

;;------------------------------------------------------------

(declaim (type (signed-byte 62) *daft-frame-counter*))
(defvar *daft-frame-counter* 0)
(defvar *frame-id* 0)

(defun+ daft (action &optional (frames -1))
  (ecase action
    (:start
     (if (= *daft-frame-counter* 0)
         (progn
           (setf *daft-frame-counter* frames)
           (format t "~%- starting ~a -" 'daft)
           (unwind-protect
                (progn
                  (when (cepl.lifecycle:uninitialized-p) (repl))
                  ;; kick host, hopefully we have a size now :p
                  (step-host)
                  (step-host)
                  (funcall #'init)
                  (loop :until (= *daft-frame-counter* 0) :do
                     (incf *frame-id* 1)
                     (decf *daft-frame-counter* 1)
                     (livesupport:continuable
                       (step-host))
                     (livesupport:continuable
                       (tiny-time-manager:update))
                     (livesupport:continuable
                       (step-engine))))
             (setf *daft-frame-counter* 0)
             (format t "~%~%- stopping ~a -~%" 'daft)))
         (format t "~%~%- ~a is already running -~%" 'daft)))
    (:stop (setf *daft-frame-counter* (max 0 (or frames 0))))))

;;------------------------------------------------------------

(defun+ run-end-of-frame-tasks ()
  (loop :for task :in *tasks-for-next-frame* :do
     (restart-case (funcall task)
       (continue () :report "Daft: Skip Task")))
  (setf *tasks-for-next-frame* nil))

;;------------------------------------------------------------
