(in-package :daft)

(defvar *timers* (make-hash-table))
(defvar *timer-fired-this-frame* (make-hash-table))

(defun+ add-timer (name interval)
  (declare (profile t))
  (setf (gethash name *timers*)
        (cons (get-internal-real-time)
              (* interval 1000)))
  name)

(defun+ remove-timer (name)
  (declare (profile t))
  (remhash name *timers*))

(defun+ time-p (name)
  (declare (profile t))
  (multiple-value-bind (data exists)
      (gethash name *timer-fired-this-frame*)
    (if exists
        data
        (let ((timer (gethash name *timers*)))
          (when timer
            (let* ((now (get-internal-real-time))
                   (time-p (> now (+ (car timer) (cdr timer)))))
              (when time-p
                (setf (car timer) (get-internal-real-time)))
              (setf (gethash name *timer-fired-this-frame*) time-p)
              time-p))))))

(defun+ clear-this-frames-timer-data ()
  (declare (profile t))
  (clrhash *timer-fired-this-frame*))
