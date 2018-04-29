(in-package :daft)

;;------------------------------------------------------------

(defclass public-state ()
  ((pos :initform (v! 0 0 0) :initarg :pos)
   (rot :initform 0f0 :initarg :rot)))

(defun+ make-public-state (&optional start-pos creator depth)
  (declare (profile t))
  (let ((state (make-instance 'public-state)))
    (when (and start-pos creator)
      (let* ((creator-pos (%pos creator))
             (creator-rot (%rot creator))
             (offset (m3:*v (m3:rotation-z creator-rot)
                            (v! (x start-pos)
                                (y start-pos)
                                0))))
        (with-slots (pos rot) state
          (setf pos (v3:make (+ (x creator-pos) (x offset))
                             (+ (y creator-pos) (y offset))
                             depth)
                rot creator-rot))))
    state))

;;------------------------------------------------------------

(defun+ copy-actor-state (actor)
  (declare (profile t))
  (let ((src (slot-value actor 'current-public-state))
        (next (slot-value actor 'next-public-state)))
    (setf (slot-value next 'pos) (slot-value src 'pos))
    (setf (slot-value next 'rot) (slot-value src 'rot))))

;;------------------------------------------------------------
;; these are slow as hell

(defun+ %pos (actor)
  (declare (profile t))
  (slot-value
   (if (eq *self* actor)
       (slot-value actor 'next-public-state)
       (slot-value actor 'current-public-state))
   'pos))

(defun+ (setf %pos) (value actor)
  (declare (profile t))
  (setf (slot-value
         (if (eq *self* actor)
             (slot-value actor 'next-public-state)
             (slot-value actor 'current-public-state))
         'pos)
        value))

(defun+ %rot (actor)
  (declare (profile t))
  (slot-value
   (if (eq *self* actor)
       (slot-value actor 'next-public-state)
       (slot-value actor 'current-public-state))
   'rot))

(defun+ (setf %rot) (value actor)
  (declare (profile t))
  (setf (slot-value
         (if (eq *self* actor)
             (slot-value actor 'next-public-state)
             (slot-value actor 'current-public-state))
         'rot)
        value))

;;------------------------------------------------------------

(defn current-pos ((actor actor)) vec3
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (slot-value (slot-value actor 'current-public-state) 'pos))

(defn next-pos ((actor actor)) vec3
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (slot-value (slot-value actor 'next-public-state) 'pos))

(defn (setf current-pos) ((value vec3) (actor actor)) vec3
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (setf (slot-value (slot-value actor 'current-public-state) 'pos)
        value))

(defn (setf next-pos) ((value vec3) (actor actor)) vec3
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (setf (slot-value (slot-value actor 'next-public-state) 'pos)
        value))

(defn current-rot ((actor actor)) single-float
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (slot-value (slot-value actor 'current-public-state) 'rot))

(defn next-rot ((actor actor)) single-float
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (slot-value (slot-value actor 'next-public-state) 'rot))

(defn (setf current-rot) ((value single-float) (actor actor)) single-float
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (setf (slot-value (slot-value actor 'current-public-state) 'rot)
        value))

(defn (setf next-rot) ((value single-float) (actor actor)) single-float
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (setf (slot-value (slot-value actor 'next-public-state) 'rot)
        value))

;;------------------------------------------------------------
