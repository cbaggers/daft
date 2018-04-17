(in-package :daft)

;;------------------------------------------------------------

(defclass actor-kind ()
  ((name
    :initarg :name
    :reader name)
   (current
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :initarg :current
    :accessor this-frames-actors)
   (next
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :initarg :next
    :accessor next-frames-actors)
   (collision-fbo
    :initform nil
    :initarg :collision-fbo
    :accessor collision-fbo)
   (collision-sampler
    :initform nil
    :initarg :collision-sampler
    :accessor collision-sampler)
   (kinds-to-test-collision-with
    :initform (make-hash-table)
    :accessor kinds-to-test-collision-with)
   (collision-results
    :initform (make-hash-table)
    :accessor collision-results)))

(defmethod print-object ((obj actor-kind) stream)
  (with-slots (name) obj
    (format stream "#<ACTOR-KIND :NAME ~a>" name)))

(defun make-actor-kind ()
  (let ((tex (gen-collision-texture)))
    (make-instance
     'actor-kind
     :current (make-array 0 :adjustable t :fill-pointer 0)
     :next (make-array 0 :adjustable t :fill-pointer 0)
     :collision-fbo (make-fbo (list 0 tex))
     :collision-sampler (sample tex))))

(defun get-actor-kind-by-name (type)
  (or (gethash type *actor-kinds*)
      (setf (gethash type *actor-kinds*) (make-actor-kind))))

(defun gen-collision-texture ()
  (make-texture
   nil
   :dimensions (viewport-dimensions *world-viewport*)
   :element-type :uint8-vec4))

;;------------------------------------------------------------
