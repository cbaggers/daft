(in-package :daft)

;;------------------------------------------------------------

(defclass actor-kind ()
  ((name :initarg :name :reader name)
   (current :initform (make-array 0 :adjustable t :fill-pointer 0)
            :initarg :current
            :type (array t (*))
            :accessor this-frames-actors)
   (next :initform (make-array 0 :adjustable t :fill-pointer 0)
         :initarg :next
         :type (array t (*))
         :accessor next-frames-actors)
   (collision-fbo :initform nil
                  :initarg :collision-fbo
                  :type (or null fbo)
                  :accessor collision-fbo)
   (coll-sampler :initform nil
                 :initarg :coll-sampler
                 :type (or null sampler)
                 :accessor collision-sampler)
   (coll-with :initform (make-hash-table)
              :accessor actors-coll-with)
   (coll-results :initform (make-hash-table)
                 :accessor collision-results)))

(defmethod print-object ((obj actor-kind) stream)
  (with-slots (name) obj
    (format stream "#<ACTOR-KIND :NAME ~a>" name)))

(defun make-actor-kind ()
  (let ((tex (gen-collision-texture)))
    (make-instance
     'actors
     :current (make-array 0 :adjustable t :fill-pointer 0)
     :next (make-array 0 :adjustable t :fill-pointer 0)
     :collision-fbo (make-fbo (list 0 tex))
     :coll-sampler (sample tex))))

(defun get-actor-kind-by-name (type)
  (or (gethash type *actor-kinds*)
      (setf (gethash type *actor-kinds*) (make-actor-kind))))

(defun gen-collision-texture ()
  (make-texture
   nil
   :dimensions (viewport-dimensions *world-viewport*)
   :element-type :uint8-vec4))

;;------------------------------------------------------------
