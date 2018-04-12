(in-package :daft)

;;------------------------------------------------------------

(defclass actors ()
  ((current :initform (make-array 0 :adjustable t :fill-pointer 0)
            :initarg :current
            :type (array t (*))
            :accessor actors-current)
   (next :initform (make-array 0 :adjustable t :fill-pointer 0)
         :initarg :next
         :type (array t (*))
         :accessor actors-next)
   (collision-texture :initform nil
                      :initarg :collision-texture
                      :type (or null texture)
                      :accessor actors-collision-texture)
   (coll-sampler :initform nil
                 :initarg :coll-sampler
                 :type (or null sampler)
                 :accessor actors-coll-sampler)
   (coll-with :initform (make-hash-table)
              :accessor actors-coll-with)
   (coll-results :initform (make-hash-table)
                 :accessor actors-coll-results)))

(defun make-actors (&key current next collision-texture coll-sampler)
  (make-instance
   'actors
   :current (or current (make-array 0 :adjustable t :fill-pointer 0))
   :next (or next (make-array 0 :adjustable t :fill-pointer 0))
   :collision-texture collision-texture
   :coll-sampler coll-sampler))

(defun get-actor-kind (type)
  (or (gethash type *actors*)
      (let ((col (gen-collision-texture)))
        (setf (gethash type *actors*)
              (make-actors
               :collision-texture col
               :coll-sampler (sample col))))))

(defun gen-collision-texture ()
  (make-texture
   nil
   :dimensions (viewport-dimensions *world-viewport*)
   :element-type :uint8-vec4))

;;------------------------------------------------------------
