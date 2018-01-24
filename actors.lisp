(in-package :daft)

(defclass actor ()
  ((pos :initform (v! 0 0 0) :initarg :pos)
   (rot :initform 0f0 :initarg :rot)))

(defvar *current-actor-state*
  (make-array 0 :element-type 'actor :adjustable t
              :fill-pointer 0))
(defvar *next-actor-state*
  (make-array 0 :element-type 'actor :adjustable t
              :fill-pointer 0))

(defun add-new-actor ()
  )

(defgeneric update (actor))

(defmacro define-actor (name values &body body)
  (let* ((local-vars (remove-if #'keywordp values
                                :key #'first))
         (local-var-names (mapcar #'first local-vars)))
    `(progn
       (defclass ,name (actor)
         ,(loop :for (var-name var-val) :in local-vars :collect
             `(,var-name :initform ,var-val
                         :initarg ,(intern (symbol-name var-name)
                                           :keyword))))
       (defmethod update ((self ,name))
         (symbol-macrolet ((x (x (slot-value self 'pos)))
                           (y (y (slot-value self 'pos))))
           (with-slots ,local-var-names self
             (let ((*self* self))
               ,@body)))))))

;;------------------------------------------------------------

(defvar *self*)

(defun spawn (actor-kind-name pos
              &rest args &key &allow-other-keys)
  (declare (ignore actor-kind-name pos args))
  nil)

(defun die ()
  nil)

(defun play-sound (sound-name)
  (declare (ignore sound-name))
  nil)

(defun touching-p (actor/s)
  (declare (ignore actor/s))
  nil)

(defun mouse-x ()
  0f0)

(defun move-forward (distance)
  (declare (ignore distance))
  nil)

(defun gamepad-button-a ())

(defun actors-in-range (distance &optional actor-kind)
  (declare (ignore distance actor-kind)))

;;------------------------------------------------------------
