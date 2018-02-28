(in-package :daft)

;;------------------------------------------------------------

(defun spawn! (actor-kind-name pos
               &rest args &key &allow-other-keys)
  (%spawn actor-kind-name (v! 0 0 0) 0f0 pos args
          *next-actors*))

(defmacro as (actor &body body)
  `(let ((*self* ,actor))
     ,@body))

(defun hey (name)
  (find name *current-actors* :test #'string=
        :key #'debug-name))

;;------------------------------------------------------------
