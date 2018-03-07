(in-package :daft)

;;------------------------------------------------------------

(defun spawn! (actor-kind-name pos
               &rest args &key &allow-other-keys)
  (%spawn actor-kind-name (v! 0 0 0) 0f0 pos args))

(defmacro as (actor &body body)
  `(let ((*self* ,actor))
     ,@body))

(defun hey (name)
  (loop :for arrays :being :the :hash-values :of *actors*
     :for x := (find name (actors-current arrays)
                     :test #'string=
                     :key #'debug-name)
     :when x :return x))

;;------------------------------------------------------------
