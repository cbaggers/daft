(in-package :daft)

;;------------------------------------------------------------

(defun hey (name)
  (loop :for arrays :being :the :hash-values :of *actors*
     :for x := (find name (actors-current arrays)
                     :test #'string=
                     :key #'debug-name)
     :when x :return x))

(defmacro as (actor &body body)
  `(let ((*self* ,actor))
     ,@body))

(defun spawn! (actor-kind-name pos
               &rest args &key &allow-other-keys)
  (as *god*
    (apply #'spawn actor-kind-name pos args)))

;;------------------------------------------------------------
