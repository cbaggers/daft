(in-package :daft)

;;------------------------------------------------------------

(defun hey (name)
  (loop :for arrays :being :the :hash-values :of *actor-kinds*
     :for x := (find name (this-frames-actors arrays)
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
