(in-package :daft)

;;------------------------------------------------------------

(defun ensure-array-size (arr count)
  (let ((len (length arr)))
    (cond
      ((> len count)
       (setf (fill-pointer arr) count))
      ((< len count)
       (adjust-array arr count :initial-element nil
                     :fill-pointer t))))
  arr)

;;------------------------------------------------------------

(defmacro do-hash-vals (var hashtable &body body)
  (let ((key (gensym)))
    `(maphash
      (lambda (,key ,var)
        (declare (ignore ,key))
        ,@body)
      ,hashtable)))

;;------------------------------------------------------------

(defun now ()
  (/ (get-internal-real-time) 1000f0))

;;------------------------------------------------------------
