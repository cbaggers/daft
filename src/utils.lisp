(in-package :daft)

;;------------------------------------------------------------

(defun+ ensure-array-size (arr count)
  (declare (profile t))
  (let ((len (length arr)))
    (cond
      ((> len count)
       (setf (fill-pointer arr) count))
      ((< len count)
       (adjust-array arr count :initial-element nil
                     :fill-pointer t))))
  arr)

;;------------------------------------------------------------

(defmacro do-hash-keys (var hashtable &body body)
  (let ((val (gensym)))
    `(maphash
      (lambda (,var ,val)
        (declare (ignore ,val))
        ,@body)
      ,hashtable)))

(defmacro do-hash-vals (var hashtable &body body)
  (let ((key (gensym)))
    `(maphash
      (lambda (,key ,var)
        (declare (ignore ,key))
        ,@body)
      ,hashtable)))

;;------------------------------------------------------------

(defun+ now ()
  (/ (get-internal-real-time) 1000f0))

;;------------------------------------------------------------

(cffi:defcfun (memcpy "memcpy") :pointer
  (destination-pointer :pointer)
  (source-pointer :pointer)
  (byte-length :long))

(cffi:defcfun (memset "memset") :pointer
  (ptr :pointer)
  (val :int)
  (byte-length :long))
