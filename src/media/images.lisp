(in-package #:daft)

(defvar *samplers* (make-hash-table :test #'equal))

(defun+ load-tex (rel-path)
  (declare (profile t))
  (or (gethash rel-path *samplers*)
      (let* ((path (funcall *get-local-path* *system-hack* rel-path))
             (tex (load-image-to-texture path)))
        (setf (gethash rel-path *samplers*)
              (sample tex :wrap :clamp-to-edge)))))
