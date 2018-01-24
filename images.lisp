(in-package #:daft)

(defvar *samplers* (make-hash-table :test #'equal))

(defun load-tex (rel-path)
  (or (gethash rel-path *samplers*)
      (let* ((path (asdf:system-relative-pathname :daft rel-path))
             (tex (load-image-to-texture path)))
        (setf (gethash rel-path *samplers*)
              (sample tex)))))
