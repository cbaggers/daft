(in-package #:daft)

(defvar *samplers* (make-hash-table :test #'equal))

(defun load-tex (rel-path)
  (or (gethash rel-path *samplers*)
      (let* ((path (asdf:system-relative-pathname *system-hack* rel-path))
             (tex (load-image-to-texture path)))
        (setf (gethash rel-path *samplers*)
              (sample tex :wrap :clamp-to-edge)))))
