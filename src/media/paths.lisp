(in-package :daft)

;;------------------------------------------------------------

(defvar *get-local-path*
  (lambda (local-to path)
    (asdf:system-relative-pathname local-to path)))

(defun set-local-path-function (function)
  (setf *get-local-path* function))

;;------------------------------------------------------------
