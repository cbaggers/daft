(in-package :daft)

;;------------------------------------------------------------

(defun in-screen-p (&optional (actor *self*))
  ;; TODO: WAT! ↓↓↓↓↓↓
  (and (< (y (%pos actor)) 400)
       (> (y (%pos actor)) -400)
       (< (x (%pos actor)) 400)
       (> (x (%pos actor)) -400)))

(defun in-world-p (&optional (actor *self*))
  ;; TODO: WAT! ↓↓↓↓↓↓
  (and (< (y (%pos actor)) 1024)
       (> (y (%pos actor)) -1024)
       (< (x (%pos actor)) 1024)
       (> (x (%pos actor)) -1024)))

;;------------------------------------------------------------
