(in-package :daft)

;;------------------------------------------------------------

(defun+ in-screen-p ()
  (declare (profile t))
  ;; TODO: WAT! ↓↓↓↓↓↓
  (let ((pos (next-pos *self*)))
    (and (< (y pos) 400)
         (> (y pos) -400)
         (< (x pos) 400)
         (> (x pos) -400))))

(defun+ in-world-p ()
  (declare (profile t))
  ;; TODO: WAT! ↓↓↓↓↓↓
  (let ((pos (next-pos *self*)))
    (and (< (y pos) 1024)
         (> (y pos) -1024)
         (< (x pos) 1024)
         (> (x pos) -1024))))

;;------------------------------------------------------------
