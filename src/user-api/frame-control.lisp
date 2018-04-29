(in-package :daft)

;;------------------------------------------------------------

(defun+ next-frame (&optional range)
  (declare (profile t))
  (let ((start-frame (if range
                         (first range)
                         0))
        (anim-length (if range
                         (second range)
                         (slot-value (kind *self*) 'anim-length))))
    (with-slots (anim-frame) *self*
      (let ((frame (- anim-frame start-frame)))
        (setf anim-frame
              (float (+ (mod (+ (floor frame) 1)
                             anim-length)
                        start-frame)
                     0f0))))))

(defun+ last-frame (&optional range)
  (declare (profile t))
  (let ((start-frame (if range
                         (first range)
                         0))
        (anim-length (if range
                         (second range)
                         (slot-value (kind *self*) 'anim-length))))
    (with-slots (anim-frame) *self*
      (let ((frame (- anim-frame start-frame)))
        (setf anim-frame
              (float (+ (mod (- (floor frame) 1)
                             anim-length)
                        start-frame)
                     0f0))))))

(defun+ advance-frame (amount &optional range)
  (declare (profile t))
  (let* ((start-frame (if range
                          (first range)
                          0))
         (anim-length (if (and range (second range))
                          (- (second range) start-frame)
                          (slot-value (kind *self*) 'anim-length))))
    (with-slots (anim-frame) *self*
      (let ((frame (- anim-frame start-frame)))
        (setf anim-frame
              (float (+ (mod (+ frame amount)
                             anim-length)
                        start-frame)
                     0f0))))))

(defun+ set-frame (frame)
  (declare (profile t))
  (let ((anim-length (slot-value (kind *self*) 'anim-length)))
    (with-slots (anim-frame) *self*
      (setf anim-frame
            (float (max (min frame anim-length) 0)
                   0f0)))))

(defun+ get-frame ()
  (slot-value *self* 'anim-frame))

;;------------------------------------------------------------
