(in-package #:daft)

;;------------------------------------------------------------

(defclass sound ()
  ((chunk
    :initarg :chunk
    :accessor chunk)
   (duration
    :initarg :duration
    :accessor duration)))

(defclass track ()
  ((audio-stream
    :initarg :stream
    :accessor audio-stream)))

(defvar *audio* (make-hash-table :test #'equal))
(defvar *bitrate* 22050)

(defun ext (path)
  (let ((ext (string-downcase (subseq path (- (length path) 4)))))
    (cond
      ((string= ext ".wav") :wav)
      ((string= ext ".mp3") :mp3)
      (t (error "Invalid audio file extension for daft: ~s" ext)))))

(defun get-chunk-duration (chunk)
  ;; bah ,having problems remembering how cl-autowrap works :|
  (/ (float (plus-c:c-ref chunk sdl2-ffi:mix-chunk :alen) 0f0)
     #.(* *bitrate* 2))) ;; stereo assumed

(defgeneric load-internals (ext path)
  (:method ((ext (eql :wav)) path)
    (let* ((chunk (sdl2-mixer:load-wav path))
           (dur (get-chunk-duration chunk)))
      (make-instance 'sound
                     :chunk chunk
                     :duration dur)))
  (:method ((ext (eql :wav)) path)
    (make-instance 'track
                   :stream (sdl2-mixer:load-music path))))

(defun load-audio (rel-path)
  (or (gethash rel-path *audio*)
      (let* ((ext (ext rel-path))
             (path (asdf:system-relative-pathname *system-hack* rel-path))
             (audio (load-internals ext path)))
        (setf (gethash rel-path *audio*) audio))))

;;------------------------------------------------------------

(defun init-audio ()
  (unless *audio-initialized*
    (sdl2-mixer:init :mp3)
    (sdl2-mixer:open-audio *bitrate* :s16sys 1 1024)
    (sdl2-mixer:allocate-channels 1)
    (setf *audio-initialized* t)))

;;------------------------------------------------------------


(defclass audio-output ()
  ((name :initarg :name)
   (channel-offset :initarg :channel-offset)
   (priorities :initarg :priorities)
   (finish-times :initarg :finish-times)))

(defvar *outputs* (make-hash-table))

(defun update-audio-outputs (outputs)
  (clrhash *outputs*)
  (let ((total 0))
    (loop :for (name count) :in outputs :do
       (let ((priorities (make-array count :initial-element 0))
             (times (make-array count :initial-element 0)))
         (setf (gethash name *outputs*)
               (make-instance 'audio-output
                              :name name
                              :channel-offset total
                              :priorities priorities
                              :finish-times times)))
       (incf total count))
    (sdl2-mixer:allocate-channels total))
  (print "reallocated audio"))

(defun play-sound (output-name sound &optional (priority 1f0))
  (check-type sound sound)
  (let ((now (now)))
    (labels ((get-channel (output)
               (with-slots (channel-offset finish-times priorities) output
                 (loop
                    :for time :across finish-times
                    :for pri :across priorities
                    :for i :from 0
                    :when (or (< pri priority) (< time now))
                    :return
                    (progn
                      (setf (aref priorities i) priority)
                      (setf (aref finish-times i)
                            (+ (now) (duration sound)))
                      (+ channel-offset i)))))
             (play-through (output)
               (let ((channel (get-channel output)))
                 (when channel
                   (with-slots (chunk) sound
                     (sdl2-mixer:play-channel channel chunk 0))))
               sound))
      (let* ((output (gethash output-name *outputs*)))
        (if output
            (play-through output)
            (warn "No audio output named ~a is known" output-name))))))

(defmacro define-audio (&body outputs)
  `(push (lambda ()
           (update-audio-outputs
            ',(loop
                 :for (name key count) :in outputs
                 :do (assert (and (symbolp name)
                                  (eq key :channels)
                                  (numberp count)
                                  (> count 1)))
                 :collect (list name count))))
         daft::*tasks-for-next-frame*))
