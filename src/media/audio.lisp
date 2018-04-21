(in-package #:daft)

;;------------------------------------------------------------

(defvar *audio* (make-hash-table :test #'equal))

(defun ext (path)
  (let ((ext (string-downcase (subseq path (- (length path) 4)))))
    (cond
      ((string= ext ".wav") :wav)
      ((string= ext ".mp3") :mp3)
      (t (error "Invalid audio file extension for daft: ~s" ext)))))

(defun load-audio (rel-path)
  (or (gethash rel-path *audio*)
      (let* ((ext (ext rel-path))
             (path (asdf:system-relative-pathname *system-hack* rel-path))
             (audio (ecase ext
                      (:wav (sdl2-mixer:load-wav path))
                      (:mp3 (sdl2-mixer:load-music path)))))
        (setf (gethash rel-path *audio*) audio))))

;;------------------------------------------------------------

(defun init-audio ()
  (unless *audio-initialized*
    (sdl2-mixer:init :mp3)
    (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
    (sdl2-mixer:allocate-channels 1)
    (setf *audio-initialized* t)))

(defun play-sound (sound)
  (sdl2-mixer:play-channel 0 sound 0))
