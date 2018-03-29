(in-package #:daft)

;;------------------------------------------------------------

(defun init ()
  (unless *sdl2-pads*
    (init-pads '(0)))
  (unless *instanced-cube-stream*
    (init-actor-data)
    (destructuring-bind (vert-arr index-arr)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *instanced-cube-stream*
            (make-buffer-stream (list vert-arr
                                      (cons *per-actor-data* 1))
                                :index-array index-arr)))
    (setf *actors-fbo* (make-fbo 0))))

(defun init-pads (ids)
  (setf *sdl2-pads*  (make-array 10 :initial-element nil))
  (sdl2-game-controller-db:load-db)
  (loop :for id :in ids :do
     (unless (aref *sdl2-pads* id)
       (setf (aref *sdl2-pads* id)
             (sdl2:game-controller-open id))))
  (skitter.sdl2:enable-background-joystick-events))

(defun now ()
  (/ (get-internal-real-time) 1000f0))

(defun step-engine ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution
         (current-surface
          (cepl-context))))

  (clear)
  (update-actors)
  (swap)
  (decay-events))


(defun daft (nineveh::action &optional nineveh::frames)
  (ecase nineveh::action
    (:start
     (if (= *daft-frame-counter* 0)
         (progn
           (setf *daft-frame-counter* (or nineveh::frames -1))
           (format t "~%- starting ~a -" 'daft)
           (unwind-protect
                (progn
                  (when (cepl.lifecycle:uninitialized-p) (repl))
                  (let ((nineveh::on-start #'init))
                    (when nineveh::on-start (funcall nineveh::on-start)))
                  (loop :until (= *daft-frame-counter* 0) :do
                     (decf *daft-frame-counter* 1)
                     (livesupport:continuable
                       (step-host))
                     (livesupport:continuable
                       (tiny-time-manager:update))
                     (livesupport:continuable
                       (step-engine))))
             (setf *daft-frame-counter* 0)
             (format t "~%~%- stopping ~a -~%" 'daft)))
         (format t "~%~%- ~a is already running -~%" 'daft)))
    (:stop (setf *daft-frame-counter* (max 0 (or nineveh::frames 0))))))


;;
