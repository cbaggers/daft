(in-package #:daft)

(defvar *cube-stream* nil)
(defparameter *screen-height-in-game-units* 600f0)
(defvar *tasks-for-next-frame* nil)

(defun-g vert-game-units-to-gl ((pos :vec4)
                                (screen-height :float)
                                (screen-ratio :float))
  (let* ((game-v4 (/ pos
                     (v! (* screen-height screen-ratio)
                         screen-height
                         screen-height
                         1))))
    (+ (* game-v4 (v! 2 2 2 1))
       (v! 0 0 -10 0))))

(defun-g cube-vs ((vert g-pnt)
                  &uniform
                  (screen-height :float)
                  (screen-ratio :float)
                  (transform :mat4)
                  (size :vec2))
  (let* ((game-v4 (* (v! (pos vert) 1)
                     (v! size 1 1)))
         (transformed (* transform game-v4))
         (gv4 (vert-game-units-to-gl transformed
                                     screen-height
                                     screen-ratio)))
    (values gv4 (tex vert))))

(defun-g cube-fs ((uv :vec2)
                  &uniform
                  (sam :sampler-2d)
                  (uv-scale :vec2)
                  (uv-offset :vec2))
  (texture sam (+ (* uv uv-scale) uv-offset)))

(defpipeline-g simple-cube ()
  :vertex (cube-vs g-pnt)
  :fragment (cube-fs :vec2))

(defvar *god* nil)

(defvar *sdl2-pads* nil)

(defun init ()
  (unless *sdl2-pads*
    (init-pads '(0)))
  (unless *cube-stream*
    (destructuring-bind (vert-arr index-arr)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *cube-stream*
            (make-buffer-stream vert-arr :index-array index-arr)))))

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

(defvar *daft-frame-counter* 0)

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
