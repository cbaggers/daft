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
                  (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g simple-cube ()
  :vertex (cube-vs g-pnt)
  :fragment (cube-fs :vec2))

(defun init ()
  (unless *cube-stream*
    (destructuring-bind (vert-arr index-arr)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *cube-stream*
            (make-buffer-stream vert-arr :index-array index-arr)))))

(defun now ()
  (/ (get-internal-real-time) 1000f0))

(defun step-engine ()
  (loop :for task :in *tasks-for-next-frame* :do
     (restart-case (funcall task)
       (continue () :report "Daft: Skip Task")))
  (setf *tasks-for-next-frame* nil)

  (setf (viewport-resolution (current-viewport))
        (surface-resolution
         (current-surface
          (cepl-context))))
  (clear)
  (update-actors)
  (swap)
  (decay-events))

(def-simple-main-loop daft (:on-start #'init)
  (step-engine))
