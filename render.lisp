(in-package #:daft)

;;------------------------------------------------------------

(defstruct-g per-actor-data
  (transform :mat4)
  (size :vec2)
  (uv-scale :vec2)
  (uv-offset :vec2))

(cffi:defcstruct foop
  (transform :mat4)
  (size :vec2)
  (uv-scale :vec2)
  (uv-offset :vec2))

(defun init-actor-data ()
  (unless *per-actor-data*
    (setf *per-actor-data*
          (make-gpu-array nil :element-type 'per-actor-data
                          :dimensions *max-actor-count*))
    (setf *per-actor-c-data*
          (make-c-array nil :element-type 'per-actor-data
                        :dimensions *max-actor-count*))))

;;------------------------------------------------------------

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

(defun-g icube-vs ((vert g-pnt)
                  (data per-actor-data)
                  &uniform
                  (screen-height :float)
                   (screen-ratio :float))
  (with-slots (transform size uv-offset uv-scale) data
    (let* ((game-v4 (* (v! (pos vert) 1)
                       (v! size 1 1)))
           (transformed (* transform game-v4))
           (gv4 (vert-game-units-to-gl transformed
                                       screen-height
                                       screen-ratio)))
      (values gv4
              (tex vert)
              uv-scale
              uv-offset))))

(defun-g icube-fs ((uv :vec2)
                  (uv-scale :vec2)
                  (uv-offset :vec2)
                  &uniform
                  (sam :sampler-2d))
  (texture sam (+ (* uv uv-scale) uv-offset)))

(defpipeline-g instanced-cube ()
  :vertex (icube-vs g-pnt per-actor-data)
  :fragment (icube-fs :vec2 :vec2 :vec2))

;;------------------------------------------------------------
