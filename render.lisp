(in-package #:daft)

;;------------------------------------------------------------

(defstruct-g per-actor-data
  (pos :vec3)
  (rot :float)
  (anim-frame :float))

(defstruct-g (collision-info :layout std-430)
  (actors (:int #.+max-actor-count+)))

(defun init-actor-data ()
  (unless *per-actor-data*
    (setf *per-actor-data*
          (make-gpu-array nil :element-type 'per-actor-data
                          :dimensions +max-actor-count+))
    (setf *per-actor-c-data*
          (make-c-array nil :element-type 'per-actor-data
                        :dimensions +max-actor-count+))))

;;------------------------------------------------------------

;; {TODO} make into gpu func
;;
(defun-g calc-uv-mod ((tile-count-x :int)
                      (tile-count-y :int)
                      (anim-frame :float))
  (let* ((anim-frame (floor anim-frame))
         (uv-scale (v! (/ 1f0 tile-count-x)
                      (/ 1f0 tile-count-y)))
         (uv-offset (v! (* (mod anim-frame tile-count-x)
                           (x uv-scale))
                        (* (floor (/ anim-frame tile-count-x))
                           (y uv-scale)))))
    (values uv-scale uv-offset)))

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
                   (screen-ratio :float)
                   (tile-count-x :int)
                   (tile-count-y :int)
                   (size :vec2))
  (with-slots (pos rot anim-frame) data
    (multiple-value-bind (uv-scale uv-offset)
        (calc-uv-mod tile-count-x tile-count-y anim-frame)
      (let* ((vpos (* (pos vert) (v! size 1)))
             (sa (sin rot))
             (ca (cos rot))
             (vpos (v! (+ (* (x vpos) ca) (* (y vpos) (- sa)))
                       (+ (* (x vpos) sa) (* (y vpos) ca))
                       (z vpos)))
             (game-v4 (+ (v! vpos 1)
                         (v! pos 0)))
             (gv4 (vert-game-units-to-gl game-v4
                                         screen-height
                                         screen-ratio)))
        (values gv4
                (tex vert)
                uv-scale
                uv-offset)))))

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

(defpipeline-g write-collision-map ()
  :vertex (icube-vs g-pnt per-actor-data)
  :fragment (icube-fs :vec2 :vec2 :vec2))

;;------------------------------------------------------------

(defun-g coll-mask-vs ((vert g-pnt)
                       (data per-actor-data)
                       &uniform
                       (screen-height :float)
                       (screen-ratio :float)
                       (tile-count-x :int)
                       (tile-count-y :int)
                       (size :vec2)
                       (world-size :vec2))
  (with-slots (pos rot anim-frame) data
    (multiple-value-bind (uv-scale uv-offset)
        (calc-uv-mod tile-count-x tile-count-y anim-frame)
      (let* ((vpos (* (pos vert) (v! size 1)))
             (sa (sin rot))
             (ca (cos rot))
             (vpos (v! (+ (* (x vpos) ca) (* (y vpos) (- sa)))
                       (+ (* (x vpos) sa) (* (y vpos) ca))
                       (z vpos)))
             (game-v4 (+ (v! vpos 1)
                         (v! pos 0)))
             (gv4 (vert-game-units-to-gl game-v4
                                         screen-height
                                         screen-ratio))
             (coll-uv (/ (s~ game-v4 :xy) world-size)))
    (values (v! (* vpos 100) 1)
                (tex vert)
                uv-scale
                uv-offset
                coll-uv)))))

(defun-g coll-mask-fs ((uv :vec2)
                       (uv-scale :vec2)
                       (uv-offset :vec2)
                       (coll-uv :vec2)
                       &uniform
                       (sam :sampler-2d)
                       (coll-mask :sampler-2d)
                       (collision collision-info :ssbo))
  (let ((our-color (texture sam (+ (* uv uv-scale) uv-offset)))
        (threshold (vec4 0.01 0.01 0.01 0)))
    (setf (aref (collision-info-actors collision) 0) 123)
    (setf (aref (collision-info-actors collision) 1) 123)
    (setf (aref (collision-info-actors collision) 2) 123)
    (discard)))

(defpipeline-g check-collisions-with ()
  :vertex (coll-mask-vs g-pnt per-actor-data)
  :fragment (coll-mask-fs :vec2 :vec2 :vec2 :vec2))

;;------------------------------------------------------------
