(in-package #:daft)

;;------------------------------------------------------------

(defstruct-g per-actor-data
  (pos :vec3)
  (rot :float)
  (anim-frame :float))

(defstruct-g (collision-info :layout std-430)
  (ids (:int #.+max-actor-count+)))

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
                         1
                         1))))
    (* game-v4 (v! 2 2 1 1))))

(defun-g icube-vs ((vert g-pnt)
                   (data per-actor-data)
                   &uniform
                   (screen-height :float)
                   (screen-ratio :float)
                   (tile-count-x :int)
                   (tile-count-y :int)
                   (size :vec2)
                   (offset :vec2))
  (with-slots (pos rot anim-frame) data
    (multiple-value-bind (uv-scale uv-offset)
        (calc-uv-mod tile-count-x tile-count-y anim-frame)
      (let* ((vpos (* (pos vert) (v! size 1)))
             (sa (sin rot))
             (ca (cos rot))
             (vpos (v! (+ (* (x vpos) ca)
                          (* (y vpos) (- sa))
                          (- (x offset)))
                       (+ (* (x vpos) sa)
                          (* (y vpos) ca)
                          (- (y offset)))
                       (z vpos)))
             (game-v4 (v! (+ vpos
                             (v! (x pos)
                                 (y pos)
                                 (1- (* (z pos) 0.001))))
                          1))
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
  (let ((col (texture sam (+ (* uv uv-scale) uv-offset)))
        (nasty-discard-threshold 0.01))
    (when (< (w col) nasty-discard-threshold)
      (discard))
    col))

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
                       (tile-count-x :int)
                       (tile-count-y :int)
                       (size :vec2)
                       (world-size :vec2)
                       (collision collision-info :ssbo))
  (with-slots ((world-pos pos) rot anim-frame) data
    (let* ((vpos (* (pos vert) ;; -0.5 -> 0.5 cube
                    (v! size 1))) ;;-size/2 -> size/2 box
           (sa (sin rot))
           (ca (cos rot))
           (vpos (v! (+ (* (x vpos) ca) (* (y vpos) (- sa)))
                     (+ (* (x vpos) sa) (* (y vpos) ca))
                     (z vpos)))
           (vert-world-pos (+ (v! vpos 1)
                              (v! world-pos 0)))
           (unit-pos (/ vert-world-pos (v! world-size 1 1)))
           (clip-pos (* unit-pos (v! 2 2 1 1)))
           (coll-uv (+ (s~ unit-pos :xy) 0.5)))
      (atomic-min (aref (collision-info-ids collision)
                        gl-instance-id)
                  0)
      (multiple-value-bind (uv-scale uv-offset)
          (calc-uv-mod tile-count-x tile-count-y anim-frame)
        (values (v! (s~ clip-pos :xyz) 1f0)
                (tex vert)
                uv-scale
                uv-offset
                coll-uv
                (:flat gl-instance-id))))))

(defun-g coll-mask-fs ((uv :vec2)
                       (uv-scale :vec2)
                       (uv-offset :vec2)
                       (coll-uv :vec2)
                       (id :int)
                       &uniform
                       (sam :sampler-2d)
                       (coll-mask :sampler-2d)
                       (collision collision-info :ssbo))
  (let* ((our-color (texture sam (+ (* uv uv-scale) uv-offset)))
         (mask-col (texture coll-mask coll-uv))
         (threshold (vec4 0.01 0.01 0.01 0))
         (collision-col (* mask-col (w our-color)))
         (collision-val (step 0.01 (vmax collision-col))))
    (atomic-add (aref (collision-info-ids collision) id)
                (int collision-val))
    (vec4 collision-val)))

(defpipeline-g check-collisions-with ()
  :vertex (coll-mask-vs g-pnt per-actor-data)
  :fragment (coll-mask-fs :vec2 :vec2 :vec2 :vec2 :int))

;;------------------------------------------------------------
