(in-package #:daft)

;;------------------------------------------------------------

(defun+ init-collision ()
  (unless *ssbo*
    (setf *ssbo* (make-ssbo nil 'collision-info))))

;;------------------------------------------------------------

(defpipeline-g draw-actor-collision-mask ()
  :vertex (base-actor-vs :vec2 per-actor-data)
  :fragment (base-actor-fs :vec2 :vec2 :vec2))

;;------------------------------------------------------------

(defun-g coll-mask-vs ((vert :vec2)
                       (data per-actor-data)
                       &uniform
                       (tile-count-x :int)
                       (tile-count-y :int)
                       (size :vec2)
                       (world-size :vec2)
                       (collision collision-info :ssbo)
                       (offset-v2 :vec2))
  (with-slots ((world-pos pos) rot anim-frame) data
    (let* ((vert3 (v! (* vert 0.5) 0)) ;; quad is -1 to 1
           (vpos (* vert3 (v! size 1)))
           (sa (sin rot))
           (ca (cos rot))
           (vpos (v! (+ (* (x vpos) ca)
                        (* (y vpos) (- sa))
                        (- (x offset-v2)))
                     (+ (* (x vpos) sa)
                        (* (y vpos) ca)
                        (- (y offset-v2)))
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
                (+ (* vert 0.5) 0.5)
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
  (let* ((uv (v! (x uv) (- 1 (y uv))))
         (our-color (texture sam (+ (* uv uv-scale) uv-offset)))
         (mask-col (texture coll-mask coll-uv))
         (threshold (vec4 0.01 0.01 0.01 0))
         (collision-col (* mask-col (w our-color)))
         (collision-val (step 0.01 (vmax collision-col))))
    (atomic-add (aref (collision-info-ids collision) id)
                (int collision-val))
    (vec4 collision-val)))

(defpipeline-g check-collisions-with ()
  :vertex (coll-mask-vs :vec2 per-actor-data)
  :fragment (coll-mask-fs :vec2 :vec2 :vec2 :vec2 :int))

;;------------------------------------------------------------
