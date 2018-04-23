(in-package #:daft)

;;------------------------------------------------------------

(defconstant +glyph-range+ 500)

(defstruct-g (glyph :layout :std-140)
  (atlas-x :uint)
  (atlas-y :uint)
  (atlas-w :uint)
  (atlas-h :uint)
  (min-x :float)
  (max-x :float)
  (min-y :float)
  (max-y :float)
  (advance :float))

(defstruct-g (glyph-set :layout :std-140)
  (glyphs (glyph #.+glyph-range+)))

;;------------------------------------------------------------

(defparameter *print-g-max-len* 128)

(defvar *text-layout* nil)
(defvar *text-stream* nil)

(defun init-text ()
  (unless *text-layout*
    (setf *text-layout*
          (make-gpu-array nil :dimensions *print-g-max-len*
                          :element-type :vec2)))
  (unless *text-stream*
    (setf *text-stream*
          (make-buffer-stream
           (list
            (make-gpu-array *quad-data*
                            :element-type :vec2
                            :dimensions 6)
            (cons *text-layout* 1))))))

;;------------------------------------------------------------

(defun-g median ((r :float) (g :float) (b :float))
  (max (min r g)
       (min (max r g) b)))

(defun-g msdf-sample ((uv :vec2)
                      (tile-offset-in-pixels :vec2)
                      (tile-dims-in-pixels :vec2)
                      (msdf :sampler-2d)
                      (sampler-dims :vec2)
                      (px-range :float)
                      (bg-color :vec4)
                      (fg-color :vec4))
  (let* ((nuv (/ (+ tile-offset-in-pixels
                    (* tile-dims-in-pixels uv))
                 sampler-dims))
         (sam (s~ (texture msdf nuv) :xyz))
         (msdf-unit (/ px-range tile-dims-in-pixels))
         (sig-dist (- (median (x sam) (y sam) (z sam))
                      0.5)))
    (multf sig-dist (dot msdf-unit (/ 0.5 (fwidth uv))))
    (let* ((opacity (clamp (+ sig-dist 0.5) 0.0 1.0)))
      (mix bg-color fg-color opacity))))

(defun-g msdf-blit-vs ((vert :vec2)
                       (layout :vec2))
  (values
   (v! (+ vert layout) 0 1)
   (+ (* vert 0.5) 0.5)))

(defun-g msdf-blit-fs ((uv :vec2)
                       &uniform
                       (msdf :sampler-2d)
                       (px-range :float)
                       (bg-color :vec4)
                       (fg-color :vec4)
                       (gset glyph-set :ubo))
  (let* ((code #.(char-code #\@))
         (glyph (aref (glyph-set-glyphs gset) code))
         (size2 (s~ (texture-size msdf 0) :xy)))
    (with-slots (atlas-x atlas-y atlas-w atlas-h) glyph
      (msdf-sample (v! (x uv) (- (y uv)))
                   (vec2 atlas-x
                         (- (y size2) atlas-y))
                   (vec2 atlas-w
                         atlas-h)
                   msdf
                   size2
                   px-range
                   bg-color
                   fg-color))))

(defpipeline-g msdf-blit (:points)
  (msdf-blit-vs :vec2 :vec2)
  (msdf-blit-fs :vec2))

(defun print-g (font string position)
  (error "nope")
  (assert (< (length string) *print-g-max-len*))
  (let ((total (x position)))
    (with-slots (glyph-info-ubo glyph-info atlas) font
      (with-gpu-array-as-c-array (carr *text-layout*)
        (loop
           :for i :from 0
           :for char :across string
           :for code := (char-code char)
           :for glyph := (aref-c glyph-info code)
           :for advance := (glyph-advance glyph)
           :do
           (setf (aref-c carr i)
                 (v! total (y position)))
           (incf total advance)))
      ;;
      ;; this will move soon
      (map-g #'msdf-blit
             *text-stream*
             :msdf atlas
             :px-range 1f0
             :bg-color (v! 0 0 0 0)
             :fg-color (v! 1 1 1 1)
             :gset glyph-info-ubo))))

;;------------------------------------------------------------
