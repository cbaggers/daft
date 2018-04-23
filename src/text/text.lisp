(in-package #:daft)

;;------------------------------------------------------------

(defvar *fonts* (make-hash-table))

(defclass font ()
  ((atlas
    :initarg :atlas
    :accessor atlas)
   (glyph-info-ubo
    :initarg :glyph-info-ubo
    :accessor glyph-info-ubo)
   (glyph-info
    :initarg :glyph-info
    :accessor glyph-info)))

(defun switch-extension (pathname new-ext)
  (make-pathname
   :host (pathname-host pathname)
   :device (pathname-device pathname)
   :directory (pathname-directory pathname)
   :name (pathname-name pathname)
   :type new-ext
   :version (pathname-version pathname)))

(defun write-glyph-info (glyph-info c-arr)
  (loop :for (ax ay aw ah minx maxx miny maxy a) :in glyph-info
     :for i :from 0
     :for elem = (aref-c c-arr i)
     :do (setf (glyph-atlas-x elem) ax
               (glyph-atlas-y elem) ay
               (glyph-atlas-w elem) aw
               (glyph-atlas-h elem) ah
               (glyph-min-x elem) (float minx 0f0)
               (glyph-max-x elem) (float maxx 0f0)
               (glyph-min-y elem) (float miny 0f0)
               (glyph-max-y elem) (float maxy 0f0)
               (glyph-advance elem) (float a 0f0))))

(defun %load-font (rel-path)
  (error "nope")
  (let* ((sampler (load-tex rel-path))
         (info-path (switch-extension
                     (asdf:system-relative-pathname *system-hack* rel-path)
                     "txt"))
         (*read-eval* nil)
         (glyph-info (with-open-file (str info-path)
                       (read str)))
         (glyph-set (make-ubo nil 'glyph-set)))
    ;;
    ;; load the info into the ubo
    (with-gpu-array-as-c-array (c-arr (ubo-data glyph-set))
      (let ((glyphs (glyph-set-glyphs (aref-c c-arr 0))))
        (write-glyph-info glyph-info glyphs)))
    ;;
    (make-instance
     'font
     :atlas sampler
     :glyph-info-ubo glyph-set
     :glyph-info glyph-info)))

(defun load-font (rel-path)
  (or (gethash rel-path *fonts*)
      (setf (gethash rel-path *fonts*)
            (%load-font rel-path))))

;;------------------------------------------------------------
