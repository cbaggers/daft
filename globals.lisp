(in-package #:daft)

;;------------------------------------------------------------

(defparameter *screen-height-in-game-units* 600f0)
(defvar *actors* (make-hash-table))
(defvar *blend-params* (make-blending-params))
(defvar *cube-stream* nil)
(defvar *daft-frame-counter* 0)
(defvar *god* nil)
(defvar *instanced-cube-stream* nil)
(defvar *max-actor-count* 40000)
(defvar *per-actor-c-data* nil)
(defvar *per-actor-data* nil)
(defvar *sdl2-pads* nil)
(defvar *self*)
(defvar *tasks-for-next-frame* nil)

;;------------------------------------------------------------
