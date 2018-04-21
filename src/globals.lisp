(in-package #:daft)

;;------------------------------------------------------------

(defconstant +max-actor-count+ 20000)
(defparameter *screen-height-in-game-units* 600f0)
(defvar *blend-params* (make-blending-params))
(defvar *god* nil)
(defvar *sdl2-pads* nil)
(defvar *self*)
(defvar *tasks-for-next-frame* nil)
(defvar *ssbo* nil)
(defvar *default-depth* 100f0)
(defvar *current-scene* nil)
(defvar *audio-initialized* nil)
(defvar *system-hack* :daft)

;;------------------------------------------------------------
