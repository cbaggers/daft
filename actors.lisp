(in-package :daft)

(defclass actor ()
  ((pos :initform (v! 0 0 0) :initarg :pos
        :accessor pos)
   (rot :initform 0f0 :initarg :rot
        :accessor rot)
   (visual :initarg :visual)
   (next :initform nil)
   (dead :initform nil)))

(defun radius (actor)
  (with-slots (visual) actor
    (/ (x (resolution (sampler-texture visual))) 2f0)))

(defvar *current-actors*
  (make-array 0 :adjustable t :fill-pointer 0))
(defvar *next-actors*
  (make-array 0 :adjustable t :fill-pointer 0))

(defgeneric update (actor))

(defgeneric %change-state (actor new-state))

(defun change-state (new-state)
  (%change-state *self* new-state))

(defun gen-state-funcs (name states local-var-names)
  (loop :for (state-name . body) :in states :collect
     (let ((state-func-name (symb name :- state-name)))
       `(defun ,state-func-name (self)
          (with-slots ,local-var-names self
            (let ((*self* self))
              ,@body))))))

(defmacro define-actor (name values &body states)
  (assert states)
  (let* ((local-vars (remove-if #'keywordp values
                                :key #'first))
         (keyword-vars (remove-if-not #'keywordp values
                                      :key #'first))
         (local-var-names (mapcar #'first local-vars))
         (state-funcs (gen-state-funcs name states local-var-names))
         (func-names (mapcar #'second state-funcs))
         (state-names (mapcar #'first states))
         (default-state (first state-names)))
    (assert (every #'keywordp state-names))
    (destructuring-bind (&key visual sprite-size)
        (reduce #'append keyword-vars)
      (declare (ignore sprite-size))
      `(progn
         (defclass ,name (actor)
           ((state :initform ,default-state)
            (visual :initform (load-tex ,visual))
            ,@(loop :for (var-name var-val) :in local-vars :collect
                 `(,var-name
                   :initform ,var-val
                   :initarg ,(intern (symbol-name var-name)
                                     :keyword)))))
         (defmethod update ((self ,name))
           (with-slots (state) self
             (case state
               ,@(loop :for state :in state-names
                    :for func :in func-names :collect
                    `(,state (,func self))))))
         ,@state-funcs
         (defmethod %change-state ((self ,name) new-state)
           (assert (member new-state ',state-names))
           (with-slots (state) self
             (setf state new-state)))
         (defmethod copy-actor-state ((src ,name))
           (with-slots (next) src
             ,@(loop :for slot :in (append '(pos rot visual)
                                           local-var-names)
                  :collect
                  `(setf (slot-value next ',slot)
                         (slot-value src ',slot)))))
         (push
          (lambda ()
            (let ((vars
                   (list
                    ,@(loop :for (name val dont-change)
                         :in local-vars
                         :unless dont-change
                         :collect `(list ',name ,val)))))
              (update-all-existing-actors
               ',name ,visual ',state-names vars)))
          *tasks-for-next-frame*)))))

(defun update-actors ()
  (let ((res (viewport-resolution (current-viewport))))
    (setf (fill-pointer *next-actors*) 0)
    (loop :for actor :across *current-actors* :do
       (copy-actor-state actor)
       (with-slots (next) actor
         (update next)
         (unless (slot-value next 'dead)
           (draw-actor actor res)
           (vector-push-extend next *next-actors*))))
    (rotatef *current-actors* *next-actors*)))

(defvar *blend-params* (make-blending-params))

(defun draw-actor (actor res)
  (let ((size (resolution
               (sampler-texture
                (slot-value actor 'visual)))))
    (with-blending *blend-params*
      (map-g #'simple-cube *cube-stream*
             :screen-height *screen-height-in-game-units*
             :screen-ratio (/ (x res) (y res))
             :transform (m4:translation (pos actor))
             :sam (slot-value actor 'visual)
             :size size))))

(defun update-all-existing-actors (type-name
                                   new-visual
                                   new-valid-states
                                   vars)
  (loop :for a :across *current-actors* :do
     (loop :for (slot-name val) :in vars :do
        (setf (slot-value a slot-name) val))
     (with-slots (visual state) a
       (when (typep a type-name)
         (setf visual (load-tex new-visual)))
       (when (not (find state new-valid-states))
         (setf state (first new-valid-states))))))

;;------------------------------------------------------------

(defvar *self*)

(defun spawn (actor-kind-name pos
              &rest args &key &allow-other-keys)
  (%spawn actor-kind-name (pos *self*) pos args
          *next-actors*))

(defun spawn! (actor-kind-name pos
               &rest args &key &allow-other-keys)
  (%spawn actor-kind-name (v! 0 0 0) pos args
          *current-actors*))

(defun %spawn (actor-kind-name parent-pos pos args
               into)
  (let* ((hack-name (intern (symbol-name actor-kind-name)
                            :daft))
         (actor (apply #'make-instance hack-name
                       args))
         (next (apply #'make-instance hack-name
                      args)))
    (setf (slot-value actor 'next) next)
    (setf (slot-value next 'next) actor)
    (setf (pos actor)
          (v3:+ parent-pos (v! (x pos) (y pos) 0)))
    (vector-push-extend actor into)
    actor))

(defun strafe (amount)
  ;; TODO: take rotation into account
  (incf (x (pos *self*)) amount))

(defun die ()
  (setf (slot-value *self* 'dead) t))

(defun play-sound (sound-name)
  (declare (ignore sound-name))
  nil)

(defun mouse-x ()
  0f0)

(defun move-forward (distance)
  (with-slots (pos rot) *self*
    (setf pos
          (v3:+ pos
                (v3:*s (v! (sin (radians rot)) (cos (radians rot)) 0)
                       (float distance 1f0))))))

(defun gamepad-button-a ())

(defun actors-in-range (distance &optional actor-kind)
  (declare (ignore distance actor-kind)))

(defun offscreen-p ()
  nil)

;;------------------------------------------------------------
