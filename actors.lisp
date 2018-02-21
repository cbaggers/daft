(in-package :daft)

(defvar *self*)

(defclass actor ()
  ((debug-name :reader debug-name)
   (pos :initform (v! 0 0 0) :initarg :pos :accessor %pos)
   (rot :initform 0f0 :initarg :rot :accessor %rot)
   (visual :initarg :visual)
   (next :initform nil)
   (dead :initform nil)))

(defmethod print-object ((actor actor) stream)
  (format stream "#<~a ~a>" (type-of actor)
          (slot-value actor 'debug-name)))

(defun radius (actor)
  (with-slots (visual) actor
    (/ (x (resolution (sampler-texture visual))) 2f0)))

(defvar *current-actors*
  (make-array 0 :adjustable t :fill-pointer 0))
(defvar *next-actors*
  (make-array 0 :adjustable t :fill-pointer 0))

(defgeneric update (actor))
(defgeneric init-actor (actor spawn-args))

(defgeneric %change-state (actor new-state))

(defun change-state (new-state)
  (%change-state *self* new-state))

(defun gen-state-funcs (name states local-var-names)
  (loop :for (state-name . body) :in states :collect
     (let ((state-func-name (symbolicate name :- state-name)))
       `(defun ,state-func-name (self)
          (with-slots ,local-var-names self
            (let ((*self* self))
              ,@body))))))

(defmacro define-god (values &body states)
  `(define-actor god ,values ,@states))

(defmacro define-actor (name values &body states)
  (assert states)
  (let* ((local-vars (remove-if #'keywordp values
                                :key #'first))
         (keyword-vars (remove-if-not #'keywordp values
                                      :key #'first))
         (local-var-names (mapcar #'first local-vars))
         (local-var-kwds (mapcar (lambda (name)
                                   (intern (symbol-name name) :keyword))
                                 local-var-names))
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
            (visual :initform ,(when visual
                                 `(load-tex ,visual)))
            ,@(loop :for (var-name var-val) :in local-vars
                 :for kwd :in local-var-kwds :collect
                 `(,var-name :initarg ,kwd))))

         (defmethod init-actor ((self ,name) spawn-args)
           (let ((*self* self)
                 (*spawn-into* *current-actors*)
                 (spawn-keys
                  (loop :for x :in spawn-args :by #'cddr
                     :collect x)))
             (declare (ignorable spawn-keys))
             ,@(loop :for (name val) :in local-vars :collect
                  `(unless (find ',name spawn-keys :test #'string=)
                     (setf (slot-value self ',name)
                           ,val))))
           self)
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
            (update-all-existing-actors
             ',name ,visual ',state-names
             (lambda (actor)
               (let ((*self* actor))
                 (list
                  ,@(loop :for (name val dont-change)
                       :in local-vars
                       :unless dont-change
                       :collect `(list ',name ,val)))))))
          *tasks-for-next-frame*)))))

(defun zero-out-next-actors ()
  (setf (fill-pointer *next-actors*) 0))

(defvar *spawn-into* nil)

(defun update-actors ()
  (let ((res (viewport-resolution (current-viewport)))
        (*spawn-into* *next-actors*))
    (with-setf (depth-test-function) nil
      (setf (fill-pointer *next-actors*) 0)
      (loop :for actor :across *current-actors* :do
         (copy-actor-state actor)
         (with-slots (next visual) actor
           (update next)
           (unless (slot-value next 'dead)
             (when visual
               (draw-actor actor res))
             (vector-push-extend next *next-actors*)))))
    (rotatef *current-actors* *next-actors*)))

(defvar *blend-params* (make-blending-params))

(defun draw-actor (actor res)
  (with-slots (visual pos rot) actor
    (let ((size (resolution
                 (sampler-texture visual))))
      (with-blending *blend-params*
        (map-g #'simple-cube *cube-stream*
               :screen-height *screen-height-in-game-units*
               :screen-ratio (/ (x res) (y res))
               :transform (m4:* (m4:translation pos)
                                (m4:rotation-z rot))
               :sam visual
               :size size)))))

(defun update-all-existing-actors (type-name
                                   new-visual
                                   new-valid-states
                                   gen-vars)
  (loop :for a :across *current-actors* :do
     (with-slots (visual state) a
       (when (typep a type-name)
         (loop :for (slot-name val)
            :in (funcall gen-vars a)
            :do (setf (slot-value a slot-name) val))
         (setf visual (when new-visual
                        (load-tex new-visual)))
         (when (not (find state new-valid-states))
           (setf state (first new-valid-states)))))))

;;------------------------------------------------------------
