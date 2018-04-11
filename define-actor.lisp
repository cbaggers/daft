(in-package :daft)

;;------------------------------------------------------------

(defun gen-state-funcs (name states local-var-names)
  (loop :for (state-name . body) :in states :collect
     (let ((state-func-name (symbolicate name :- state-name)))
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
         (local-var-kwds (mapcar (lambda (name)
                                   (intern (symbol-name name) :keyword))
                                 local-var-names))
         (state-funcs (gen-state-funcs name states local-var-names))
         (func-names (mapcar #'second state-funcs))
         (state-names (mapcar #'first states))
         (default-state (first state-names)))
    (assert (every #'keywordp state-names))
    (destructuring-bind (&key visual tile-count noisy)
        (reduce #'append keyword-vars)
      (assert (member noisy '(t nil)))
      (let ((tile-count (if (numberp tile-count)
                            (list tile-count 1)
                            (or tile-count '(1 1)))))
        (assert (and (listp tile-count) (= 2 (length tile-count))))
        `(progn
           (defclass ,name (actor)
             ((state :initform ,default-state)
              (visual :initform ,(when visual
                                   `(load-tex ,visual)))
              (tile-count :initform ',tile-count)
              (anim-length :initform ,(reduce #'* tile-count))
              (anim-frame :initform 0f0)
              ,@(loop :for (var-name var-val) :in local-vars
                   :for kwd :in local-var-kwds :collect
                   `(,var-name :initarg ,kwd))))

           (defmethod init-actor ((self ,name) spawn-args)
             (let ((*self* self)
                   (spawn-keys
                    (loop :for x :in spawn-args :by #'cddr
                       :collect x)))
               (declare (ignorable spawn-keys))
               (with-slots (visual size tile-count noisy) self
                 (when visual
                   (setf size
                         (v2:/ (resolution (sampler-texture visual))
                               (v! tile-count))))
                 (setf noisy ,noisy))
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
           (push
            (lambda ()
              (update-all-existing-actors
               ',name ,visual ',tile-count ',state-names ,noisy
               (lambda (actor)
                 (let ((*self* actor))
                   (list
                    ,@(loop :for (name val dont-change)
                         :in local-vars
                         :unless dont-change
                         :collect `(list ',name ,val)))))))
            *tasks-for-next-frame*))))))

;;------------------------------------------------------------
