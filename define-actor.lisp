(in-package :daft)

;;------------------------------------------------------------

(defmacro define-god (values &body states)
  (%define-actor 'god values states))

(defmacro define-actor (name values &body states)
  (assert (not (string= name :god)) ()
          "God can only be defined using the define-god macro")
  (%define-actor name values states))

;;------------------------------------------------------------

(defun seperate-var-types (vars)
  (values (remove-if-not #'keywordp vars :key #'first)
          (remove-if #'keywordp vars :key #'first)))

(defun %define-actor (name values states)
  (assert states)
  (multiple-value-bind (keyword-vars private-vars)
      (seperate-var-types values)
    (let* ((state-funcs (gen-state-funcs name states private-vars))
           (state-names (mapcar #'first states))
           (default-state (first state-names)))
      (assert (every #'keywordp state-names))
      (destructuring-bind (&key visual tile-count noisy)
          (reduce #'append keyword-vars)
        (assert (member noisy '(t nil)))
        (let ((tile-count (if (numberp tile-count)
                              (list tile-count 1)
                              (or tile-count '(1 1)))))
          (assert (and (listp tile-count)
                       (= 2 (length tile-count))
                       (every #'numberp tile-count)))
          `(progn
             ,(gen-actor-class name private-vars)
             ,(gen-spawn name private-vars noisy default-state)
             ,(gen-update-method name state-funcs state-names)
             ,@state-funcs
             ,@(gen-reinit-methods name private-vars visual tile-count
                                   state-names)
             ,(gen-change-state name state-names)
             (push (lambda () (update-all-existing-actors ',name))
                   *tasks-for-next-frame*)))))))

(defun gen-actor-class (name private-vars)
  `(defclass ,name (actor)
     ,(loop
         :for (var-name) :in private-vars
         :for kwd := (intern (symbol-name var-name) :keyword)
         :collect `(,var-name :initarg ,kwd))))

(defun gen-spawn (name private-vars noisy default-state)
  (let* ((key-args (loop :for (name val) :in private-vars :collect
                      `(,name nil ,(gensym)))))
    `(defmethod spawn ((kwd-kind-name (eql ',name)) pos
                       &key ,@key-args)
       (declare (ignore kwd-kind-name))
       (let* ((creator *self*)
              (actor (make-instance ',name))
              (*self* actor)
              (kind-obj (get-actor-kind ',name)))
         (with-slots (current-public-state
                      next-public-state
                      debug-name
                      state
                      kind)
             actor
           (setf state ,default-state)
           (setf kind kind-obj)
           (setf current-public-state (make-public-state pos creator))
           (setf next-public-state (make-public-state))
           (reinit-system-state actor)
           ,@(loop
                :for (slot-name val) :in private-vars
                :for (arg-name nil was-set) :in key-args
                :collect
                `(setf (slot-value actor ',slot-name)
                       (if ,was-set ,arg-name ,val)))
           (vector-push-extend actor (actors-next kind-obj))
           (when ,noisy
             (format t "~%; ~a has spawned!" debug-name))
           actor)))))

(defun gen-reinit-methods (name private-vars visual tile-count state-names)
  (let ((new-len (reduce #'* tile-count)))
    `((defmethod reinit-private-state ((actor ,name))
        (let ((*self* actor))
          ,@(loop
               :for (slot-name new-val dont-change) :in private-vars
               :unless dont-change
               :collect `(setf (slot-value actor ',slot-name)
                               ,new-val))))
      (defmethod reinit-system-state ((actor ,name))
        (let ((*self* actor))
          (with-slots (visual
                       state
                       tile-count
                       anim-length
                       anim-frame
                       size)
              actor
            (setf visual ,(when visual `(load-tex ,visual)))
            (setf size (tile-size visual ',tile-count))
            (setf tile-count ',tile-count)
            (setf anim-length ,new-len)
            (setf anim-frame (if (< anim-frame ,new-len)
                                 anim-frame
                                 0f0))
            (when (not (find state ',state-names))
              (setf state ,(first state-names)))))))))

(defun gen-update-method (name state-funcs state-names)
  (let ((func-names (mapcar #'second state-funcs)))
    `(defmethod update ((self ,name))
       (with-slots (state) self
         (case state
           ,@(loop :for state :in state-names
                :for func :in func-names :collect
                `(,state (,func self))))))))

(defun gen-state-funcs (name states private-vars)
  (loop :for (state-name . body) :in states :collect
     (let ((state-func-name (symbolicate name :- state-name))
           (private-var-names (mapcar #'first private-vars)))
       `(defun ,state-func-name (self)
          (with-slots ,private-var-names self
            (let ((*self* self))
              ,@body))))))

(defun gen-change-state (name state-names)
  `(defmethod %change-state ((self ,name) new-state)
     (assert (member new-state ',state-names))
     (with-slots (state) self
       (setf state new-state))))

;;------------------------------------------------------------
