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

(defun kind-class-name (kind-name)
  (intern (format nil "~a-KIND" kind-name)
          (symbol-package kind-name)))

(defun %define-actor (name values states)
  (multiple-value-bind (keyword-vars private-vars)
      (seperate-var-types values)
    (let* ((state-funcs (gen-state-funcs name states private-vars))
           (state-names (mapcar #'first states))
           (default-state (first state-names))
           (class-name (kind-class-name name))
           (static-p (null state-names)))
      (assert (every #'keywordp state-names))
      (destructuring-bind (&key visual tile-count noisy default-depth origin
                                collision-mask)
          (reduce #'append keyword-vars)
        (assert (member noisy '(t nil)))
        (assert (or (null default-depth) (numberp default-depth)))
        (assert (or (null origin) (and (listp origin) (= (length origin) 2))))
        (let ((default-depth (clamp 0f0
                                    100f0
                                    (float (or default-depth *default-depth*)
                                           0f0)))
              (origin (or origin '(0 0)))
              (tile-count (if (numberp tile-count)
                              (list tile-count 1)
                              (or tile-count '(1 1)))))
          (assert (and (listp tile-count)
                       (= 2 (length tile-count))
                       (every #'numberp tile-count)))
          `(progn
             (defclass ,class-name (actor-kind)
               ((name :initform ',name)
                (static-p :initform ,static-p)))
             ,(gen-actor-class name private-vars)
             ,(gen-spawn name private-vars noisy default-state static-p
                         default-depth)
             ,(unless static-p
                (gen-update-method name state-funcs state-names))
             ,@state-funcs
             ,@(gen-reinit-methods name class-name private-vars visual
                                   tile-count state-names static-p origin
                                   collision-mask)
             ,(gen-change-state name state-names)
             (push (lambda () (reinit-all-actors-of-kind ',name))
                   *tasks-for-next-frame*)))))))

(defun gen-actor-class (name private-vars)
  `(defclass ,name (actor)
     ((name :initform ',name)
      ,@(loop
           :for (var-name) :in private-vars
           :for kwd := (intern (symbol-name var-name) :keyword)
           :collect `(,var-name :initarg ,kwd)))))

(defun gen-spawn (name private-vars noisy default-state static-p
                  default-depth)
  (let* ((key-args (loop :for (name val) :in private-vars :collect
                      `(,name nil ,(gensym)))))
    `(defmethod spawn ((kwd-kind-name (eql ',name)) pos
                       &key ,@key-args)
       (declare (ignore kwd-kind-name))
       (let* ((scene *current-scene*)
              (creator *self*)
              (actor (make-instance ',name))
              (*self* actor)
              (kind-obj (get-actor-kind-by-name scene ',name)))
         (with-slots (current-public-state
                      next-public-state
                      debug-name
                      state
                      kind)
             actor
           (setf (dirty-p kind-obj) t)
           (setf state ,default-state)
           (setf kind kind-obj)
           (setf current-public-state (make-public-state pos
                                                         creator
                                                         ,default-depth))
           (setf next-public-state (make-public-state))
           (reinit-system-state actor)
           ,@(loop
                :for (slot-name val) :in private-vars
                :for (arg-name nil was-set) :in key-args
                :collect
                `(setf (slot-value actor ',slot-name)
                       (if ,was-set ,arg-name ,val)))
           (vector-push-extend actor ,(if static-p
                                          `(this-frames-actors kind-obj)
                                          `(next-frames-actors kind-obj)))
           (when ,noisy
             (format t "~%; ~a has spawned!" debug-name))
           actor)))))

(defgeneric reinit-kind (kind))

(defun gen-reinit-methods (name class-name private-vars visual
                           tile-count state-names static-p origin
                           collision-mask)
  (let ((new-len (reduce #'* tile-count)))
    `((defmethod reinit-kind ((kind ,class-name))
        (with-slots (visual
                     collision-mask
                     size tile-count anim-length origin
                     static-p dirty-p
                     current next)
            kind
          (when (and static-p (not ,static-p))
            ;; transform from static to dynamic
            (loop :for actor :across current :do
               (vector-push-extend actor next)))
          (setf origin (v! ',origin))
          (setf static-p ,static-p)
          (setf dirty-p t)
          (setf visual
                ,(when visual `(load-tex ,visual)))
          (setf collision-mask
                ,(if collision-mask
                     `(load-tex ,collision-mask)
                     'visual))
          (setf size (tile-size visual ',tile-count))
          (setf tile-count ',tile-count)
          (setf anim-length ,new-len))
        kind)
      (defmethod reinit-private-state ((actor ,name))
        (let ((*self* actor))
          ,@(loop
               :for (slot-name new-val dont-change) :in private-vars
               :unless dont-change
               :collect `(setf (slot-value actor ',slot-name)
                               ,new-val))))
      (defmethod reinit-system-state ((actor ,name))
        (let ((*self* actor))
          (with-slots (state tile-count anim-frame) actor
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

;; why would this vv reset a private variable?

(defun gen-change-state (name state-names)
  `(defmethod %change-state ((self ,name) new-state)
     (assert (member new-state ',state-names))
     (with-slots (state) self
       (setf state new-state))))

;;------------------------------------------------------------
