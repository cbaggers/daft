(in-package :daft)

;;------------------------------------------------------------

(defmacro define-god (values &body states)
  `(define-actor god ,values ,@states))

(define-god () (:main))

;;------------------------------------------------------------
