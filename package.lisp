;;;; package.lisp

(uiop:define-package #:daft
  (:use #:cl :cepl :vari :rtg-math :nineveh :dirt
        :temporal-functions :cepl.skitter)
  (:import-from :alexandria
                :symbolicate
                :with-gensyms))
