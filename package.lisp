;;;; package.lisp

(uiop:define-package #:daft
  (:use #:cl :cepl :vari :rtg-math :nineveh :dirt
        :temporal-functions :cepl.skitter :with-setf
        :cepl.defn)
  (:import-from :alexandria
                :symbolicate
                :with-gensyms))
