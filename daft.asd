;;;; daft.asd

(asdf:defsystem #:daft
  :description "Describe daft here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:cepl.sdl2
               :rtg-math
               :nineveh
               :dirt
               :cepl.skitter.sdl2
               :temporal-functions)
  :components ((:file "package")
               (:file "images")
               (:file "daft")
               (:file "actors")
               (:file "collision")))
