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
               :temporal-functions
               :with-setf
               :sdl2-game-controller-db
               :alexandria)
  :components ((:file "package")
               (:file "src/utils")
               (:file "src/globals")
               (:file "src/names")
               (:file "src/media/images")
               (:file "src/render")

               (:file "src/actor-kind/actor-kind")

               (:file "src/actor/public-state")
               (:file "src/actor/actor")
               (:file "src/actor/update")
               (:file "src/actor/tiles")

               (:file "src/actor-kind/dispatch")
               (:file "src/actor-kind/update")
               (:file "src/actor-kind/define-actor-kind")
               (:file "src/actor-kind/god")

               (:file "src/init")
               (:file "src/main-loop")

               (:file "src/user-api/life-and-death")
               (:file "src/user-api/region-predicates")
               (:file "src/user-api/movement-and-orientation")
               (:file "src/user-api/input")
               (:file "src/user-api/frame-control")

               (:file "src/collision/collision")
               (:file "src/debug/debug")))
