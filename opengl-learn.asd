;;;; opengl-learn.asd

(asdf:defsystem #:opengl-learn
  :description "Translating C++ tutorial to SDL/CL"
  :author "Mateusz \"Malice\" Malisz"
  :license "MIT"
  :depends-on (#:sdl2
               #:cl-opengl
               #:rtg-math
               #:cffi)
  :serial t
  :components ((:file "package")
               (:file "opengl-learn")))
