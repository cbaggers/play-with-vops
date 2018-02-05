;;;; play-with-vops.asd

(asdf:defsystem #:play-with-vops
  :description "Describe play-with-vops here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cffi)
  :components ((:file "package")
               (:file "vops")
               (:file "play-with-vops")))
