;;;; jack-compiler.asd

(asdf:defsystem #:jack-compiler
  :description "Describe jack-compiler here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (iterate str flexi-streams trivia cl-interpol)
  :components ((:file "package")
               (:file "tokenizer")
               (:file "parser" :depends-on ("tokenizer"))
               (:file "jack-compiler")))
