;;;; jack-compiler.asd

(asdf:defsystem #:jack-compiler
  :description "Describe jack-compiler here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (iterate str flexi-streams trivia cl-interpol alexandria)
  :components ((:file "package")
               (:file "tokenizer")
               (:file "expression" :depends-on ("tokenizer"))
               (:file "statement" :depends-on ("tokenizer" "expression"))
               (:file "parser" :depends-on ("tokenizer" "expression" "statement"))
               (:file "to-xml" :depends-on ("parser"))
               (:file "symbol-table" :depends-on ("parser"))
               (:file "write-vm")
               (:file "compiler" :depends-on ("parser" "symbol-table" "write-vm"))
               (:file "jack-compiler")))

(asdf:defsystem "jack-compiler/test"
  :depends-on (jack-compiler)
  :components ((:file "test" :depends-on ("equiv"))
               (:file "equiv")))
