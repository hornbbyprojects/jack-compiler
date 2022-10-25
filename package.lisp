;;;; package.lisp

(defpackage #:jack-compiler
  (:local-nicknames (i iterate))
  (:export
   #:parse-parameter-list
   #:parse-class-definition
   #:parse
   #:parse-let-statement
   #:tokenize-stream
   #:jack-let-statement
   #:jack-integer-literal
   #:jack-string-literal
   #:jack-class-variable-declaration
   #:jack-class-definition
   #:jack-static-access
   #:+jack-static-access+
   #:jack-field-access
   #:+jack-field-access+
   #:jack-parameter-list
   #:jack-type-int
   #:+jack-type-int+
   #:jack-type-boolean
   #:+jack-type-boolean+
   #:jack-type-char
   #:+jack-type-char+)
  (:use #:cl))

(defpackage #:jack-compiler/test
  (:local-nicknames (i iterate))
  (:export #:test-parse-class-definition #:test-parse-parameter-list #:test-all)
  (:use #:cl #:jack-compiler))
