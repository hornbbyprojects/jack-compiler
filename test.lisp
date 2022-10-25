(in-package #:jack-compiler/test)

(defun tokenize-string (str)
  (let ((in (make-string-input-stream str)))
    (tokenize-stream in)))

(derive-unequiv jack-class-definition)
(derive-unequiv jack-class-variable-declaration)
(derive-unequiv jack-static-access)
(derive-unequiv jack-field-access)
(derive-unequiv jack-parameter-list)
(derive-unequiv jack-type-int)
(derive-unequiv jack-type-boolean)
(derive-unequiv jack-type-char)
(derive-unequiv jack-integer-literal)
(derive-unequiv jack-string-literal)
(derive-unequiv jack-let-statement)

(alexandria:define-constant +sample-class-definition+ "
class Foobar {
 static int foovar, foojar;
 field char foocar;
}
" :test 'equal)
(defun test-parse-class-definition ()
  (let ((class-definition (parse-class-definition (tokenize-string +sample-class-definition+)))
        (expected (make-instance 'jack-class-definition
                                 :name "Foobar"
                                 :subroutine-declarations nil
                                 :variable-declarations
                                 (list
                                  (make-instance 'jack-class-variable-declaration
                                                 :names '("foovar" "foojar")
                                                 :type +jack-type-int+
                                                 :access +jack-static-access+)
                                  (make-instance 'jack-class-variable-declaration
                                                 :names '("foocar")
                                                 :type +jack-type-char+
                                                 :access +jack-field-access+)))))
    (assert (not (unequiv class-definition expected)))))


(defun test-parse-parameter-list ()
  (let ((parameter-list (parse-parameter-list (tokenize-string "(int a, boolean b, int c)")))
        (expected (make-instance 'jack-parameter-list
                                 :parameters
                                 (list
                                  (cons "a" +jack-type-int+)
                                  (cons "b" +jack-type-boolean+)
                                  (cons "c" +jack-type-int+)))))
    (assert (not (unequiv parameter-list expected)))))

(alexandria:define-constant +sample-let-statement+ "
let x[100] = 200;"
  :test 'equal)

(defun test-parse-let-statement ()
  (let ((stmt (parse-let-statement (tokenize-string +sample-let-statement+)))
        (expected (make-instance 'jack-let-statement
                                 :name "x"
                                 :index (make-instance 'jack-integer-literal :value 100)
                                 :value (make-instance 'jack-integer-literal :value 200))))
    (assert (not (unequiv stmt expected)))))

(defun test-all ()
  (test-parse-class-definition)
  (test-parse-parameter-list)
  (test-parse-let-statement))
