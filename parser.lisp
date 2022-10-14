(named-readtables:in-readtable :interpol-syntax)

(in-package #:jack-compiler)

(defclass token-generator ()
  ((peek :initarg :peek) (drop :initarg :drop)))

(defun drop-token (token-generator)
  (with-slots (drop) token-generator
    (funcall drop)))

(defun peek-token (token-generator)
  (with-slots (peek) token-generator
    (funcall peek)))

(defun read-token (token-generator)
  (let ((ret (peek-token token-generator)))
    (drop-token token-generator)
    ret))

(defun read-token-type (type token-generator)
  (let ((token (read-token token-generator)))
    (assert (typep token type))
    token))

(defun string-to-token-generator (input)
  (let* ((words (str:words (add-whitespace-around-symbols input)))
         (peek (lambda () (and words (tokenize-word (car words)))))
         (drop (lambda () (pop words))))
    (make-instance 'token-generator :peek peek :drop drop)))

(defclass ast () ())

(defmacro defconstant-new (name value)
  `(when (not (boundp (quote ,name)))
     (defconstant ,name ,value)))

(defclass jack-class-definition (ast)
  ((name :initarg :name)
   (variable-declarations :initarg :variable-declarations)
   (subroutine-declarations :initarg :subroutine-declarations)))

(defclass jack-access-specificer (ast) ())
(defclass jack-static-access (jack-access-specificer) ())
(defconstant-new +jack-static-access+ (make-instance 'jack-static-access))
(defclass jack-field-access (jack-access-specificer) ())
(defconstant-new +jack-field-access+ (make-instance 'jack-field-access))
(defclass jack-class-variable-declaration (ast)
  ((access-specificer :initarg :access)
   (type :initarg :type)
   (names :initarg :names)))

(defclass jack-type (ast) ())
(defclass jack-type-int (jack-type) ())
(defconstant-new +jack-type-int+ (make-instance 'jack-type-int))
(defclass jack-type-char (jack-type) ())
(defconstant-new +jack-type-char+ (make-instance 'jack-type-char))
(defclass jack-type-boolean (jack-type) ())
(defconstant-new +jack-type-boolean+ (make-instance 'jack-type-boolean))
(defclass jack-type-class (jack-type) ((class-name :initarg :name)))

(defclass jack-subroutine-declaration (ast) ())

(defclass jack-parameter-list (ast) ())

(defclass jack-subroutine-body (ast) ())

(defclass jack-variable-declaration (ast) ())

(defclass jack-class-name (ast) ())

(defclass jack-subroutine-name (ast) ())

(defclass jack-variable-name (ast) ((name :initarg :name)))

(defun parse-type (token-generator)
  (let ((token (read-token token-generator)))
    (trivia:match token
      ((class int-token) +jack-type-int+)
      ((class char-token) +jack-type-char+)
      ((class boolean-token) +jack-type-boolean+)
      ((class identifier identifier-value) (make-instance 'jack-type-class :name identifier-value))
      (otherwise (error #?"${token} is not a type")))))

(defun parse-variable-name (token-generator)
  (make-instance 'jack-variable-name :name (read-token-type 'identifier token-generator)))

(defun maybe-parse-access-specifier (token-generator)
  (let* ((next-token (peek-token token-generator))
          (ret (trivia:match next-token
                 ((class static-token) +jack-static-access+)
                 ((class field-token) +jack-field-access+)
                 (otherwise nil))))
     (when ret
       (drop-token token-generator))
     ret))

(defun maybe-parse-class-variable-declaration (token-generator)
  (let ((access-specificer (maybe-parse-access-specifier token-generator)))
    (when access-specificer
      (let ((type (parse-type token-generator))
            (names nil))
        (push (parse-variable-name token-generator) names)
        (i:iterate
          (i:for maybe-comma next (peek-token token-generator))
          (if (typep maybe-comma 'comma-token)
              (progn
                (drop-token token-generator)
                (push (parse-variable-name token-generator) names))
              (i:terminate)))
        (read-token-type 'semicolon-token token-generator)
        (make-instance 'jack-class-variable-declaration :names names :type type :access access-specificer)))))


(defun maybe-parse-class-subroutine-declaration (token-generator)
  ())

(defun parse-class-name (token-generator)
    (identifier-value (read-token-type 'identifier token-generator)))

(defun parse-class (token-generator)
  (let ((name (parse-class-name token-generator))
        (variable-declarations nil)
        (subroutine-declarations nil))
    (read-token-type 'open-brace-token token-generator)
    (i:iterate
      (i:for decl next (maybe-parse-class-variable-declaration token-generator))
      (if (not decl)
          (i:terminate))
      (push decl variable-declarations))
    (i:iterate
      (i:for decl next (maybe-parse-class-subroutine-declaration token-generator))
      (if (not decl)
          (i:terminate))
      (push decl subroutine-declarations))
    (read-token-type 'close-brace-token token-generator)
    (make-instance 'jack-class-definition
                   :name name
                   :variable-declarations variable-declarations
                   :subroutine-declarations subroutine-declarations)))

(defun parse (token-generator)
  (let ((next-token (read-token token-generator)))
    (format t #?"Read ${next-token}\n")
    (trivia:match next-token
      ((class class-token) (parse-class token-generator))
      (otherwise (error #?"Unexpected token ${next-token}")))))

