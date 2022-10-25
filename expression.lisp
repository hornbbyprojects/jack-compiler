
(in-package #:jack-compiler)
(named-readtables:in-readtable :interpol-syntax)

(defclass jack-literal (ast)
  ((value :initarg :value :accessor jack-literal-value)))

(defclass jack-integer-literal (jack-literal) ())

(defun parse-integer-literal (token-generator)
  (make-instance 'jack-integer-literal
                 :value (constant-value (read-token-type 'integer-constant token-generator))))

(defclass jack-string-literal (jack-literal) ())
(defun parse-string-literal (token-generator)
  (make-instance 'jack-string-literal
                 :value (constant-value (read-token-type 'string-constant token-generator))))

(defclass jack-boolean-literal (jack-literal) ())
(defclass jack-null (ast) ())
(defclass jack-this (ast) ())

(defun maybe-parse-index (token-generator)
  (when (maybe-read-token-type 'open-bracket-token token-generator)
    (let ((index (parse-expression token-generator)))
      (read-token-type 'close-bracket-token token-generator)
      index)))

(defclass jack-variable-reference (ast)
  ((name :initarg :name)
   (index :initarg :index)))


(defun maybe-parse-term-starting-with-name (token-generator)
  (let ((name (maybe-parse-variable-name token-generator))
        (next-token (peek-token token-generator)))
    (when name
      (trivia:match next-token
        ((class period-token)
         (drop-token token-generator)
         (let ((subroutine-name (parse-variable-name token-generator))
               (arguments (progn
                            (read-token-type 'open-paren-token token-generator)
                           (parse-comma-separated #'parse-expression token-generator))))
           (prog1 (make-instance 'jack-subroutine-call
                                 :target-name name
                                 :subroutine-name subroutine-name
                                 :arguments arguments)
             (read-token-type 'close-paren-token token-generator))))
        ((class open-paren-token)
         (let ((arguments (progn
                            (read-token-type 'open-paren-token token-generator)
                            (parse-comma-separated #'parse-expression token-generator))))
           (prog1 (make-instance 'jack-subroutine-call
                                 :target-name nil
                                 :subroutine-name name
                                 :arguments arguments)
             (read-token-type 'close-paren-token token-generator))))
         (otherwise (make-instance 'jack-variable-reference
                                   :name name
                                   :index (maybe-parse-index token-generator)))))))

(defun parse-comma-separated (maybe-parse token-generator)
  (i:iterate
    (i:for next-token = (peek-token token-generator))
    (when (not (i:first-iteration-p))
      (if (typep next-token 'comma-token)
          (drop-token token-generator)
          (i:terminate)))
    (i:for result next (funcall maybe-parse token-generator))
    (when (not result)
      (i:terminate))
    (i:collecting result)))

(defclass jack-subroutine-call (ast)
  ((target-name :initarg :target-name)
   (subroutine-name :initarg :subroutine-name)
   (arguments :initarg :arguments)))

(defun maybe-parse-parened-expression (token-generator)
  (when (maybe-read-token-type 'open-paren-token token-generator)
    (prog1
        (parse-expression token-generator)
      (read-token-type 'close-paren-token token-generator))))

(defclass jack-binary-operator (ast)
  ((left :initarg :left)
   (right :initarg :right)))

(defclass jack-plus (jack-binary-operator) ())
(defclass jack-minus (jack-binary-operator) ())
(defclass jack-times (jack-binary-operator) ())
(defclass jack-divide (jack-binary-operator) ())
(defclass jack-bitwise-and (jack-binary-operator) ())
(defclass jack-bitwise-or (jack-binary-operator) ())
(defclass jack-less-than (jack-binary-operator) ())
(defclass jack-more-than (jack-binary-operator) ())
(defclass jack-equals (jack-binary-operator) ())

(defun maybe-parse-binary-operator (token-generator)
  (let ((ret (trivia:match (peek-token token-generator)
                 ((class plus-token) 'jack-plus)
                 ((class minus-token) 'jack-minus)
                 ((class asterisk-token) 'jack-times)
                 ((class forward-slash-token) 'jack-divide)
                 ((class ampersand-token) 'jack-bitwise-and)
                 ((class pipe-token) 'jack-bitwise-or)
                 ((class less-than-token) 'jack-less-than)
               ((class more-than-token) 'jack-more-than))))
    (when ret (drop-token token-generator))
    ret))

(defclass jack-unary-op (ast)
  ((expr :initarg :expr)))
(defclass jack-negate (jack-unary-op) ())

(defclass jack-bitwise-negate (jack-unary-op) ())

(defun make-single-token-class (class token-generator)
  (drop-token token-generator)
  (make-instance class))

(defun parse-term (token-generator)
  (trivia:match (peek-token token-generator)
    ((class minus-token) (drop-token token-generator)
     (make-instance 'jack-negate :expr (parse-term token-generator)))
    ((class tilde-token) (drop-token token-generator)
     (make-instance 'jack-bitwise-negate :expr (parse-term token-generator)))
    ((class integer-constant) (parse-integer-literal token-generator))
    ((class string-constant) (parse-string-literal token-generator))
    ((class true-token) (drop-token token-generator)
     (make-instance 'jack-boolean-literal :value t))
    ((class false-token) (drop-token token-generator)
     (make-instance 'jack-boolean-literal :value nil))
    ((class null-token) (make-single-token-class 'jack-null token-generator))
    ((class this-token) (make-single-token-class 'jack-this token-generator))
    (otherwise
     (or
      (maybe-parse-parened-expression token-generator)
      (maybe-parse-term-starting-with-name token-generator)))
    (x (error #?"Expected expression, got ${x}"))))

(defun parse-expression (token-generator)
  (let ((ret (parse-term token-generator)))
    (i:iterate
      (i:for binary-operator next (maybe-parse-binary-operator token-generator))
      (when (not binary-operator)
        (i:terminate))
      (i:for next-term next (parse-term token-generator))
      (setf ret (make-instance binary-operator :left ret :right next-term)))
    ret))
