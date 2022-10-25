(named-readtables:in-readtable :interpol-syntax)

(in-package #:jack-compiler)


(defun greedy-parse-repeatedly (maybe-parse token-generator)
  "Keep calling maybe-parse on token-generator until it returns nil, and return the results"
  (i:iterate
    (i:for result next (funcall maybe-parse token-generator))
    (when (null result)
      (i:terminate))
    (i:collecting result)))


(defclass ast () ())

(defclass jack-class-definition (ast)
  ((name :initarg :name)
   (variable-declarations :initarg :variable-declarations)
   (subroutine-declarations :initarg :subroutine-declarations)))

(defclass jack-access-specificer (ast) ())
(defclass jack-static-access (jack-access-specificer) ())
(defparameter +jack-static-access+ (make-instance 'jack-static-access))
(defclass jack-field-access (jack-access-specificer) ())
(defparameter +jack-field-access+ (make-instance 'jack-field-access))

(defclass jack-variable-declaration (ast)
  ((type :initarg :type)
   (names :initarg :names)))

(defclass jack-class-variable-declaration (jack-variable-declaration)
  ((access-specifier :initarg :access)))

(defclass jack-subroutine-variable-declaration (jack-variable-declaration) ())

(defclass jack-parameter-list (ast)
  ((parameters :initarg :parameters))) ;; An alist of parameters to types


(defclass jack-class-subroutine-declaration (ast)
  ((name :initarg :name)
   (return-type :initarg :return-type)
   (parameter-list :initarg :parameter-list)
   (variable-declarations :initarg :variable-declarations)
   (statements :initarg :statements)))

(defun parameter-count (sub-decl)
  (with-slots (parameter-list) sub-decl
    (with-slots (parameters) parameter-list
      (length parameters))))

(defun subroutine-variable-count (sub-decl)
  (with-slots (variable-declarations) sub-decl
    (length variable-declarations)))

(defclass jack-class-constructor-declaration (jack-class-subroutine-declaration) ())
(defclass jack-class-method-declaration (jack-class-subroutine-declaration) ())
(defclass jack-class-function-declaration (jack-class-subroutine-declaration) ())

(defclass jack-type (ast) ())
(defclass jack-type-int (jack-type) ())
(defparameter +jack-type-int+ (make-instance 'jack-type-int))
(defclass jack-type-char (jack-type) ())
(defparameter +jack-type-char+ (make-instance 'jack-type-char))
(defclass jack-type-boolean (jack-type) ())
(defparameter +jack-type-boolean+ (make-instance 'jack-type-boolean))
(defclass jack-type-class (jack-type) ((class-name :initarg :name)))

(defclass jack-type-void (jack-type) ((void-name :initarg :name)))
(defparameter +jack-type-void+ (make-instance 'jack-type-void))

(defclass jack-subroutine-body (ast) ())

(defun maybe-parse-type (token-generator)
  (let* ((token (peek-token token-generator))
         (ret (trivia:match token
                ((class int-token) +jack-type-int+)
                ((class char-token) +jack-type-char+)
                ((class boolean-token) +jack-type-boolean+)
                ((class void-token) +jack-type-void+) ;; Allow making void variables as well, because why not
                ((class identifier identifier-value) (make-instance 'jack-type-class :name identifier-value)))))
    (when ret
      (drop-token token-generator))
    ret))

(defun parse-type (token-generator)
  (or
   (maybe-parse-type token-generator)
   (error #?"${(peek-token token-generator)} is not a type")))

(defun maybe-parse-variable-name (token-generator)
  (let ((next-token (maybe-read-token-type 'identifier token-generator)))
    (when next-token
      (identifier-value next-token))))

(defun parse-variable-name (token-generator)
  (or
   (maybe-parse-variable-name token-generator)
   (error "Expected an identifier for variable name!")))

(defun maybe-parse-access-specifier (token-generator)
  (let* ((next-token (peek-token token-generator))
          (ret (trivia:match next-token
                 ((class static-token) +jack-static-access+)
                 ((class field-token) +jack-field-access+)
                 (otherwise nil))))
     (when ret
       (drop-token token-generator))
     ret))

(defun maybe-parse-type-and-name (token-generator)
  (let* ((type (maybe-parse-type token-generator))
        (name (and type (parse-variable-name token-generator))))
    (and type (cons name type))))

(defun maybe-parse-class-variable-declaration (token-generator)
  (let ((access-specifier (maybe-parse-access-specifier token-generator)))
    (when access-specifier
      (let ((type (parse-type token-generator))
            (names (parse-comma-separated #'maybe-parse-variable-name token-generator)))
        (read-token-type 'semicolon-token token-generator)
        (make-instance 'jack-class-variable-declaration :names names :type type :access access-specifier)))))

(defun parse-subroutine-name (token-generator)
  (identifier-value (read-token-type 'identifier token-generator)))

(defun parse-parameter-list (token-generator)
  (read-token-type 'open-paren-token token-generator)
  (let ((parameters (parse-comma-separated #'maybe-parse-type-and-name token-generator)))
    (read-token-type 'close-paren-token token-generator)
    (make-instance 'jack-parameter-list :parameters parameters)))


(defun maybe-parse-subroutine-variable-declaration (token-generator)
  (let ((var-token (maybe-read-token-type 'var-token token-generator)))
    (when var-token
      (let ((type (parse-type token-generator))
            (names (parse-comma-separated #'maybe-parse-variable-name token-generator)))
        (read-token-type 'semicolon-token token-generator)
        (make-instance 'jack-subroutine-variable-declaration :type type :names names)))))

(defun maybe-parse-class-subroutine-declaration (token-generator)
  (let* ((next-token (peek-token token-generator))
         (class-type (trivia:match next-token
                       ((class constructor-token)
                        'jack-class-constructor-declaration)
                       ((class method-token)
                        'jack-class-method-declaration)
                       ((class function-token)
                        'jack-class-function-declaration))))
    (when class-type
      (drop-token token-generator)
      (let* ((return-type (parse-type token-generator))
             (name (parse-subroutine-name token-generator))
             (parameter-list (parse-parameter-list token-generator))
             (variable-declarations
               (progn
                 (read-token-type 'open-brace-token token-generator)
                 (greedy-parse-repeatedly #'maybe-parse-subroutine-variable-declaration token-generator)))
             (statements
               (prog1
                   (greedy-parse-repeatedly #'maybe-parse-statement token-generator)
                 (read-token-type 'close-brace-token token-generator))))
        (make-instance class-type
                       :name name
                       :parameter-list parameter-list
                       :statements statements
                       :variable-declarations variable-declarations
                       :return-type return-type)))))

(defun parse-class-name (token-generator)
    (identifier-value (read-token-type 'identifier token-generator)))

(defun parse-class-definition (token-generator)
  (let* ((ct (read-token-type 'class-token token-generator))
         (name (parse-class-name token-generator))
         (open-brace (read-token-type 'open-brace-token token-generator))
         (variable-declarations
           (greedy-parse-repeatedly #'maybe-parse-class-variable-declaration  token-generator))
         (subroutine-declarations
           (greedy-parse-repeatedly #'maybe-parse-class-subroutine-declaration token-generator)))
    (declare (ignore open-brace ct))
    (read-token-type 'close-brace-token token-generator)
    (make-instance 'jack-class-definition
                   :name name
                   :variable-declarations variable-declarations
                   :subroutine-declarations subroutine-declarations)))

(defun parse (token-generator)
  (let ((next-token (peek-token token-generator)))
    (trivia:match next-token
      ((class class-token) (parse-class-definition token-generator))
      (otherwise (error #?"Unexpected token ${next-token}")))))

(defun parse-file (filename)
  (with-open-file (in filename)
    (parse (tokenize-stream in))))
