
(in-package #:jack-compiler)


(defparameter *token-lookup-table* (make-hash-table :test 'equal))

(defclass token () ())


(defclass keyword-token (token) ())

(defmacro define-subclasses (superclass name-or-conses)
  "Define subtypes of a token type that always correspond to a certain string.
Each name-or-cons should either be the string to be decoded (e.g. class will become class-token)
Or a cons cell with the string to be decoded and the name
(e.g. (open-paren . '(') will become open-paren-token)"
  (let ((token-lookups ()))
    `(progn
       ,@(i:iterate
           (i:for name-or-cons in name-or-conses)
           (i:for name = nil)
           (i:for token-string = nil)
           (if (consp name-or-cons)
               (setf name (car name-or-cons) token-string (cdr name-or-cons))
               (setf name name-or-cons token-string (string name-or-cons)))
           (i:for class-name = (intern (string-upcase (concatenate 'string (string name) "-token")) 'jack-compiler))
           (push (cons token-string class-name) token-lookups)
           (i:collecting
            `(defclass ,class-name (,superclass) ())))
       ,@(i:iterate
           (i:for (token-string . class-name) in token-lookups)
           (i:collecting
            `(setf (gethash (str:downcase ,token-string) *token-lookup-table*)
                   (make-instance (quote ,class-name))))))))

(setf *token-lookup-table* (make-hash-table :test 'equal))

(define-subclasses keyword-token
    (
     class
     constructor
     function
     method
     field
     static
     var
     int
     char
     boolean
     void
     true
     false
     null
     this
     let
     do
     if
     else
     while
     return))

(defclass symbol-token (token) ())

(defparameter +all-symbols+ nil)

(defmacro define-symbols (symbol-pairs)
  `(progn
     (define-subclasses symbol-token ,symbol-pairs)
     ,@(i:iterate
         (i:for (nil . symbol) in symbol-pairs)
         (i:collecting `(push ,symbol +all-symbols+)))))


(setf +all-symbols+ nil)

(define-symbols
    (
     (open-brace . "{")
     (close-brace . "}")
     (open-paren . "(")
     (close-paren . ")")
     (open-bracket . "[")
     (close-bracket . "]")
     (period . ".")
     (comma . ",")
     (semicolon . ";")
     (plus . "+")
     (minus . "-")
     (asterisk . "*")
     (forward-slash . "/")
     (ampersand . "&")
     (pipe . "|")
     (less-than . "<")
     (more-than . ">")
     (equals . "=")
     (tilde . "~")))

(defclass constant (token)
  ((value :initarg :value :accessor constant-value)))

(defclass integer-constant (constant) ())

(defclass string-constant (constant) ())

(defclass identifier (token)
  ((value :initarg :value :accessor identifier-value)))

(defun build-symbols-class ()
  (i:iterate
    (i:for symbol in +all-symbols+)
    (assert (= (length symbol) 1))
    (i:collecting (elt symbol 0))))


(defun add-whitespace-around-symbols (line)
  (cl-ppcre:regex-replace-all
   `(:sequence
     (:greedy-repetition 0 1 :whitespace-char-class)
     (:register (:char-class ,@(build-symbols-class)))
     (:greedy-repetition 0 1 :whitespace-char-class))
   line
   " \\1 "))


(defun tokenize-word (word)
  (or
   (gethash word *token-lookup-table*)
   (let ((register-matches (cadr (multiple-value-list (cl-ppcre:scan-to-strings
                                                       '(:sequence
                                                         #\"
                                                         (:register
                                                          (:greedy-repetition 0 NIL (:inverted-char-class #\")))
                                                         #\") word)))))
     (and
      (> (length register-matches) 0)
      (make-instance 'string-constant :value (elt register-matches 0))))
   (handler-case
       (make-instance 'integer-constant :value (parse-integer word))
     (parse-error () ()))
   (make-instance 'identifier :value word)))

(defun tokenize-stream (stream)
  (i:iterate outer
    (i:for line next (add-whitespace-around-symbols (read-line stream)))
    (i:for words = (str:words line))
    (i:iterate
      (i:for word in words)
      (i:in outer
            (i:collecting (tokenize-word word))))))
