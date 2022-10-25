(named-readtables:in-readtable :interpol-syntax)
(in-package #:jack-compiler)


(defparameter *token-lookup-table* (make-hash-table :test 'equal))

(defclass token () ())

(defclass token-generator ()
  ((peek :initarg :peek) (drop :initarg :drop)))

(defgeneric drop-token (token-generator))

(defgeneric peek-token (token-generator))

(defun read-token (token-generator)
  (let ((ret (peek-token token-generator)))
    (drop-token token-generator)
    ret))

(defun maybe-read-token-type (type token-generator)
  (let ((token (peek-token token-generator)))
    (when (typep token type)
      (drop-token token-generator)
      token)))

(defun read-token-type (type token-generator)
  (let ((token (read-token token-generator)))
    (when (not (typep token type))
      (error #?"Expected token of type ${type}, got ${token}"))
    token))

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
     (comma . ",")
     (period . ".")
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

(defclass buffered-token-generator (token-generator)
  ((in-multiline-comment :initform nil :initarg :in-multiline-comment) (buffer :initform nil) (stream :initarg :stream) (stream-complete :initform nil)))

(defmethod initialize-instance :after ((instance buffered-token-generator) &key i)
  (declare (ignore i))
  (refill-buffer instance))

(alexandria:define-constant +inline-comment-regex+
  "/\\*.*?\\*/" :test 'equal)

(defun remove-inline-comments (line)
  (cl-ppcre:regex-replace-all +inline-comment-regex+ line ""))

(defun close-multiline-comments (line token-generator)
  (with-slots (in-multiline-comment) token-generator
    (if in-multiline-comment
        (multiple-value-bind (start end) (cl-ppcre:scan "\\*/" line)
          (declare (ignore start))
          (if end
              (progn
                (setf in-multiline-comment nil)
                (str:substring end (length line) line))
              ""))
        line)))
(defun open-multiline-comments (line token-generator)
  (with-slots (in-multiline-comment) token-generator
    (if (not in-multiline-comment)
      (multiple-value-bind (start end) (cl-ppcre:scan "/\\*" line)
        (declare (ignore end))
        (if start
            (progn
              (setf in-multiline-comment t)
              (str:substring 0 (1- start) line))
            line))
      line)))

(defun handle-multiline-comments (line token-generator)
  (let* ((line (remove-inline-comments line))
         (line (close-multiline-comments line token-generator))
         (line (open-multiline-comments line token-generator)))
    line))

(defun remove-single-line-comments (line)
  (cl-ppcre:regex-replace "//.*" line  ""))

(defun process-line (token-generator)
  (with-slots (stream buffer stream-complete in-multiline-comment) token-generator
    (unless stream-complete
      (let* ((next-line (handler-case
                            (read-line stream)
                          (end-of-file () (setf stream-complete t) ())))
             (next-line (handle-multiline-comments next-line token-generator))
             (next-line (remove-single-line-comments next-line))
             (next-line (add-whitespace-around-symbols next-line)))
        (when next-line
          (setf buffer (append buffer
                               (i:iterate
                                 (i:for word in (str:words next-line))
                                 (i:collecting (tokenize-word word))))))))))

(defmethod peek-token ((token-generator buffered-token-generator))
  (with-slots (buffer) token-generator
    (car buffer)))

(defun refill-buffer (token-generator)
  (with-slots (stream-complete buffer) token-generator
    (loop while (and (null buffer) (not stream-complete))
          do (process-line token-generator))))

(defmethod drop-token ((token-generator buffered-token-generator))
  (with-slots (buffer) token-generator
    (pop buffer)
    (when (null buffer)
      (refill-buffer token-generator))))

(defun tokenize-stream (stream)
  (make-instance 'buffered-token-generator :stream stream))
