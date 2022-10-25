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

(defclass literal-token (token)
  ((value :initarg :value :accessor literal-value)))

(defclass integer-literal-token (literal-token) ())

(defclass string-literal-token (literal-token) ())

(defmethod print-object ((object literal-token) out)
  (format out "#<literal (~a)>" (literal-value object)))

(defmethod print-object ((object string-literal-token) out)
  (format out "#<literal (\"~a\")>" (literal-value object)))
(defclass identifier (token)
  ((value :initarg :value :accessor identifier-value)))

(defmethod print-object ((object identifier) out)
  (format out "#<Identifier (~a)>" (identifier-value object)))

(defun build-symbols-class ()
  (i:iterate
    (i:for symbol in +all-symbols+)
    (assert (= (length symbol) 1))
    (i:collecting (elt symbol 0))))

(defun tokenize-word (word)
  (or
   (gethash word *token-lookup-table*)
   (make-instance 'identifier :value word)))

(defclass sm-token-generator (token-generator)
   ((next-token :initform nil :accessor peek-token)
    (stream :initarg :stream)))



(defun whitespace-char-p (char)
  (trivia:match char
    ((or
      #\Space
      #\Tab
      #\Return
      #\Newline) t)
    (otherwise nil)))

(defun skip-whitespace (stream)
  (i:iterate
    (i:for next-char next (peek-char nil stream))
    (unless (whitespace-char-p next-char)
      (i:terminate))
    (read-char stream)))


(defun finish-single-line-comment (stream)
  (i:iterate
    (i:for next-char next (read-char stream nil :eof))
    (when (or
           (equal #\Newline next-char)
           (equal :eof next-char))
      (i:terminate))))

(defun valid-identifier-char-p (char)
  (and
   (typep char 'character)
   (or
    (alphanumericp char)
    (equal #\_ char))))

(defun finish-identifier (stream)
  (i:iterate
    (i:for next-char next (peek-char nil stream nil :eof))
    (unless (valid-identifier-char-p next-char)
      (i:terminate))
    (read-char stream)
    (i:collecting next-char result-type string)))

(defun finish-multiline-comment (stream)
  (i:iterate
    (i:for next-char next (read-char stream))
    (when
        (and
         (equal next-char #\*)
         (equal (peek-char nil stream) #\/))
      (read-char stream)
      (i:terminate))))

(defun finish-string-literal (stream)
  (make-instance
   'string-literal-token
   :value
   (i:iterate
     (i:with escaped = nil)
     (i:for next-char next (read-char stream))
     (if escaped
         (setf escaped nil)
         (progn
           (when (equal next-char #\")
             (i:terminate))
           (when (equal next-char #\\)
             (setf escaped t)
             (i:next-iteration))))
     (i:collecting next-char result-type string))))

(defun finish-integer-literal (stream)
  (make-instance
   'integer-literal-token
   :value
   (parse-integer
    (i:iterate
      (i:for next-char next (peek-char nil stream nil :eof))
      (unless (and (typep next-char 'character) (digit-char-p next-char))
        (i:terminate))
      (read-char stream)
      (i:collecting next-char result-type string)))))

(defun next-token-from-stream (tg)
  (with-slots (stream) tg
    (skip-whitespace stream)
    (let* ((next-char (peek-char nil stream))
           (symbol (gethash (string next-char) *token-lookup-table*)))
      (cond
       ((equal next-char #\/)
          (let ((next-char (read-char stream))
                (subsequent (peek-char nil stream)))
            (declare (ignore next-char))
            (trivia:match subsequent
              (#\*
               (read-char stream)
               (finish-multiline-comment stream)
               :comment)
              (#\/
               (read-char stream)
               (finish-single-line-comment stream)
               :comment)
              (otherwise
               (make-instance 'forward-slash-token)))))
       ((equal next-char #\")
        (read-char stream)
        (finish-string-literal stream))
       ((digit-char-p next-char)
        (finish-integer-literal stream))
       (symbol
        (read-char stream)
        symbol)
       ((valid-identifier-char-p next-char) (tokenize-word (finish-identifier stream)))
       (t (error #?"While tokenizing, encountered char ${next-char}"))))))

(defun ensure-next-token (token-generator)
  (setf
   (peek-token token-generator)
   (handler-case
       (i:iterate
         (i:for next-token next (next-token-from-stream token-generator))
         (when (not (equal next-token :comment))
           (return next-token)))
     (end-of-file () ()))))

(defmethod initialize-instance :after ((instance sm-token-generator) &rest initargs)
  (declare (ignore initargs))
  (ensure-next-token instance))


(defmethod drop-token ((token-generator sm-token-generator))
  (setf (peek-token token-generator) nil)
  (ensure-next-token token-generator))

(defun tokenize-stream (stream)
  (make-instance 'sm-token-generator :stream stream))

(defun tokenize-string (str)
  (let ((in (make-string-input-stream str)))
    (tokenize-stream in)))

(defun list-tokens (tg)
  (i:iterate
    (i:for next-token next (read-token tg))
    (unless next-token
      (i:terminate))
    (i:collecting next-token)))
