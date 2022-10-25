(in-package #:jack-compiler)
(named-readtables:in-readtable :interpol-syntax)

(defgeneric jompile (node context out)
  (:documentation "Turns a jack AST node into vm commands, written to the stream out.
declarations/definitions are turned into commands that define vm functions, sets the symbol table up, etc.
statements are turned into commands that perform the expected actions.
Expressions are turned into commands that push the calculated value onto the stack."))


(defclass function-information ()
  ((return-type :initarg :return-type)
   (is-method :initarg :is-method :reader function-is-method)))

(defclass function-lookup-table ()
  ((by-name :initform (make-hash-table :test 'equal))))

(defclass compilation-context ()
  ((symbol-tables :initarg :symbol-tables :initform nil)
   (function-table :initform nil)
   (current-class :initform nil)
   (label-counter :initform 0)))

(defun add-function-info (context name info)
  (with-slots (function-table) context
    (with-slots (by-name) function-table
      (setf (gethash name by-name) info))))

(defun function-lookup (context name)
  (with-slots (function-table current-class) context
    (with-slots (by-name) function-table
      (or
       (gethash name by-name)
       (error #?"Could not find function ${name} in ${current-class}")))))

(defun get-label (context &optional helpful-name)
  (with-slots (label-counter) context
    #?"label-${helpful-name}-${(incf label-counter)}"))

(defun symbol-lookup (context name)
  (with-slots (symbol-tables) context
    (i:iterate
      (i:for symbol-table in symbol-tables)
      (with-slots (by-name) symbol-table
        (let ((symbol-info (gethash name by-name)))
          (when symbol-info
            (return-from symbol-lookup symbol-info)))))))

(defun symbol-lookup-e (context name)
  (or
   (symbol-lookup context name)
   (error #?"No symbol found for ${name} in ${context}")))

(defun symbol-kind-to-segment (kind)(trivia:match kind
                                      (:static "static")
                                      (:field "this")
                                      (:argument "argument")
                                      (:var "local")))

(defun symbol-info-to-pop-command (symbol-info out)
  (with-slots (kind index) symbol-info
    (let ((segment (symbol-kind-to-segment kind)))
      (write-line #?"pop ${segment} ${index}" out))))

(defun symbol-info-to-push-command (symbol-info out)
  (with-slots (kind index) symbol-info
    (let ((segment (symbol-kind-to-segment kind)))
      (write-line #?"push ${segment} ${index}" out))))

(defun symbol-info-to-indexed-push-command (context symbol-info index-offset out)
  (symbol-info-to-push-command symbol-info out)
  (jompile index-offset context out)
  (write-line #?"add" out)
  (write-line #?"pop pointer 1" out)
  (write-line #?"push that 0" out))

(defun symbol-info-to-indexed-pop-command (context symbol-info index-offset out)
  (symbol-info-to-push-command symbol-info out)
  (jompile index-offset context out)
  (write-line #?"add" out)
  (write-line #?"pop pointer 1" out)
  (write-line #?"pop that 0" out))

(defmacro jompile-unary-op (classname vm-command)
  (alexandria:with-gensyms (node context out expr)
    `(defmethod jompile ((,node ,classname) ,context ,out)
       (with-slots ((,expr expr)) ,node
         (jompile ,expr ,context ,out)
         (write-line ,vm-command ,out)))))

(jompile-unary-op jack-negate "neg")
(jompile-unary-op jack-bitwise-negate "not")

(defmacro jompile-binary-op (classname vm-command)
  (alexandria:with-gensyms (node context out left right)
    `(defmethod jompile ((,node ,classname) ,context ,out)
       (with-slots ((,left left) (,right right)) ,node
         (jompile ,left ,context ,out)
         (jompile ,right ,context ,out)
         (write-line ,vm-command ,out)))))

(jompile-binary-op jack-plus "add")
(jompile-binary-op jack-minus "sub")
(jompile-binary-op jack-times "eq")
(jompile-binary-op jack-divide "gt")
(jompile-binary-op jack-plus "lt")
(jompile-binary-op jack-plus "and")
(jompile-binary-op jack-plus "or")




(defmethod jompile ((node jack-variable-reference) context out)
  (with-slots (name index) node
    (let ((symbol-info (symbol-lookup-e context name)))
      (if index
          (symbol-info-to-indexed-push-command context symbol-info index out)
          (symbol-info-to-push-command symbol-info out)))))

(defmethod jompile ((node jack-integer-literal) context out)
  (write-line #?"push constant ${(jack-literal-value node)}" out))

(defun jack-boolean-as-int (bool)
  (if bool
      -1
      0))

(defmethod jompile ((node jack-boolean-literal) context out)
  (write-line #?"push constant ${(jack-boolean-as-int (jack-literal-value node))}" out))
(defmethod jompile ((node jack-this) context out)
  (write-line #?"push pointer 0" out))

(defmethod jompile ((node jack-let-statement) context out)
  (with-slots (name index value) node
    (let ((symbol-info (symbol-lookup-e context name)))
      (jompile value context out)
      (if index
        (symbol-info-to-indexed-pop-command context symbol-info index out)
        (symbol-info-to-pop-command symbol-info out)))))

(defmethod jompile ((node jack-do-statement) context out)
  (with-slots (expression) node
    (jompile expression context out)
    (write-line #?"pop temp 0" out)))

(defmethod jompile ((node jack-return-statement) context out)
  (with-slots (expression) node
    (if expression
        (jompile expression context out)
        (write-line "push constant 0" out))
    (write-line "return" out)))

(defmethod jompile ((node jack-if-statement) context out)
  (let ((end-label (get-label context "if-end"))
        (then-label (get-label context "if-then")))
    (with-slots (condition then-statements else-statements) node
      (write-line #?"if-goto ${then-label}" out)
      (i:iterate
        (i:for statement in else-statements)
        (jompile statement context out))
      (write-line #?"goto ${end-label}" out)
      (write-line #?"label ${then-label}" out)
      (i:iterate
        (i:for statement in then-statements)
        (jompile statement context out))
      (write-line #?"label ${end-label}" out))))

(defmethod jompile ((node jack-subroutine-call) context out)
  (with-slots (current-class) context
    (with-slots (target-name subroutine-name arguments) node
      (let* ((is-method nil)
             (target-class
              (if (null target-name)
                  (let ((func-info (function-lookup context subroutine-name)))
                    (when (function-is-method func-info)
                      (write-line "push pointer 0" out)
                      (setf is-method t))
                    current-class)
                  (let* ((target-symbol-info (symbol-lookup context target-name)))
                    (if target-symbol-info
                        (let ((target-type (symbol-type target-symbol-info)))
                          (assert (typep target-type 'jack-type-class))
                          (with-slots ((target-class-name class-name)) target-type
                            (setf is-method t)
                            (symbol-info-to-push-command target-symbol-info out)
                            target-class-name))
                        target-name))))
              (arglength (+
                          (if is-method 1 0) ;; add the implicit this argument
                          (length arguments))))
          (i:iterate
            (i:for argument in arguments)
            (jompile argument context out))
          (write-line #?"call ${target-class}.${subroutine-name} ${arglength}" out)))))

(defmethod start-of-subroutine-code ((node jack-class-method-declaration) context func-symbol-table out)
  (with-slots (current-class) context
    (write-line "push argument 0" out)
    (write-line "pop pointer 0" out)
    (add-symbol func-symbol-table "this" (make-instance 'jack-type-class :name current-class) :argument)))

(defmethod start-of-subroutine-code ((node jack-class-constructor-declaration) context func-symbol-table out)
  (declare (ignore func-symbol-table))
  (with-slots (symbol-tables) context
    (let* ((class-symbol-table (car (last symbol-tables)))
           (sizeofme (i:iterate
                       (i:for (name symbol-info) in-hashtable (symbol-info-by-name class-symbol-table))
                       (when (equal (symbol-kind symbol-info) :field)
                         (i:counting name)))))
      (write-line #?"push constant ${sizeofme}" out)
      (write-line #?"call Memory.alloc" out)
      (write-line #?"pop pointer 0" out))))

(defmethod start-of-subroutine-code ((node jack-class-function-declaration) context func-symbol-table out)
  (declare (ignore context func-symbol-table node out)))

(defmethod jompile ((node jack-class-subroutine-declaration) context out)
  (with-slots (symbol-tables current-class) context
    (with-slots (name parameter-list variable-declarations statements) node
      (with-slots (parameters) parameter-list
        (let ((func-symbol-table (make-instance 'symbol-table)))
          (write-line #?"function ${current-class}.${name} ${(subroutine-variable-count node)}" out)
          (start-of-subroutine-code node context func-symbol-table out)
          (i:iterate
            (i:for (name . type) in parameters)
            (add-symbol func-symbol-table name type :argument))
          (i:iterate
           (i:for var-declaration in variable-declarations)
           (with-slots (names type) var-declaration
             (i:iterate
              (i:for name in names)
              (add-symbol func-symbol-table name type :var))))
          (push func-symbol-table symbol-tables)
          (i:iterate
            (i:for statement in statements)
            (jompile statement context out))
          (pop symbol-tables))))))

(defmethod jompile ((node jack-class-definition) context out)
  (with-slots ((class-name name) variable-declarations subroutine-declarations) node
    (with-slots (current-class symbol-tables function-table) context
      (let ((class-symbol-table (make-instance 'symbol-table))
            (class-function-table (make-instance 'function-lookup-table)))
        (push class-symbol-table symbol-tables)
        (setf current-class class-name)
        (setf function-table class-function-table)
        (i:iterate
          ;; We need the subroutine types and kinds all ready before we compile them, so do that first
          (i:for subroutine-declaration in subroutine-declarations)
          (with-slots (name return-type) subroutine-declaration
            (add-function-info
             context
             name
             (make-instance 'function-information
                            :return-type return-type
                            :is-method (typep subroutine-declaration 'jack-class-method-declaration)))))
        (i:iterate
          (i:for var-declaration in variable-declarations)
          (with-slots (access-specifier type (var-names names)) var-declaration
            (let ((kind (trivia:match access-specifier
                     ((class jack-static-access) :static)
                     ((class jack-field-access) :field))))
              (i:iterate
                (i:for var-name in var-names)
                (add-symbol class-symbol-table var-name type kind)))))
        (i:iterate
          (i:for subroutine-declaration in subroutine-declarations)
          (jompile subroutine-declaration context out))))))



(defun jompile-file (filename)
  (with-open-file (in filename)
    (with-output-to-string (out)
      (let ((context (make-instance 'compilation-context)))
        (jompile (parse-class-definition (tokenize-stream in)) context out)))))

(defun tompile (instr)
  (with-input-from-string (in instr)
    (with-output-to-string (out)
      (jompile (parse-class-definition (tokenize-stream in)) (make-instance 'compilation-context) out))))
