
(in-package #:jack-compiler)
(named-readtables:in-readtable :interpol-syntax)

(defgeneric to-xml (node out))

(defvar *indent-level* 0)

(defun write-line-indented (str out)
  (i:iterate
    (i:for _ from 1 to *indent-level*)
    (write-string #?"  " out))
  (write-line str out))

(defmacro tag (tag out &body body)
  `(progn
     (write-line-indented #?"<${,tag}>" ,out)
     (let ((*indent-level* (1+ *indent-level*)))
       ,@body)
     (write-line-indented #?"</${,tag}>" ,out)))

(defun tag-terminal (tag str out)
  (write-line-indented #?"<${tag}> ${str} </${tag}>" out))

(defun jsymbol (sym out)
  (tag-terminal "symbol" sym out))

(defun jkeyword (key out)
  (tag-terminal "keyword" key out))

(defun jidentifier (key out)
  (tag-terminal "identifier" key out))

;; Non-terminals
(defmethod to-xml ((node jack-class-definition) out)
  (with-slots (name variable-declarations subroutine-declarations) node
  (tag "class" out
    (jkeyword "class" out)
    (jidentifier name out)
    (jsymbol "{" out)
    (i:iterate
      (i:for subnode in variable-declarations)
      (to-xml subnode out))
    (i:iterate
      (i:for subnode in subroutine-declarations)
      (to-xml subnode out))
    (jsymbol "}" out))))

(defmethod to-xml ((node jack-if-statement) out)
  (with-slots (condition then-statements else-statements) node
      (tag "ifStatement" out
        (jkeyword "if" out)
        (jsymbol "(" out)
        (to-xml condition out)
        (jsymbol ")" out)
        (jsymbol "{" out)
        (tag "statements" out
          (i:iterate
            (i:for statement in then-statements)
            (to-xml statement out)))
        (jsymbol "}" out)
        (when else-statements
          (jkeyword "else" out)
          (jsymbol "{" out)
          (tag "statements" out
            (i:iterate
              (i:for statement in else-statements)
              (to-xml statement out)))
          (jsymbol "}" out)))))

(defmethod to-xml ((node jack-let-statement) out)
  (with-slots (name index value) node
    (tag "letStatement" out
      (jkeyword "let" out)
      (jidentifier name out)
      (when index
        (jsymbol "[" out)
        (to-xml index out)
        (jsymbol "]" out))
      (jsymbol "=" out)
      (to-xml value out)
      (jsymbol ";" out))))

(defmethod to-xml ((node jack-while-statement) out)
  (with-slots (condition statements) node
    (tag "whileStatement" out
      (jkeyword "while" out)
      (jsymbol "(" out)
      (to-xml condition out)
      (jsymbol ")" out)
      (jsymbol "{" out)
      (tag "statements" out
        (i:iterate
          (i:for statement in statements)
          (to-xml statement out)))
      (jsymbol "}" out))))

(defmethod to-xml ((node jack-do-statement) out)
  (with-slots (expression) node
    (tag "doStatement" out
      (jkeyword "do" out)
      (to-xml expression out)
      (jsymbol ";" out))))

(defmethod to-xml ((node jack-integer-literal) out)
  (with-slots (value) node
    (tag "IntegerLiteral" out
         (write-line-indented #?"${value}" out))))

(defmethod to-xml ((node jack-boolean-literal) out)
  (with-slots (value) node
    (tag "BooleanLiteral" out
      (write-line-indented #?"${value}" out))))

(defmethod to-xml ((node jack-binary-operator) out)
  (with-slots (left right) node
    (tag #?"${(type-of node)}" out
         (tag #?"Left" out
              (to-xml left out))
         (tag #?"Right" out
              (to-xml right out)))))

(defmethod to-xml ((node jack-variable-reference) out)
  (with-slots (name) node
    (tag "expression" out
      (tag "term" out
        (jidentifier name out)))))

(defmethod to-xml ((node jack-type-int) out)
  (jkeyword "int" out))

(defmethod to-xml ((node jack-type-char) out)
  (jkeyword "char" out))

(defmethod to-xml ((node jack-type-boolean) out)
  (jkeyword "boolean" out))

(defmethod to-xml ((node jack-subroutine-call) out)
  (with-slots (target-name subroutine-name arguments) node
    (when target-name
      (jidentifier target-name out)
      (jsymbol "." out))
    (jidentifier subroutine-name out)
    (jsymbol "(" out)
    (tag "expressionList" out
      (i:iterate
        (i:for argument in arguments)
        (when (not (i:first-iteration-p))
          (jsymbol "," out))
        (to-xml argument out)))
    (jsymbol ")" out)))

(defmethod to-xml ((node jack-class-variable-declaration) out)
  (with-slots (type names) node
    (tag "classVarDec" out
      (jkeyword "field" out)
      (to-xml type out)
      (i:iterate
        (i:for name in names)
        (when (not (i:first-iteration-p))
          (jsymbol "," out))
        (jidentifier name out))
      (jsymbol ";" out))))

(defmethod to-xml ((node jack-parameter-list) out)
  (with-slots (parameters) node
    (jsymbol "(" out)
    (tag "parameterList" out
      (i:iterate
        (i:for (name . type) in parameters)
        (unless (i:first-iteration-p)
          (jsymbol  "," out))
        (to-xml type out)
        (jidentifier name out)))
      (jsymbol ")" out)))

(defmethod subroutine-type ((node jack-class-constructor-declaration))
  "constructor")
(defmethod subroutine-type ((node jack-class-method-declaration))
  "method")
(defmethod subroutine-type ((node jack-class-function-declaration))
  "function")

(defmethod to-xml ((node jack-class-subroutine-declaration) out)
  (with-slots (name return-type parameter-list variable-declarations statements) node
    (tag "subroutineDec" out
      (jkeyword (subroutine-type node) out)
      (to-xml return-type out)
      (jidentifier name out)
      (to-xml parameter-list out)
      (tag "subroutineBody" out
        (jsymbol "{" out)
        (when variable-declarations
          (tag "variableDeclarations" out
            (i:iterate
              (i:for variable-declaration in variable-declarations)
              (to-xml variable-declaration out))))
        (tag "statements" out
          (i:iterate
            (i:for statement in statements)
            (to-xml statement out)))
        (jsymbol "}" out)))))


(defmethod to-xml ((node jack-return-statement) out)
  (with-slots (expression) node
    (tag "returnStatement" out
      (jkeyword "return" out)
      (when expression
        (to-xml expression out))
      (jsymbol ";" out))))

(defmethod to-xml ((node jack-negate) out)
  (with-slots (expr) node
    (tag "Negate" out
      (to-xml expr out))))

(defmethod to-xml ((node jack-this) out)
  (tag "expression" out
    (tag "term" out
      (jkeyword "this" out))))

(defmethod to-xml ((node jack-type-class) out)
  (with-slots (class-name) node
    (jidentifier class-name out)))

(defmethod to-xml ((node jack-type-void) out)
  (jkeyword "void" out))

(defun to-xml-string (node)
  (with-output-to-string (out)
    (to-xml node out)))
