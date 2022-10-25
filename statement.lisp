
(in-package #:jack-compiler)


(defclass jack-statement (ast) ())
(defclass jack-let-statement (jack-statement)
  ((name :initarg :name)
   (index :initarg :index)
   (value :initarg :value)))

(defun parse-let-statement (token-generator)
  (read-token-type 'let-token token-generator)
  (let ((name (parse-variable-name token-generator))
        (index (maybe-parse-index token-generator))
        (eq (read-token-type 'equals-token token-generator))
        (value (parse-expression token-generator)))
    (declare (ignore eq))
    (read-token-type 'semicolon-token token-generator)
    (make-instance 'jack-let-statement :name name :index index :value value)))

(defclass jack-if-statement (jack-statement)
  ((condition :initarg :condition)
   (then-statements :initarg :then-statements)
   (else-statements :initarg :else-statements)))

(defun parse-if-statement (token-generator)
  (read-token-type 'if-token token-generator)
  (read-token-type 'open-paren-token token-generator)
  (let ((condition (prog1
                       (parse-expression token-generator)
                     (read-token-type 'close-paren-token token-generator)))
        (then-statements (prog2
                             (read-token-type 'open-brace-token token-generator)
                             (greedy-parse-repeatedly #'maybe-parse-statement token-generator)
                             (read-token-type 'close-brace-token token-generator)))
        (else-statements
          (when (maybe-read-token-type 'else-token token-generator)
            (prog2
                (read-token-type 'open-brace-token token-generator)
                (greedy-parse-repeatedly #'maybe-parse-statement token-generator)
              (read-token-type 'close-brace-token token-generator)))))
    (make-instance 'jack-if-statement
                   :condition condition
                   :then-statements then-statements
                   :else-statements else-statements)))

(defclass jack-while-statement (jack-statement)
  ((condition :initarg :condition)
   (statements :initarg :statements)))

(defun parse-while-statement (token-generator)
  (read-token-type 'while-token token-generator)
  (read-token-type 'open-paren-token token-generator)
  (let ((condition (prog1
                      (parse-expression token-generator)
                     (read-token-type 'close-paren-token token-generator)))
        (statements (prog2
                             (read-token-type 'open-brace-token token-generator)
                             (greedy-parse-repeatedly #'maybe-parse-statement token-generator)
                             (read-token-type 'close-brace-token token-generator))))
    (make-instance 'jack-while-statement
                   :condition condition
                   :statements statements)))

(defclass jack-do-statement (jack-statement)
  ((expression :initarg :expression)))

(defun parse-do-statement (token-generator)
  (read-token-type 'do-token token-generator)
  (let ((expression (maybe-parse-term-starting-with-name token-generator)))
    (prog1
        (make-instance 'jack-do-statement
                       :expression expression)
      (read-token-type 'semicolon-token token-generator))))

(defclass jack-return-statement (jack-statement)
  ((expression :initarg :expression)))

(defun parse-return-statement (token-generator)
  (read-token-type 'return-token token-generator)
  (if (maybe-read-token-type 'semicolon-token token-generator)
      (make-instance 'jack-return-statement :expression nil)
      (prog1
          (make-instance 'jack-return-statement :expression (parse-expression token-generator))
        (read-token-type 'semicolon-token token-generator))))


(defun maybe-parse-statement (token-generator)
  (trivia:match (peek-token token-generator)
    ((class let-token) (parse-let-statement token-generator))
    ((class if-token) (parse-if-statement token-generator))
    ((class while-token) (parse-while-statement token-generator))
    ((class do-token) (parse-do-statement token-generator))
    ((class return-token) (parse-return-statement token-generator))))
