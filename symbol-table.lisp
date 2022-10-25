
(in-package #:jack-compiler)
(named-readtables:in-readtable :interpol-syntax)


(defclass symbol-information ()
  ((kind :initarg :kind :accessor symbol-kind) ;; :static, :field, :argument, or :var
   (type :initarg :type :accessor symbol-type)
   (index :initarg :index)))

(defclass symbol-table ()
  ((next-index-by-kind :initform (make-hash-table :test 'equal))
   (by-name :initform (make-hash-table :test 'equal) :reader symbol-info-by-name)))

(defun add-symbol (symbol-table name type kind)
  (with-slots (next-index-by-kind by-name) symbol-table
      (let* ((index (incf (gethash kind next-index-by-kind -1)))
             (info (make-instance 'symbol-information :kind kind :index index :type type)))
         (setf (gethash name by-name) info))))


