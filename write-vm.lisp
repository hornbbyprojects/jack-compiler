(in-package #:jack-compiler)
(named-readtables:in-readtable :interpol-syntax)

(defun write-pop (segment index out)
  (write-line #?"pop ${segment} ${index}" out))

(defun write-push (segment index out)
  (write-line #?"push ${segment} ${index}" out))

(defun write-pop-pointer-this (out)
  (write-pop "pointer" 0 out))

(defun write-push-pointer-this (out)
  (write-push "pointer" 0 out))

(defun write-pop-pointer-that (out)
  (write-pop "pointer" 1 out))

(defun write-push-pointer-that (out)
  (write-push "pointer" 1 out))

(defun write-add (out)
  (write-line "add" out))

(defun write-neg (out)
  (write-line "neg" out))

(defun write-not (out)
  (write-line "not" out))

(defun write-return (out)
  (write-line "return" out))

(defun write-goto (label out)
  (write-line #?"goto ${label}" out))

(defun write-if-goto (label out)
  (write-line #?"if-goto ${label}" out))

(defun write-label (label out)
  (write-line #?"label ${label}" out))

(defun write-push-constant (constant out)
  (if (>= constant 0)
      (write-push "constant" constant out)
      (progn
        (write-push "constant" (- constant) out)
        (write-neg out))))

(defun write-call (target arg-count out)
  (write-line #?"call ${target} ${arg-count}" out))

(defun write-function (name variable-count out)
  (write-line #?"function ${name} ${variable-count}" out))
