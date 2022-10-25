(in-package #:jack-compiler/test)
(named-readtables:in-readtable :interpol-syntax)
;; Deep compare objects. Returns either nil to say they match or a string explaining why not!
;; Intended for tests

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun name-our-slots (slot-names nicknames)
    (i:iterate
      (i:for sn in slot-names)
      (i:for nn in nicknames)
      (i:collecting
       `(,nn ,sn)))))

(defmacro derive-unequiv (class-name)
  (sb-mop:finalize-inheritance (find-class class-name))
  (let ((slots (sb-mop:class-slots (find-class class-name)))
        (first-nicknames nil)
        (second-nicknames nil)
        (slot-names nil))
    (i:iterate
      (i:for slot in slots)
      (i:collecting (gensym) into a)
      (i:collecting (gensym) into b)
      (i:collecting (sb-mop:slot-definition-name slot) into c)
      (i:finally
       (setf first-nicknames a)
       (setf second-nicknames b)
       (setf slot-names c)))
    `(defmethod unequiv-same-class ((a ,class-name) (b ,class-name))
       (with-slots ,(name-our-slots slot-names first-nicknames) a
         (with-slots ,(name-our-slots slot-names second-nicknames) b
           (or
            ,@(i:iterate
                (i:for n1 in first-nicknames)
                (i:for n2 in second-nicknames)
                (i:collecting `(unequiv ,n1 ,n2)))))))))

(defgeneric unequiv-same-class (a b))

(defun unequiv-list (a b)
  (i:iterate
    (i:for a2 initially a then (cdr a2))
    (i:for b2 initially b then (cdr b2))
    (trivia:match (list (null a2) (null b2))
      ((list t t) (return nil))
      ((list nil nil) ())
      (otherwise (return #?"${a} and ${b} are different lengths")))
    (when (or
           (not (listp a2))
           (not (listp b2)))
      (return (unequiv a2 b2)))
    (i:for match-heads = (unequiv (car a2) (car b2)))
    (when match-heads
      (return match-heads))))


(defun unequiv (a b)
  (cond
   ((not (equal (type-of a) (type-of b))) #?"Type mismatch between ${a} and ${b}")
   ((listp a) (unequiv-list a b))
   ((or (null a) (stringp a) (numberp a))
    (if (equal a b)
        nil
        #?"${a} and ${b} not equal"))
   (t (unequiv-same-class a b))))

