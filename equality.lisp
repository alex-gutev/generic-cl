;;;; equality.lisp
;;;;
;;;; Copyright 2018 Alexander Gutev
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :generic-cl.impl)

(defgeneric equalp (a b)
  (:documentation
   "Generic equality comparison function. Returns true if objects A
    and B are equal."))

(defmethod equalp ((a number) (b number))
  "Numeric equality comparison method. Returns true if A and B
   represent the same numeric value, compared using CL:=."

  (cl:= a b))

(defmethod equalp ((a character) (b character))
  "Character equality comparison method. Returns true if A and B
   represent the same character, compared using CL:CHAR=."

  (cl:char= a b))

(defmethod equalp ((a cons) (b cons))
  "CONS equality comparison method. Returns true if the CAR of A is
   equal to the CAR of B (compared using EQUALP) and the CDR of A is
   equal to the CDR of B (compared using EQUALP)."

  ;; A recursive solution would have been very elegant however it
  ;; requires that full TCO is supported by the implementation.

  (do ((a a (cdr a))
       (b b (cdr b)))
      ((not (and (listp a) (listp b))) (equalp a b))
    (unless (equalp (car a) (car b))
      (return nil))))

(defmethod equalp ((a vector) (b vector))
  "Array equality comparison method. Returns true if both arrays are
   of the same length and each element of A is equal (by EQUALP) to
   the corresponding element of B."

  (and (cl:= (length a) (length b)) (every #'equalp a b)))

(defmethod equalp ((a hash-table) (b hash-table))
  "Hash-table comparison method. Returns true if both hash-tables have
   the same number of entries, and the value corresponding to each key
   in A is equal (by EQUALP) to the value corresponding to the same
   key in B."

  (and (cl:= (hash-table-count a) (hash-table-count b))
       (iter (for (key a-value) in-hashtable a)
             (always
              (multiple-value-bind (b-value in-hash?) (gethash key b)
                (and in-hash?
                 (equalp a-value b-value)))))))

(defmethod equalp (a b)
  "Default equality comparison method. Returns true if objects A and B
   are the same object, compared using CL:EQ."

  (cl:eq a b))


(defun = (first &rest rest)
  "Returns true if each object in REST is equal, by EQUALP, to FIRST."

  (every (curry #'equalp first) rest))

(defun /= (first &rest rest)
  "Returns true if at least one object in REST is not equal, by
   EQUALP, to FIRST."

  (notevery (curry #'equalp first) rest))


(define-compiler-macro = (&whole form first &rest rest)
  (declare (ignore form))
  `(and (equalp ,first ,(first rest))
        ,@(awhen (rest rest)
            `((= ,first ,@it)))))

(define-compiler-macro /= (&whole form first &rest rest)
  (declare (ignore form))
  `(or (not (equalp ,first ,(first rest)))
       ,@(awhen (rest rest)
           `((/= ,first ,@it)))))
