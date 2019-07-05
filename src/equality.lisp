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


;;;; Generic EQUALP Predicate

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
      ((not (and (consp a) (consp b))) (equalp a b))
    (unless (equalp (car a) (car b))
      (return nil))))

(defmethod equalp ((a vector) (b vector))
  "Array equality comparison method. Returns true if both arrays are
   of the same length and each element of A is equal (by EQUALP) to
   the corresponding element of B."

  (and (cl:= (cl:length a) (cl:length b)) (cl:every #'equalp a b)))

(defmethod equalp ((a array) (b array))
  "Multi-dimensional array equality comparison method. Returns true if
   both arrays have the same dimensions and each element of A is
   equal (by EQUALP) to the corresponding element of B."

  (and (cl:equal (array-dimensions a) (array-dimensions b))
       (loop
	  for i from 0 below (array-total-size a)
	  always (equalp (row-major-aref a i)
			 (row-major-aref b i)))))

(defmethod equalp ((a string) (b string))
  "Returns true if both strings are equal by CL:STRING="

  (string= a b))

(defmethod equalp ((a pathname) (b pathname))
  "Returns true if both `pathname' objects are equal by
   CL-FAD:PATHNAME-EQUAL."

  (uiop:pathname-equal a b))


(defmethod equalp (a b)
  "Default equality comparison method. Returns true if objects A and B
   are the same object, compared using CL:EQ."

  (cl:eq a b))


;;;; N-Argument Functions

(defun = (first &rest rest)
  "Returns true if each object in REST is equal, by EQUALP, to FIRST."

  (cl:every (curry #'equalp first) rest))

(defun /= (first &rest rest)
  "Returns true if at least one object in REST is not equal, by
   EQUALP, to FIRST."

  (or (null rest)
      (cl:notevery (curry #'equalp first) rest)))


;;; Optimizations

(define-compiler-macro = (first &rest rest &environment env)
  (flet ((make-equalp (arg)
           `(equalp ,first ,arg)))

    (if (numbers? (cons first rest) env)
	`(cl:= ,first ,@rest)

	(or (null rest)
	    `(and ,@(mapcar #'make-equalp rest))))))

(define-compiler-macro /= (first &rest rest &environment env)
  (flet ((make-equalp (arg)
           `(not (equalp ,first ,arg))))

    (if (numbers? (cons first rest) env)
	`(cl:/= ,first ,@rest)

	(or (null rest)
	    `(or ,@(mapcar #'make-equalp rest))))))

(defun numbers? (args env)
  "Returns true if each form in ARGS evaluates (within the lexical
   environment ENV) to a type that is a subtype of NUMBER."

  (flet ((number? (thing)
	   ;; For some reason SUBTYPEP on CMUCL does not take a third
	   ;; environment parameter
	   (subtypep (get-value-type thing env) 'number #-cmucl env)))

    (every #'number? args)))
