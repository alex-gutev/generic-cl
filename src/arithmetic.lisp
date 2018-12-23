;;;; arithmetic.lisp
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

;;;; Generic Arithmetic Functions

(defgeneric add (a b)
  (:documentation "Returns the sum of A and B."))

(defgeneric subtract (a b)
  (:documentation "Returns the difference of A and B."))

(defgeneric multiply (a b)
  (:documentation "Returns the product of A and B."))

(defgeneric divide (a b)
  (:documentation
   "Returns the quotient of A and B. If A is the constant 1, should
    return the reciprocal of B."))

(defgeneric negate (a)
  (:documentation "Returns the negation of A."))

;; TODO: Add generic MOD and REM functions. These are more
;; complicated as they are defined in terms of FLOOR and TRUNCATE.


;;; Methods

(defmethod add ((a number) (b number))
  (cl:+ a b))

(defmethod subtract ((a number) (b number))
  (cl:- a b))

(defmethod multiply ((a number) (b number))
  (cl:* a b))

(defmethod divide ((a number) (b number))
  (cl:/ a b))

(defmethod negate ((a number))
  (cl:- a))


;;;; N-Argument Functions

;; Symbol macros for the +, * REPL variables
(define-symbol-macro + cl:+)
(define-symbol-macro - cl:-)
(define-symbol-macro * cl:*)
(define-symbol-macro / cl:/)

(defun + (&rest xs)
  (if xs
      (reduce #'add xs)
      0))

(defun - (x &rest xs)
  (if xs
      (reduce #'subtract xs :initial-value x)
      (negate x)))

(defun * (&rest xs)
  (if xs
      (reduce #'multiply xs)
      1))

(defun / (x &rest xs)
  (if xs
      (reduce #'divide xs :initial-value x)
      (divide 1 x))) ; Should return reciprocal in this case


;;; Optimizations

(define-compiler-macro + (&whole form &rest xs)
  (declare (ignore form))

  (match xs
    (nil 1)

    ((list x) x)

    ((list* x xs)
     (reduce (lambda (sum x) `(add ,sum ,x)) xs :initial-value x))))

(define-compiler-macro - (&whole form x &rest xs)
  (declare (ignore form))

  (if xs
      (reduce (lambda (diff x) `(subtract ,diff ,x)) xs :initial-value x)
      `(negate ,x)))

(define-compiler-macro * (&whole form &rest xs)
  (declare (ignore form))

  (match xs
    (nil 1)

    ((list x) x)

    ((list* x xs)
     (reduce (lambda (prod x) `(multiply ,prod ,x)) xs :initial-value x))))

(define-compiler-macro / (&whole form x &rest xs)
  (declare (ignore form))

  (if xs
      (reduce (lambda (frac x) `(divide ,frac ,x)) xs :initial-value x)
      `(divide 1 ,x)))
