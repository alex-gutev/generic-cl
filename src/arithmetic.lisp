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


(defgeneric 1+ (a)
  (:documentation "Returns A + 1."))

(defgeneric 1- (a)
  (:documentation "Returns A - 1."))


(defgeneric minusp (a)
  (:documentation
   "Returns true if A is less than 0."))

(defgeneric plusp (a)
  (:documentation
   "Returns true if A is greater than 0"))

(defgeneric zerop (a)
  (:documentation "Returns true if A is zero."))

(defgeneric signum (a)
  (:documentation
   "Returns -1 - if A is negative, 0 - if A is zero or 1 if A is positive"))

(defgeneric abs (a)
  (:documentation "Returns the absolute value of A."))


(defgeneric evenp (a)
  (:documentation "Returns true if A is even."))

(defgeneric oddp (a)
  (:documentation "Returns true if A is odd."))


(defgeneric floor (n &optional d)
  (:documentation
   "Returns N, or N/D if D is provided, rounded towards negative
    infinity, and the remainder of the division if any."))

(defgeneric ceiling (n &optional d)
  (:documentation
   "Returns N, or N/D if D is provided, rounded towards positive
    infinity, and the remainder of the division if any."))

(defgeneric truncate (n &optional d)
  (:documentation
   "Returns N, or N/D if D is provided, rounded towards zero, and the
   remainder of the division if any."))

(defgeneric round (n &optional d)
  (:documentation
   "Returns N, or N/D if D is provided, rounded towards the nearest
    integer. If the quotient lies exactly halfway between two integers
    it is rounded to the nearest even integer."))


(defgeneric mod (n d)
  (:documentation
   "Returns the remainder of the floor operation on N and D."))

(defgeneric rem (n d)
  (:documentation
   "Returns the remainder of the truncate operation on N and D."))


;;; CL:NUMBER Methods

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


(defmethod 1+ ((a number))
  (cl:1+ a))

(defmethod 1- ((a number))
  (cl:1- a))


(defmethod minusp ((a number))
  (cl:minusp a))

(defmethod plusp ((a number))
  (cl:plusp a))

(defmethod zerop ((a number))
  (cl:zerop a))

(defmethod signum ((a number))
  (cl:signum a))

(defmethod abs ((a number))
  (cl:abs a))


(defmethod evenp ((a number))
  (cl:evenp a))

(defmethod oddp ((a number))
  (cl:oddp a))


(defmethod floor ((n number) &optional (d nil d-sp))
  (if d-sp
      (cl:floor n d)
      (cl:floor n)))

(defmethod ceiling ((n number) &optional (d nil d-sp))
  (if d-sp
      (cl:ceiling n d)
      (cl:ceiling n)))

(defmethod truncate ((n number) &optional (d nil d-sp))
  (if d-sp
      (cl:truncate n d)
      (cl:truncate n)))

(defmethod round ((n number) &optional (d nil d-sp))
  (if d-sp
      (cl:round n d)
      (cl:round n)))


(defmethod mod ((n number) (d number))
  (cl:mod n d))

(defmethod rem ((n number) (d number))
  (cl:rem n d))


;;; Generic Methods

(defmethod 1+ (a)
  (add a 1))

(defmethod 1- (a)
  (subtract a 1))


(defmethod minusp (a)
  (lessp a 0))

(defmethod plusp (a)
  (greaterp a 0))

(defmethod zerop (a)
  (equalp a 0))

(defmethod signum (a)
  (cond
    ((minusp a) -1)
    ((zerop a) 0)
    (t 1)))

(defmethod abs (a)
  (if (minusp a)
      (negate a)
      a))


(defmethod evenp (a)
  (zerop (mod a 2)))

(defmethod oddp (a)
  (not (evenp a)))


(defmethod mod (n d)
  (nth-value 1 (floor n d)))

(defmethod rem (n d)
  (nth-value 1 (truncate n d)))


;;;; INCF and DECF Macros

(define-modify-macro incf (&optional (delta 1))
  add
  "Increments the place by DELTA (defaults to 1) using the generic ADD
   function.")

(define-modify-macro decf (&optional (delta 1))
  subtract
  "Decrements the place by DELTA (defaults to 1) using the generic
   SUBTRACT function.")


;;;; N-Argument Functions

;; Symbol macros for the REPL variables
(define-symbol-macro + cl:+)
(define-symbol-macro - cl:-)
(define-symbol-macro * cl:*)
(define-symbol-macro / cl:/)

(defun + (&rest xs)
  (if xs
      (cl:reduce #'add xs)
      0))

(defun - (x &rest xs)
  (if xs
      (cl:reduce #'subtract xs :initial-value x)
      (negate x)))

(defun * (&rest xs)
  (if xs
      (cl:reduce #'multiply xs)
      1))

(defun / (x &rest xs)
  (if xs
      (cl:reduce #'divide xs :initial-value x)
      (divide 1 x))) ; Should return reciprocal in this case


;;; Optimizations

(define-compiler-macro + (&rest xs &environment env)
  (if (numbers? xs env)
      `(cl:+ ,@xs)

      (match xs
	(nil 0)

	((list x) x)

	((list* x xs)
	 (cl:reduce (lambda (sum x) `(add ,sum ,x)) xs :initial-value x)))))

(define-compiler-macro - (x &rest xs &environment env)
  (if (numbers? (cons x xs) env)
      `(cl:- ,x ,@xs)

      (if xs
	  (cl:reduce (lambda (diff x) `(subtract ,diff ,x)) xs :initial-value x)
	  `(negate ,x))))

(define-compiler-macro * (&rest xs &environment env)
  (if (numbers? xs env)
      `(cl:* ,@xs)

      (match xs
	(nil 1)

	((list x) x)

	((list* x xs)
	 (cl:reduce (lambda (prod x) `(multiply ,prod ,x)) xs :initial-value x)))))

(define-compiler-macro / (x &rest xs &environment env)
  (if (numbers? (cons x xs) env)
      `(cl:/ ,x ,@xs)

      (if xs
	  (cl:reduce (lambda (frac x) `(divide ,frac ,x)) xs :initial-value x)
	  `(divide 1 ,x))))
