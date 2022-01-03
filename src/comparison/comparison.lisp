;;;; comparison.lisp
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

(in-package :generic-cl.comparison)

;;;; Generic Comparison Functions

(defgeneric compare (a b)
  (:documentation
   "Returns :LESS if A is less than B (LESSP A B), :GREATER if A is
    greater than B (GREATERP A B) or :EQUAL if A is equal to B (EQUALP
    A B)."))

(defgeneric lessp (a b)
  (:documentation
   "Returns true if A compares less than B."))

(defgeneric greaterp (a b)
  (:documentation
   "Returns true if A compares greater than B."))

(defgeneric less-equal-p (a b)
  (:documentation
   "Returns true if A compares less than or equal to B."))

(defgeneric greater-equal-p (a b)
  (:documentation
   "Returns true if A compares greater than or equal to B."))


;;; Methods

;; Generic

(defmethod compare (a b)
  (cond
    ((lessp a b)
     :less)

    ((equalp a b)
     :equal)

    (t
     :greater)))

(defmethod greaterp (a b)
  (not (less-equal-p a b)))

(defmethod less-equal-p (a b)
  (or (lessp a b)
      (equalp a b)))

(defmethod greater-equal-p (a b)
  (not (lessp a b)))


;; Real Numbers

(defmethod compare ((a real) (b real))
  (let ((diff (cl:- a b)))
    (cond
      ((cl:zerop diff)
       :equal)

      ((cl:plusp diff)
       :greater)

      ((cl:minusp diff)
       :less))))

(defmethod lessp ((a real) (b real))
  (cl:< a b))

(defmethod greaterp ((a real) (b real))
  (cl:> a b))

(defmethod less-equal-p ((a real) (b real))
  (cl:<= a b))

(defmethod greater-equal-p ((a real) (b real))
  (cl:>= a b))


;; Characters

(defmethod lessp ((a character) (b character))
  (cl:char< a b))

(defmethod greaterp ((a character) (b character))
  (cl:char> a b))

(defmethod less-equal-p ((a character) (b character))
  (cl:char<= a b))

(defmethod greater-equal-p ((a character) (b character))
  (cl:char>= a b))


;; Strings

(defmethod lessp ((a string) (b string))
  (cl:string< a b))

(defmethod greaterp ((a string) (b string))
  (cl:string> a b))

(defmethod less-equal-p ((a string) (b string))
  (cl:string<= a b))

(defmethod greater-equal-p ((a string) (b string))
  (cl:string>= a b))


;;;; N Argument Functions

(defun < (x &rest xs)
  (loop
     for (x . xs) on (cons x xs)
     while xs
     always (lessp x (cl:first xs))))

(defun > (x &rest xs)
  (loop
     for (x . xs) on (cons x xs)
     while xs
     always (greaterp x (cl:first xs))))

(defun <= (x &rest xs)
  (loop
     for (x . xs) on (cons x xs)
     while xs
     always (less-equal-p x (cl:first xs))))

(defun >= (x &rest xs)
  (loop
     for (x . xs) on (cons x xs)
     while xs
     always (greater-equal-p x (cl:first xs))))


(defun min (first &rest xs)
  "Returns the minimum argument, the argument that is less than or
   equal to all the other arguments, the actual comparisons are done
   using LESSP. Any argument which satisfies this condition may be
   returned."

  (loop
     for min = first then (if (lessp x min) x min)
     for x in xs
     finally (return min)))

(defun max (first &rest xs)
  "Returns the maximum argument, the argument that is greater than or
   equal to all the other arguments, the actual comparisons are done
   using GREATERP. Any argument which satisfies this condition may be
   returned."

  (loop
     for max = first then (if (greaterp x max) x max)
     for x in xs
     finally (return max)))


;;; Optimizations

(define-compiler-macro < (x1 &rest xs &environment env)
  (if (numbers? (cons x1 xs) env)
      `(cl:< ,x1 ,@xs)

      (or (null xs)
	  (cons 'and
		(mapcar (curry #'list 'lessp) (cons x1 xs) xs)))))

(define-compiler-macro > (x1 &rest xs &environment env)
  (if (numbers? (cons x1 xs) env)
      `(cl:> ,x1 ,@xs)

      (or (null xs)
	  (cons 'and
		(mapcar (curry #'list 'greaterp) (cons x1 xs) xs)))))

(define-compiler-macro <= (x1 &rest xs &environment env)
  (if (numbers? (cons x1 xs) env)
      `(cl:<= ,x1 ,@xs)

      (or (null xs)
	  (cons 'and
		(mapcar (curry #'list 'less-equal-p) (cons x1 xs) xs)))))

(define-compiler-macro >= (x1 &rest xs &environment env)
  (if (numbers? (cons x1 xs) env)
      `(cl:>= ,x1 ,@xs)

      (or (null xs)
	  (cons 'and
		(mapcar (curry #'list 'greater-equal-p) (cons x1 xs) xs)))))


(define-compiler-macro min (&whole form x &rest xs &environment env)
  (if (numbers? (cons x xs) env)
      `(cl:min ,x ,@xs)
      form))

(define-compiler-macro max (&whole form x &rest xs &environment env)
  (if (numbers? (cons x xs) env)
      `(cl:max ,x ,@xs)
      form))
