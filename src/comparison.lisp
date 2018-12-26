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

(in-package :generic-cl.impl)

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

    ((greaterp a b)
     :greater)))

(defmethod greaterp (a b)
  (not (less-equal-p a b)))

(defmethod less-equal-p (a b)
  (or (lessp a b)
      (equalp a b)))

(defmethod greater-equal-p (a b)
  (not (lessp a b)))


;; Numbers

(defmethod compare ((a number) (b number))
  (let ((diff (cl:- a b)))
    (cond
      ((cl:zerop diff)
       :equal)

      ((cl:plusp diff)
       :greater)

      ((cl:minusp diff)
       :less))))

(defmethod lessp ((a number) (b number))
  (cl:< a b))

(defmethod greaterp ((a number) (b number))
  (cl:> a b))

(defmethod less-equal-p ((a number) (b number))
  (cl:<= a b))

(defmethod greater-equal-p ((a number) (b number))
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


;;;; N Argument Functions

(defun < (x &rest xs)
  (loop
     for (x . xs) on (cons x xs)
     while xs
     always (lessp x (first xs))))

(defun > (x &rest xs)
  (loop
     for (x . xs) on (cons x xs)
     while xs
     always (greaterp x (first xs))))

(defun <= (x &rest xs)
  (loop
     for (x . xs) on (cons x xs)
     while xs
     always (less-equal-p x (first xs))))

(defun >= (x &rest xs)
  (loop
     for (x . xs) on (cons x xs)
     while xs
     always (greater-equal-p x (first xs))))


;;; Optimizations

(define-compiler-macro < (x1 &rest xs)
  (if xs
      (cons 'and
	    (mapcar (curry #'list 'less) (cons x1 xs) xs))
      t))

(define-compiler-macro > (x1 &rest xs)
  (if xs
      (cons 'and
	    (mapcar (curry #'list 'greater) (cons x1 xs) xs))
      t))

(define-compiler-macro <= (x1 &rest xs)
  (if xs
      (cons 'and
	    (mapcar (curry #'list 'less-equal-p) (cons x1 xs) xs))
      t))

(define-compiler-macro >= (x1 &rest xs)
  (if xs
      (cons 'and
	    (mapcar (curry #'list 'greater-equal-p) (cons x1 xs) xs))
      t))
