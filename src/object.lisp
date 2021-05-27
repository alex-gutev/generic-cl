;;;; object.lisp
;;;;
;;;; Copyright 2019 Alexander Gutev
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


;;; Miscellaneous generic functions for manipulating objects

(defgeneric copy (object &key &allow-other-keys)
  (:documentation
   "Returns a copy of OBJECT. Some methods may accept additional
    keyword arguments which allow options, on how the object is to be
    copied, to be specified.

    Methods specialized on sequences or collections should accept
    the :DEEP keyword argument which if provided and is true, the
    objects contained in the sequence/collection should be copied as
    well otherwise (if not provided or is NIL) only the
    sequence/collection itself should be copied."))

(defgeneric coerce (object result-type)
  (:documentation
   "Coerces the OBJECT to the type RESULT-TYPE.")

  (:method (object type)
    (cl:coerce object type)))


;;;; Copy methods for standard objects.

;;; Lists

(defmethod copy ((list cons) &key deep)
  (if deep
      (deep-copy-list list)
      (copy-list list)))

(defun deep-copy-list (list)
  "Returns a copy of LIST which contains a copy (by COPY) of each
   object contained in it."

  (let ((tail (cons nil nil)))
    (do ((tail tail)
	 (head list (cdr head)))
	((atom head) (setf (cdr tail) (copy head :deep t)))

      (->> (cons (copy (car head) :deep t) nil)
	   (setf (cdr tail))
	   (setf tail)))

    (cdr tail)))


;;; Vectors

(defmethod copy ((array vector) &key deep)
  (if deep
      (deep-copy-vector array)
      (copy-array array)))

(defun deep-copy-vector (vec)
  "Returns a copy of the vector VEC which contains a copy (by COPY) of
   each object contained in it."

  (let ((new (make-array (cl:length vec)
			 :element-type (array-element-type vec)
			 :adjustable (adjustable-array-p vec)
			 :fill-pointer (and (array-has-fill-pointer-p vec)
					    (fill-pointer vec)))))
    (loop
       for elem across vec
       for i = 0 then (cl:1+ i)
       do
	 (setf (aref new i) (copy elem :deep t)))

    new))


;;; Arrays

(defmethod copy ((array array) &key deep)
  (if deep
      (deep-copy-array array)
      (copy-array array)))

(defun deep-copy-array (array)
  "Returns a copy of the multi-dimensional ARRAY which contains a
   copy (by COPY) of each object contained in it."

  (let ((new (make-array (array-dimensions array)
			 :element-type (array-element-type array)
			 :adjustable (adjustable-array-p array))))
    (loop
       for i from 0 below (array-total-size array)
       do
	 (setf (row-major-aref new i)
	       (copy (row-major-aref array i) :deep t)))
    new))


;;; Structures

(defmethod copy ((object structure-object) &key)
  (copy-structure object))

;;; Other Objects

(defmethod copy (object &key)
  "Default method, does not copy OBJECT."

  object)
