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

(defmacro defstruct (options &rest slots)
  "Sames as CL:DEFSTRUCT except that a COPY method, for the struct, is
   automatically generated which invokes the structure's copier
   function. If a NIL :COPIER option is provided, the COPY method is
   not automatically generated."

  (let* ((name (ensure-car options))
	 (copier-opt (cdr (cl:find :copier (ensure-list options) :key #'ensure-car)))
	 (copier-name (if copier-opt (car copier-opt) (symb 'copy- name))))

    (with-gensyms (arg)
     `(progn
	(cl:defstruct ,options ,@slots)
	,(when copier-name
	  `(defmethod copy ((,arg ,name) &key)
	    (,copier-name ,arg)))
	',name))))


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
      (setf tail (cons (copy (car head) :deep t) nil)))
    (cdr tail)))


;;; Arrays/Vectors

(defmethod copy ((array array) &key deep)
  (if deep
      (deep-copy-vector array)
      (copy-array array)))

(defun deep-copy-vector (vec)
  "Returns a copy of LIST which contains a copy (by COPY) of each
   object contained in it."

  (let ((new (make-array (cl:length vec)
			 :element-type (array-element-type vec)
			 :adjustable (adjustable-array-p vec)
			 :fill-pointer (and (array-has-fill-pointer-p vec)
					    (fill-pointer vec)))))
    (loop
       for elem across vec
       for i = 0 then (1+ i)
       do
	 (setf (aref new i) (copy elem :deep t)))

    new))


;;; Other Objects

(defmethod copy (object)
  "Default method, does not copy OBJECT."

  object)
