;;;; cl-containers.lisp
;;;;
;;;; Copyright 2018-2019 Alexander Gutev
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

(in-package :generic-cl.container)

;;; Elements

;;;; ELT

(defmethod elt ((sequence sequence) index)
  (cl:elt sequence index))

(defmethod (setf elt) (value (sequence sequence) index)
  (setf (cl:elt sequence index) value))

(defmethod elt ((vec vector) index)
  (cl:elt vec index))

(defmethod (setf elt) (value (vec vector) index)
  (setf (cl:elt vec index) value))

(defmethod elt ((array array) (index integer))
  (row-major-aref array index))

(defmethod (setf elt) (value (array array) (index integer))
  (setf (row-major-aref array index) value))

(labels ((rmi (dims indices)
           "Mirrors cl:array-row-major-index, but functions exclusively on dimensions
            Length of dims and indices must be equal"
           ;; I'm going to submit this function to alexandria as row-major-index
           ;; so if we ever upgrade to alexandria 2 one day, then hopefully
           ;; we'll just be able to use that
           (loop with word-idx = 0
                 with dimprod = 1
                 for dim-size in (reverse dims)
                 for dim-idx in (reverse indices)
                 for dim from 0
                 do (unless (and (integerp dim-idx) (< -1 dim-idx dim-size))
                      (error (format nil "Index ~a invalid for axis ~a with size ~a"
                                     dim-idx dim dim-size)))
                    (incf word-idx (* dim-idx dimprod))
                    (setf dimprod (* dimprod dim-size))
                 finally (return word-idx))))

  (declare (inline rmi))

  (defmethod elt ((arr array) (indices list))
    (let* ((n-idcs (length indices))
           (dims (array-dimensions arr))
           (n-dims (length dims))
           (unused-dims (nthcdr n-idcs dims))
           (word-len (reduce #'* unused-dims :initial-value 1)))
      (cond ((= n-dims n-idcs) (apply #'aref arr indices))
            ((< n-dims n-idcs) (error (format nil "Numbes of indices exceeds rank of array")))
            (t (make-array unused-dims
                           :element-type (array-element-type arr)
                           :displaced-to arr
                           :displaced-index-offset (* word-len
                                                      (rmi (subseq dims 0 n-idcs) indices)))))))

  (defmethod (setf elt) (value (arr array) (indices list))
    (let* ((n-idcs (length indices))
           (dims (array-dimensions arr))
           (n-dims (length dims))
           (unused-dims (nthcdr n-idcs dims))
           (word-len (reduce #'* unused-dims :initial-value 1)))
      (cond ((= n-dims n-idcs) (setf (apply #'aref arr indices) value))
            ((< n-dims n-idcs) (error (format nil "Numbes of indices exceeds rank of array")))
            (t (unless (equalp unused-dims (array-dimensions value))
                 (error (format nil "Cannot set ~a-d slice using ~a-d~% array"
                                unused-dims (array-dimensions value))))
               (loop
                 with slice = (make-array
                               unused-dims
                               :element-type (array-element-type arr)
                               :displaced-to arr
                               :displaced-index-offset (* word-len
                                                          (rmi (subseq dims 0 n-idcs) indices)))

                 for i below (array-total-size slice)
                 do (setf (row-major-aref slice i) (row-major-aref value i))
                 finally (return slice)))))))

;;;; First

(defmethod first ((list list))
  (cl:first list))

(defmethod first ((vec vector))
  (aref vec 0))

(defmethod first ((array array))
  (row-major-aref array 0))


;;;; Last

(defmethod last ((list list) &optional (n 0))
  (car (cl:last list (cl:1+ n))))

(defmethod last ((vec vector) &optional (n 0))
  (aref vec (cl:- (cl:length vec) 1 n)))

(defmethod last ((array array) &optional (n 0))
  (row-major-aref array (cl:- (array-total-size array) 1 n)))


(defun lastcdr (list &optional (n 1))
  "Equivalent to CL:LAST. Returns the CDR of the N'th CONS cell from
   the last CONS cell in LIST."

  (cl:last list n))


;;;; ERASE

(defmethod erase ((vec vector) index)
  (unless (adjustable-array-p vec)
    (error 'type-error :datum vec :expected-type '(and vector (satisfies adjustable-array-p))))

  (let ((len (cl:length vec)))
    (loop
       for i from (cl:1+ index) below len
       do
	 (setf (aref vec (cl:1- i)) (aref vec i)))

    (if (array-has-fill-pointer-p vec)
	(adjust-array vec (cl:1- len) :initial-element 0 :fill-pointer t)
	(adjust-array vec (cl:1- len) :initial-element 0))))


;;; Length

(defmethod length ((sequence sequence))
  "Generic CL:SEQUENCE method, calls CL:LENGTH."

  (cl:length sequence))

(defmethod length ((vec vector))
  "Returns the number of elements in the vector VEC."

  (cl:length vec))

(defmethod length ((array array))
  "Returns the total number of elements in the multi-dimensional array
   ARRAY."

  (array-total-size array))


(defmethod emptyp ((list list))
  (null list))

(defmethod emptyp ((vec vector))
  (cl:zerop (cl:length vec)))

(defmethod emptyp ((array array))
  "Always returns false as a multi-dimensional array can never be
   empty."
  nil)


(defmethod clear ((vec vector))
  (unless (adjustable-array-p vec)
    (error 'type-error :datum vec :expected-type '(and vector (satisfies adjustable-array-p))))

  (if (array-has-fill-pointer-p vec)
      (adjust-array vec 0 :initial-element 0 :fill-pointer t)
      (adjust-array vec 0 :initial-element 0)))


;;; Adjust Size

(defmethod adjust-size ((seq list) size &key element)
  (check-type size (integer 0) "an integer greater than or equal to 0")

  (loop
     repeat size
     for cons = seq then (cdr cons)
     collect (if cons (car cons) element)))

(defmethod nadjust-size ((seq list) size &key element)
  (check-type size (integer 0) "an integer greater than or equal to 0")

  (cond
    ((cl:zerop size) nil)

    ((null seq)
     (make-list size :initial-element element))

    (t
     (loop
	for n from 0 below size
	for cons on seq
	for lastcell = cons
	finally
	  (if cons
	      (setf (cdr cons) nil)
	      (setf (cdr lastcell) (make-list (cl:- size n) :initial-element element))))
     seq)))


(defmethod adjust-size ((sequence vector) size &key element)
  (check-type size (integer 0) "an integer greater than or equal to 0")

  (if (adjustable-array-p sequence)
      (nadjust-size (copy sequence) size :element element)
      (nadjust-size sequence size :element element)))

(defmethod nadjust-size ((sequence vector) size &key element)
  (check-type size (integer 0) "an integer greater than or equal to 0")

  (adjust-array sequence size :initial-element element))


;;; Subsequence

(defmethod subseq ((seq sequence) start &optional end)
  (cl:subseq seq start end))

(defmethod (setf subseq) (value (seq sequence) start &optional end)
  (setf (cl:subseq seq start end) value))


;;; Creation

(defun sequence-of-type (type)
  "Creates a sequence of the type TYPE by calling
   MAKE-SEQUENCE-OF-TYPE.

   If TYPE is a list, MAKE-SEQUENCE-OF-TYPE is called with the CAR of
   the list as the first argument, and the CDR of the list as the
   second argument. Otherwise MAKE-SEQUENCE-OF-TYPE is called with
   TYPE as the first argument and NIL as the second argument."

  (destructuring-bind (type . args) (ensure-list type)
    (make-sequence-of-type type args)))

(defmethod make-sequence-of-type (type args)
  (let ((type (if args (cons type args) type)))
    (cleared (make-sequence type 0) :keep-element-type t)))


;;;; Lists

(defmethod cleared ((sequence list) &key)
  "Returns NIL the empty list."
  nil)


;;;; Vectors

(defmethod cleared ((vec vector) &key keep-element-type)
  (make-array (cl:length vec)
	      :element-type (if keep-element-type
				(array-element-type vec)
				t)
	      :adjustable t
	      :fill-pointer 0))
