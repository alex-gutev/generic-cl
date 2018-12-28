;;;; iterator.lisp
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


;;;; Generic Iterator Interface

(defgeneric make-iterator (sequence start end)
  (:documentation
   "Returns an iterator for elements of the sub-sequence of SEQUENCE,
    bounded to the range [START, END).

    START is the index of the first element to iterate over.

    END is the index of the element at which to terminate the
    iteration, the element itself is not visited.  If END is NIL then
    the iteration continues till the end of the sequence."))

(defgeneric make-reverse-iterator (sequence start end)
  (:documentation
   "Same as MAKE-ITERATOR except the elements should be iterate over
    in reverse order."))


(defgeneric start (iterator)
  (:documentation
   "Returns the element at the position specified by ITERATOR or NIL
    if the ITERATOR points to the end of the sequence.")

  (:method (iter)
    (unless (endp iter)
      (current iter))))

(defgeneric current (iterator)
  (:documentation
   "Returns the element at the position specified by ITERATOR. Is not
    guaranteed to check whether the end of the sequence has been
    reached."))

(defgeneric advance (iterator)
  (:documentation
   "Advances ITERATOR to the next element of the sequence."))

(defgeneric endp (iterator)
  (:documentation
   "Returns true if ITERATOR points to the end of the sequence."))


;;; Non-generic utility functions

(defun iterator (sequence &key (start 0) end from-end)
  "Returns an iterator for the sub-sequence of SEQUENCE bounded by the
   range [START, END).

   START is the index of the first element to iterate over. Defaults
   to 0.

   END is the index of the element at which to terminate the
   iteration, the element itself is not visited. If END is NIL then
   the iteration continues till the end of the sequence. Defaults to
   NIL.

   If FROM-END is T the elements are iterated in reverse
   order. Defaults to NIL."

  (if from-end
      (make-reverse-iterator sequence start end)
      (make-iterator sequence start end)))


;;; List Iterator

;; Unbounded

(defstruct list-iterator
  "Unbounded list iterator for iterating from a given starting
   position till the end of the list.

   CONS stores the CONS cell at the iterator's current position."

  cons)

(defmethod make-iterator ((list list) start (end null))
  (make-list-iterator :cons (nthcdr start list)))

(defmethod make-reverse-iterator ((list list) start end)
  (make-list-iterator :cons (cl:nreverse (cl:subseq list start end))))

(defmethod current ((iter list-iterator))
  (car (list-iterator-cons iter)))

(defmethod advance ((iter list-iterator))
  (slet (list-iterator-cons iter)
    (setf it (cdr it))))

(defmethod endp ((iter list-iterator))
  (cl:endp (list-iterator-cons iter)))

;; Bounded

(defstruct (bound-list-iterator (:include list-iterator))
  "Bounded list iterator for iterator from a given starting position
   till a given end position.

   INDEX is the index of the current position.

   END is the index of the end position."

  index end)

(defmethod make-iterator ((list list) start (end number))
  (make-bound-list-iterator :cons (nthcdr start list) :index start :end end))

(defmethod advance ((iter bound-list-iterator))
  (with-accessors ((cons bound-list-iterator-cons)
		   (index bound-list-iterator-index)) iter
    (setf cons (cdr cons))
    (incf index)))

(defmethod endp ((iter bound-list-iterator))
  (with-accessors ((cons bound-list-iterator-cons)
		   (index bound-list-iterator-index)
		   (end bound-list-iterator-end)) iter
    (or (cl:endp cons)
	(cl:>= index end))))


;;; Vector Iterator

(defstruct vector-iterator
  (index 0)
  array
  end)

(defmethod make-iterator ((vec vector) start end)
  (make-vector-iterator :array vec :index start :end (or end (cl:length vec))))

(defmethod current ((iter vector-iterator))
  (with-accessors ((vector vector-iterator-array)
		   (index vector-iterator-index)) iter

    (aref vector index)))

(defmethod advance ((iter vector-iterator))
  (incf (vector-iterator-index iter)))

(defmethod endp ((iter vector-iterator))
  (with-accessors ((vector vector-iterator-array)
		   (index vector-iterator-index)
		   (end vector-iterator-end)) iter
    (cl:>= index end)))

;; Reverse

(defstruct (reverse-vector-iterator (:include vector-iterator))
  "Reverse iterator for iterating over the elements of a vector in
   reverse order. Does not have any additional slots over
   `vector-iterator', as it is only used for generic dispatch.")

(defmethod make-reverse-iterator ((vec vector) start end)
  (make-reverse-vector-iterator :array vec
				:index (1- (or end (cl:length vec)))
				:end start))

(defmethod advance ((iter reverse-vector-iterator))
  (decf (reverse-vector-iterator-index iter)))

(defmethod endp ((iter reverse-vector-iterator))
  (with-accessors ((index reverse-vector-iterator-index)
		   (end reverse-vector-iterator-end)) iter
    (< index end)))


;;; Multi-dimensional Array Iterator

(defstruct (array-iterator (:include vector-iterator))
  "Multi-dimensional (non-vector) array iterator for iterator over
   each element of the flattened array. Does not have any additional
   slots over `vector-iterator', as it is used only for generic
   dispatch.")

(defmethod make-iterator ((array array) start end)
  (make-array-iterator :array array :index start :end (or end (array-total-size array))))

(defmethod current ((iter array-iterator))
  "Access the element of the array at the current position using
   ROW-MAJOR-AREF."

  (with-accessors ((array array-iterator-array)
		   (index array-iterator-index)) iter
    (row-major-aref array index)))

;; Reverse

(defstruct (reverse-array-iterator (:include reverse-vector-iterator))
  "Multi-dimensional (non-vector) array iterator for iterating over
  the elements of the flattened array in reverse order. Does not have
  any additional slots over `reverse-vector-iterator', as it is used
  only for generic dispatch.")

(defmethod make-reverse-iterator ((array array) start end)
  (make-reverse-array-iterator :array array
			       :index (1- (or end (array-total-size array)))
			       :end start))

(defmethod current ((iter reverse-array-iterator))
  "Access the element of the array at the current position using
   ROW-MAJOR-AREF."

  (with-accessors ((array reverse-array-iterator-array)
		   (index reverse-array-iterator-index)) iter
    (row-major-aref array index)))


;;; Hash-table

(defmethod make-iterator ((hash hash-table) start end)
  "Create an iterator for the elements of a `hash-table' where each
   element is a CONS of the form (KEY . VALUE). The order in which the
   elements are iterated is unspecified, likewise there is no
   guaranteed which elements will be iterated over if START is not 0
   and END is not NIL."

  (make-iterator (hash-table-alist hash) start end))

(defmethod make-reverse-iterator ((hash hash-table) start end)
  "Create a reverse iterator for the elements of a `hash-table'. Since
   the order of iteration is unspecified this is identical to
   MAKE-ITERATOR."

  (make-iterator hash start end))


;;;; Iteration Macros

(defmacro! doseq ((element o!sequence &rest args) &body body)
  "Iterates over the elements of the sequence SEQUENCE. For each
   element the forms in BODY are evaluated with the symbol named by
   ELEMENT bound to the current element of the sequence.

   ARGS are additional arguments passed to the ITERATOR function.

   The forms in BODY are surrounded in an implicit (BLOCK NIL ...)
   thus RETURN may be used to terminate the loop early. The return
   value of the DOSEQ form is NIL, unless it is terminated early by
   RETURN."

  `(let ((,g!iter (iterator ,g!sequence ,@args))
	 (,element))
     (loop until (endp ,g!iter)
	do
	  (setf ,element (current ,g!iter))
	  ,@body
	  (advance ,g!iter))))