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


;;;; Base Iterator Type

(defstruct (iterator (:constructor nil))
  "Base iterator type. All iterators should inherit (:INCLUDE) from
   this type in order for methods which specialize on iterators to be
   chosen.")


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
      (at iter))))

(defgeneric at (iterator)
  (:documentation
   "Returns the element at the position specified by ITERATOR. Is not
    guaranteed to check whether the end of the sequence has been
    reached."))

(defgeneric (setf at) (value iterator)
  (:documentation
   "Sets the value at the position of the sequence at which ITERATOR
    is currently at."))


(defgeneric advance (iterator)
  (:documentation
   "Advances ITERATOR to the next element of the sequence."))

(defgeneric endp (iterator)
  (:documentation
   "Returns true if ITERATOR points to the end of the sequence.")

  (:method (iter)
    (cl:endp iter)))

(defgeneric advance-n (iterator n)
  (:documentation
   "Advances the iterator ITERATOR N positions, advances to the
    position that the iterator would be at after calling ADVANCE N
    times.")

  (:method ((it iterator) n)
    (loop repeat n do (advance it))))


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

(defstruct (list-iterator (:include iterator))
  "Unbounded list iterator for iterating from a given starting
   position till the end of the list.

   CONS stores the CONS cell at the iterator's current position."

  cons)

(defmethod make-iterator ((list list) start (end null))
  (make-list-iterator :cons (nthcdr start list)))


(defmethod at ((iter list-iterator))
  (car (list-iterator-cons iter)))

(defmethod (setf at) (value (iter list-iterator))
  (setf (car (list-iterator-cons iter)) value))

(defmethod advance ((iter list-iterator))
  (slet (list-iterator-cons iter)
    (setf it (cdr it))))

(defmethod endp ((iter list-iterator))
  (cl:endp (list-iterator-cons iter)))


(defmethod subseq ((it list-iterator) start &optional end)
  (make-iterator (list-iterator-cons it) start end))


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

(defmethod length ((iter bound-list-iterator))
  (cl:- (bound-list-iterator-end iter) (bound-list-iterator-index iter)))


(defmethod subseq ((it bound-list-iterator) start &optional end)
  (->> (cl:- (bound-list-iterator-end it) (bound-list-iterator-index it))
       (or end)
       (call-next-method it start)))


;; Reverse

(defstruct (reverse-list-iterator (:include list-iterator))
  "Reverse iterator for iterating over the elements of a list in
   reverse order. Does not have any additional slots over
   `list-iterator', as it is only used for generic dispatch.")

(defmethod make-reverse-iterator ((list list) start (end null))
  (let (cells)
    (loop
       for cell on (nthcdr start list)
       do
	 (push cell cells))
    (make-reverse-list-iterator :cons cells)))

(defmethod make-reverse-iterator ((list list) start end)
  (let (cells)
    (loop
       for cell on (nthcdr start list)
       for index from start below end
       do
	 (push cell cells))
    (make-reverse-list-iterator :cons cells)))


(defmethod at ((iter reverse-list-iterator))
  (caar (reverse-list-iterator-cons iter)))

(defmethod (setf at) (value (iter reverse-list-iterator))
  (setf (caar (reverse-list-iterator-cons iter)) value))


(defmethod subseq ((it reverse-list-iterator) start &optional end)
  (let ((cells (cl:nthcdr start (reverse-list-iterator-cons it))))
    (make-reverse-list-iterator
     :cons
     (if end
	 (cl:butlast cells (cl:- end start))
	 cells))))


;;; Vector Iterator

(defstruct (vector-iterator (:include iterator))
  (index 0)
  array
  end)

(defmethod make-iterator ((vec vector) start end)
  (make-vector-iterator :array vec :index start :end (or end (cl:length vec))))


(defmethod at ((iter vector-iterator))
  (with-accessors ((vector vector-iterator-array)
		   (index vector-iterator-index)) iter

    (aref vector index)))

(defmethod (setf at) (value (iter vector-iterator))
  (with-accessors ((vector vector-iterator-array)
		   (index vector-iterator-index)) iter

    (setf (aref vector index) value)))

(defmethod advance ((iter vector-iterator))
  (incf (vector-iterator-index iter)))

(defmethod endp ((iter vector-iterator))
  (with-accessors ((vector vector-iterator-array)
		   (index vector-iterator-index)
		   (end vector-iterator-end)) iter
    (cl:>= index end)))


(defmethod length ((iter vector-iterator))
  (cl:- (vector-iterator-end iter) (vector-iterator-index iter)))

(defmethod advance-n ((iter vector-iterator) n)
  (cl:incf (vector-iterator-index iter) n))


(defmethod subseq ((it vector-iterator) start &optional end)
  (make-vector-iterator :array (vector-iterator-array it)
			:index (cl:+ (vector-iterator-index it) start)
			:end (or end (vector-iterator-end it))))

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


(defmethod length ((iter reverse-vector-iterator))
  (cl:- (cl:1+ (reverse-vector-iterator-index iter))
	(reverse-vector-iterator-end iter)))

(defmethod advance-n ((iter reverse-vector-iterator) n)
  (cl:decf (reverse-vector-iterator-index iter) n))


(defmethod subseq ((it reverse-vector-iterator) start &optional end)
  (with-accessors ((array reverse-vector-iterator-array)
		   (index reverse-vector-iterator-index)
		   (old-end reverse-vector-iterator-end)) it

    (make-reverse-vector-iterator
     :array array
     :index (cl:- index start)
     :end (if end
    	      (cl:1+ (cl:- index end))
    	      old-end))))


;;; Multi-dimensional Array Iterator

(defstruct (array-iterator (:include vector-iterator))
  "Multi-dimensional (non-vector) array iterator for iterator over
   each element of the flattened array. Does not have any additional
   slots over `vector-iterator', as it is used only for generic
   dispatch.")

(defmethod make-iterator ((array array) start end)
  (make-array-iterator :array array :index start :end (or end (array-total-size array))))

(defmethod at ((iter array-iterator))
  "Access the element of the array at the current position using
   ROW-MAJOR-AREF."

  (with-accessors ((array array-iterator-array)
		   (index array-iterator-index)) iter
    (row-major-aref array index)))

(defmethod (setf at) (value (iter array-iterator))
  (with-accessors ((array array-iterator-array)
		   (index array-iterator-index)) iter
    (setf (row-major-aref array index) value)))

(defmethod subseq ((it array-iterator) start &optional end)
  (make-array-iterator :array (array-iterator-array it)
		       :index (cl:+ (array-iterator-index it) start)
		       :end (or end (array-iterator-end it))))

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

(defmethod at ((iter reverse-array-iterator))
  "Access the element of the array at the current position using
   ROW-MAJOR-AREF."

  (with-accessors ((array reverse-array-iterator-array)
		   (index reverse-array-iterator-index)) iter
    (row-major-aref array index)))

(defmethod (setf at) (value (iter reverse-array-iterator))
  (with-accessors ((array reverse-array-iterator-array)
		   (index reverse-array-iterator-index)) iter
    (setf (row-major-aref array index) value)))

(defmethod subseq ((it reverse-array-iterator) start &optional end)
  (with-accessors ((array reverse-array-iterator-array)
		   (index reverse-array-iterator-index)
		   (old-end reverse-array-iterator-end)) it

    (make-reverse-array-iterator
     :array array
     :index (cl:- index start)
     :end (if end
    	      (cl:1+ (cl:- index end))
    	      old-end))))


;;;; Iteration Macros

(defmacro doiters ((&rest iters) &body body)
  "Iterates over one or more sequences and binds the iterator of each
   sequence to a variable.

   Each element of ITERS is a list of the form (IT-VAR SEQUENCE
   . ARGS) where IT-VAR is the variable to which the iterator for
   SEQUENCE is bound. Args are the remaining arguments passed to the
   ITERATOR function (if any).

   The forms in BODY are evaluated, with the iterator variable
   bindings visible to the forms, after which each iterator is
   advanced by one position, by the ADVANCE function. The forms are
   evaluated repeatedly until at least one iterator reaches the end of
   its sequence (ENDP returns true)."

  (flet ((make-it-binding (it)
	   (destructuring-bind (var &rest args) it
	     `(,var (iterator ,@args))))

	 (make-end-test (it)
	   `(endp ,(first it)))

	 (make-advance (it)
	   `(advance ,(first it))))

    `(let ,(mapcar #'make-it-binding iters)
       (loop until (or ,@(mapcar #'make-end-test iters))
	  do
	    (progn ,@body)
	    ,@(mapcar #'make-advance iters)))))

(defmacro doiter ((iter &rest args) &body body)
  "Sames as DOITERS however for the special case of iterating over a
   single sequence.

   ITER is the variable to which the iterator is bound. ARGS are the
   arguments passed to the ITERATOR function."

  `(doiters ((,iter ,@args)) ,@body))

(defmacro! doseq ((element o!sequence &rest args) &body body)
  "Iterates over the elements of the sequence SEQUENCE. For each
   element the forms in BODY are evaluated with the symbol named by
   ELEMENT bound to the current element of the sequence.

   ARGS are additional arguments passed to the ITERATOR function.

   The forms in BODY are surrounded in an implicit (BLOCK NIL ...)
   thus RETURN may be used to terminate the loop early. The return
   value of the DOSEQ form is NIL, unless it is terminated early by
   RETURN."

  `(doiter (,g!iter ,g!sequence ,@args)
     (let ((,element (at ,g!iter)))
       ,@body)))
