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

(in-package :generic-cl.iterator)


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


;;; Iterator Creation

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

   END is the number of elements (to iterate over) between the current
   position and the end of the sublist."

  end)

(defmethod make-iterator ((list list) start (end number))
  (make-bound-list-iterator :cons (nthcdr start list) :end (cl:- end start)))


(defmethod advance ((iter bound-list-iterator))
  (with-accessors ((cons bound-list-iterator-cons)
		   (end bound-list-iterator-end)) iter
    (setf cons (cdr cons))
    (decf end)))

(defmethod endp ((iter bound-list-iterator))
  (with-accessors ((cons bound-list-iterator-cons)
		   (end bound-list-iterator-end)) iter
    (or (cl:endp cons)
	(cl:<= end 0))))

(defmethod length ((iter bound-list-iterator))
  (bound-list-iterator-end iter))


(defmethod subseq ((it bound-list-iterator) start &optional end)
  (->> (bound-list-iterator-end it)
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
				:index (cl:1- (or end (cl:length vec)))
				:end start))


(defmethod advance ((iter reverse-vector-iterator))
  (decf (reverse-vector-iterator-index iter)))

(defmethod endp ((iter reverse-vector-iterator))
  (with-accessors ((index reverse-vector-iterator-index)
		   (end reverse-vector-iterator-end)) iter
    (cl:< index end)))


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
			       :index (cl:1- (or end (array-total-size array)))
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



;;; Iterator Subsequences

(defstruct (sub-iterator (:include iterator)
			 (:copier nil))
  "Iterator for iterating over a sub-sequence given an iterator for
   the sequence.

   ITER is the sequence's iterator. END is the index of the end of the
   subsequence."

  iter
  end)

(defmethod copy ((it sub-iterator) &key)
  "Returns a copy of a `SUB-ITERATOR' which contains a copy of the
   sequence's iterator stored in the ITER slot."

  (with-accessors ((iter sub-iterator-iter)
		   (index sub-iterator-index)
		   (end sub-iterator-end)) it
    (make-sub-iterator :iter (copy iter) :end end)))

(defmethod subseq ((it iterator) start &optional end)
  "Returns an iterator for the subseqeunce [START, END) of the
   sequence with iterator IT. Both START and END are interpreted
   relative to the position of the iterator IT within its sequence. "

  (let ((it (copy it)))
    (advance-n it start)

    (if end
	(make-sub-iterator :iter (copy it) :end (cl:- end start))
	it)))

(defmethod at ((it sub-iterator))
  (at (sub-iterator-iter it)))

(defmethod (setf at) (value (it sub-iterator))
  (setf (sub-iterator-iter it) value))

(defmethod endp ((it sub-iterator))
  (with-accessors ((iter sub-iterator-iter)
		   (end sub-iterator-end)) it
    (or (endp iter)
	(cl:<= end 0))))

(defmethod advance ((it sub-iterator))
  (cl:decf (sub-iterator-end it))
  (advance (sub-iterator-iter it)))

(defmethod advance-n ((it sub-iterator) n)
  (cl:decf (sub-iterator-end it) n)
  (advance-n (sub-iterator-iter it) n))

(defmethod length ((it sub-iterator))
  (sub-iterator-end it))

(defmethod subseq ((it sub-iterator) start &optional end)
  (with-accessors ((iter sub-iterator-iter)
		   (sub-end sub-iterator-end)) it
    (subseq (sub-iterator-iter it) start (or end sub-end))))


;;;; Iteration Macros

(defmacro doiters (name/iters &body body)
  "Iterate over one or more sequences with the iterator of each sequence bound to a variable.

   The macro arguments can be in one of the following two forms:

    1. (NAME (&REST ITERS) &BODY BODY)

    2. ((&REST ITERS) &BODY BODY)


   NAME is a symbol serving as the name of the BLOCK from which the
   forms in BODY can return, using (RETURN-FROM NAME ...). If not
   given defaults to NIL.

   ITERS is a list of bindings with each element of the form (IT-VAR
   SEQUENCE . ARGS). IT-VAR is the variable to which the iterator for
   SEQUENCE is bound. ARGS are the remaining arguments passed to the
   ITERATOR function (if any).

   BODY is a list of forms which are evaluated at each iteration. The
   bindings to the iterator variables (IT-VAR's) are visible. At the
   end of each iteration, each iterator is advanced by one position
   with ADVANCE. The loop is terminated, with the DOITERS form
   returning NIL, when at least one of the iterators reaches the end
   of its sequence (ENDP returns true). The loop can be terminated
   early with a RETURN-FROM to the block named NAME."

  (let-if ((name name/iters)
           (iters (first body) name/iters)
           (body (rest body) body))
      (symbolp name/iters)

    (flet ((make-it-binding (it)
	     (destructuring-bind (var &rest args) it
	       `(,var (iterator ,@args))))

	   (make-end-test (it)
	     `(endp ,(car it)))

	   (make-advance (it)
	     `(advance ,(car it))))

      `(let ,(mapcar #'make-it-binding iters)
         (loop named ,name
            until (or ,@(mapcar #'make-end-test iters))
	    do
	      (progn ,@body)
	      ,@(mapcar #'make-advance iters))))))

(defmacro doiter (name/iter &body body)
  "Sames as DOITERS however for the special case of iterating over a
   single sequence.

   The macro arguments can be in one of the following two forms:

    1. (NAME (ITER &REST ARGS) &BODY BODY)

    2. ((ITER &REST ARGS) &BODY BODY)

   NAME is the block name, NIL if not given.

   ITER is the variable to which the sequence iterator is bound.

   ARGS are the arguments passed to the ITERATOR function, the result
   of which is bound to ITER.

   BODY is the list of forms in the loop body."

  (let-if ((name name/iter)
           (iter (first body) name/iter)
           (body (rest body) body))
      (symbolp name/iter)

    (destructuring-bind (iter &rest args) iter
      `(doiters ,name ((,iter ,@args)) ,@body))))

(defmacro do-sequences (name/seqs &body body)
  "Iterate over the elements of one or more sequences.

   Iteration is performed using the iterator interface, and over
   multiple sequences simultaneously.

   The macro arguments can be in one of the following two forms:

    1. (NAME (&REST SEQS) &BODY BODY)

    2. ((&REST SEQS) &BODY BODY)

   NAME is a symbol serving as the name of the BLOCK from which the
   forms in BODY can return, using (RETURN-FROM NAME ...). If not
   given defaults to NIL.

   Each element of SEQS is a list of the form (VAR SEQUENCE . ARGS),
   where VAR is the variable to which the element of the sequence,
   SEQUENCE, is bound, at each iteration, and ARGS are arguments
   passed to the ITERATOR function, after the sequence.

   BODY is a list of forms evaluated at each iteration. RETURN-FROM to
   the block named NAME may be used to terminate the iteration early
   and return a value from the DO-SEQUENCES form. NIL is returned if
   there is no explicit RETURN."

  (let-if ((name name/seqs)
           (seqs (first body) name/seqs)
           (body (rest body) body))
      (symbolp name/seqs)

    (flet ((bind-elem (elem iter body)
             (match elem
               ((type list)
                `((destructuring-bind ,elem (at ,iter)
                    ,@body)))

               (_
                `((let ((,elem (at ,iter)))
                    ,@body))))))

      (let ((iters (make-gensym-list (cl:length seqs) "ITER")))
        `(doiters ,name
           ,(loop
               for (nil . seq) in seqs
               for iter in iters
               collect `(,iter ,@seq))

           ,@(loop
                for (elem) in seqs
                for iter in iters
                for forms = (bind-elem elem iter body)
                then (bind-elem elem iter forms)
                finally (return forms)))))))

(defmacro doseq (name/seq &body body)
  "Iterate over the elements of a single sequence SEQUENCE.

   Same as DO-SEQUENCES but for the special case of iterating over a
   single sequence.

   The macro arguments can be in one of the following two forms:

    1. (NAME (ELEMENT SEQUENCE &REST ARGS) &BODY BODY)

    2. ((ELEMENT SEQUENCE &REST ARGS) &BODY BODY)

   NAME is the block name, NIL if not given.

   ELEMENT is the variable to which each element of the sequence is
   bound.

   SEQUENCE is the sequence, with ARGS being the remaining arguments
   passed to the ITERATOR function.

   BODY is the list of forms evaluated on each iteration."

  (let-if ((name name/seq)
           (seq (first body) name/seq)
           (body (rest body) body))
      (symbolp name/seq)

    (destructuring-bind (element sequence &rest args) seq
      `(do-sequences ,name
         ((,element ,sequence ,@args))
         ,@body))))
