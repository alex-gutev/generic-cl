;;;; generic-sequences.lisp
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

(in-package :generic-cl.impl)

;;; Generic methods based on iterators


;;; Element Access

(defmethod first (sequence)
  (at (iterator sequence)))

(defmethod last (sequence)
  (at (iterator sequence :from-end t)))


;;; Length

(defmethod length (sequence)
  "Returns the number of elements in SEQUENCE, where SEQUENCE is of a
   type for which the iterator interface is implemented.

   The type is computed by iterating through the entire sequence, thus
   this is a linear O(n) operation, in the number of elements in the
   sequence."

  (length (iterator sequence)))

(defmethod length ((it iterator))
  "Returns the number of elements which will be iterated by the
   iterator IT. This is achieved by iterating until the end of the
   sequence using a copy of IT."

  (do ((it (copy it))
       (n 0 (1+ n)))
      ((endp it) n)
    (advance it)))


;;; Subsequence

(defmethod subseq (seq start &optional end)
  (let ((collector (empty-clone seq)))
    (extend collector (iterator seq :start start :end end))
    (collector-sequence collector)))

(defmethod (setf subseq) (new seq start &optional end)
  (loop
     with seq-it = (iterator seq :start start :end end)
     with new-it = (iterator new)
     until (or (endp seq-it) (endp new-it))
     do
       (setf (at seq-it) (at new-it)))
  new)


;;; Sequence Operations

;; Replacing elements of a sequence

(defmethod fill (seq item &key (start 0) end)
  (loop
     with it = (iterator seq :start start :end end)
     until (endp it)
     do
       (setf (at it) item)))

(defmethod replace (seq1 seq2 &key (start1 0) end1 (start2 0) end2)
  (loop
     with it1 = (iterator seq1 :start start1 :end end1)
     with it2 = (iterator seq2 :start start2 :end end2)
     until (or (endp it1) (endp it2))
     do
       (setf (at it1) (at it2))))


;; Reduction

(defmethod reduce (fn sequence &key key from-end (start 0) end (initial-value nil init-sp))
  (let ((key (or key #'identity))
	(f (if from-end (lambda (x y) (funcall fn y x)) fn)) ; Flip function arguments if FROM-END is T
	(iter (iterator sequence :start start :end end :from-end from-end)))

    (flet ((reduce-seq (res)
	     (advance iter)
	     (loop
		with elem
		with res = res
		until (endp iter)
		do
		  (setf elem (at iter))
		  (setf res (funcall f res elem))
		  (advance iter)
		finally (return res))))

      (if (endp iter) ; If sequence is empty
	  ;; Return INITIAL-VALUE if supplied or call FN with no arguments
	  (if init-sp initial-value (funcall fn))

	  (let ((elem (funcall key (at iter))))
	    (reduce-seq (if init-sp (funcall f initial-value elem) elem)))))))


;; Count

(defmethod count (item sequence &key from-end (start 0) end key (test #'equalp))
  (count-if (test-eq test item) sequence :from-end from-end :start start :end end :key key))

(defmethod count-if (test sequence &key from-end (start 0) end key)
  (let ((key (or key #'identity))
	(count 0))
    (doseq (elem sequence :from-end from-end :start start :end end)
      (when (funcall test (funcall key elem))
	(incf count)))
    count))

(defmethod count-if-not (test sequence &key from-end (start 0) end key)
  (count-if (test-not test) sequence
	    :from-end from-end
	    :start start
	    :end end
	    :key key))


;; Find

(defmethod find (item sequence &key from-end (start 0) end (test #'equalp) key)
  (find-if (test-eq test item) sequence :from-end from-end :start start :end end :key key))

(defmethod find-if (test sequence &key from-end (start 0) end key)
  (let ((key (or key #'identity)))
   (doseq (elem sequence :from-end from-end :start start :end end)
     (when (funcall test (funcall key elem))
       (return elem)))))

(defmethod find-if-not (test sequence &key from-end (start 0) end key)
  (find-if (test-not test) sequence :from-end from-end :start start :end end :key key))


;; Position

(defmethod position (item sequence &key from-end (start 0) end (test #'equalp) key)
  (position-if (test-eq test item) sequence :from-end from-end :start start :end end :key key))

(defmethod position-if (test sequence &key from-end (start 0) end key)
  (flet ((compute-pos (pos)
	   (+ start
	      (if from-end
		  (- (or end (length sequence)) pos)
		  pos))))
    (let ((key (or key #'identity))
	  (pos 0))
      (doseq (elem sequence :from-end from-end :start start :end end)
	(when (funcall test (funcall key elem))
	  (return (compute-pos pos)))
	(incf pos)))))

(defmethod position-if-not (test sequence &key from-end (start 0) end key)
  (position-if (test-not test) sequence :from-end from-end :start start :end end :key key))


;; Searching for/Comparing subsequences

(defmethod mismatch (seq1 seq2 &key from-end (test #'equalp) key (start1 0) (start2 0) end1 end2)
  (flet ((compute-pos (pos)
	   (if from-end
	       (- (or end1 (length seq1)) pos)
	       pos)))
    (let ((key (or key #'identity)))
      (loop
	 with it1 = (iterator seq1 :start start1 :end end1 :from-end from-end)
	 with it2 = (iterator seq2 :start start2 :end end2 :from-end from-end)
	 for pos = 0 then (1+ pos)
	 until (or (endp it1) (endp it2))
	 do
	   (unless (funcall test (funcall key (at it1)) (funcall key (at it2)))
	     (return (compute-pos pos)))

	   (advance it1)
	   (advance it2)

	 finally
	   (unless (and (endp it1) (endp it2))
	     (return (compute-pos pos)))))))


;; Reversing

(defmethod reverse (seq)
  (let* ((collector (make-collector (empty-clone seq))))
    (extend collector (iterator seq :from-end t))
    (collector-sequence collector)))

(defmethod nreverse (seq)
  (reverse seq))


;; Substitute

(defmethod nsubstitute (new old sequence &key from-end (test #'equalp) (start 0) end count key)
  (nsubstitute-if new (test-eq test old) sequence
		  :from-end from-end
		  :start start
		  :end end
		  :count count
		  :key key))

(defmethod nsubstitute-if (new test sequence &key from-end (start 0) end count key)
  (let ((key (or key #'identity)))
    (loop
       with it = (iterator sequence :from-end from-end :start start :end end)
       with n = 0

       until (endp it)
       do
	 (when (funcall test (funcall key (at it)))
	   (setf (at it) new)
	   (when (and count (cl:= (cl:incf n) count))
	     (loop-finish)))

	 (advance it))

    sequence))

(defmethod nsubstitute-if-not (new test sequence &key from-end (start 0) end count key)
  (nsubstitute-if new (test-not test) sequence
		  :from-end from-end
		  :start start
		  :end end
		  :count count
		  :key key))


(defmethod substitute (new old sequence &key from-end (test #'equalp) (start 0) end count key)
  (substitute-if new (test-eq test old) sequence
		 :from-end from-end
		 :start start
		 :end end
		 :count count
		 :key key))

(defmethod substitute-if (new test sequence &key from-end (start 0) end count key)
  (let ((key (or key #'identity))
	(collector (make-collector (empty-clone sequence) :front from-end)))

    (flet ((substitute ()
	     (let ((n 0))
	       (doseq (item sequence :start start :end end :from-end from-end)
		 (cond
		   ((funcall test (funcall key item))
		    (collect collector new)

		    (and count (cl:= (cl:incf n) count) (return)))

		   (t
		    (collect collector item)))))))

      (collect-perform-op collector sequence #'substitute :start start :end end :from-end from-end)
      (collector-sequence collector))))

(defmethod substitute-if-not (new test sequence &key from-end (start 0) end count key)
  (substitute-if new (test-not test) sequence
		 :from-end from-end
		 :start start
		 :end end
		 :count count
		 :key key))


;; Removing Items

(defmethod remove (item sequence &key from-end (test #'equalp) (start 0) end count key)
  (remove-if (test-eq test item) sequence
	     :from-end from-end
	     :start start
	     :end end
	     :count count
	     :key key))

(defmethod remove-if (test sequence &key from-end (start 0) end count key)
  (let ((key (or key #'identity))
	(collector (make-collector (empty-clone sequence) :front from-end)))

    (flet ((remove-items ()
	     (let ((n 0))
	       (doseq (item sequence :start start :end end :from-end from-end)
		 (if (funcall test (funcall key item))
		     (and count (cl:= (cl:incf n) count) (return))
		     (collect collector item))))))

      (collect-perform-op collector sequence #'remove-items :start start :end end :from-end from-end)
      (collector-sequence collector))))

(defmethod remove-if-not (test sequence &key from-end (start 0) end count key)
  (remove-if (test-not test) sequence
	     :from-end from-end
	     :start start
	     :end end
	     :count count
	     :key key))

(defmethod delete (item sequence &key from-end (test #'equalp) (start 0) end count key)
  (remove item sequence
	  :from-end from-end
	  :test test
	  :start start
	  :end end
	  :count count
	  :key key))

(defmethod delete-if (test sequence &key from-end (start 0) end count key)
  (remove-if test sequence
	     :from-end from-end
	     :start start
	     :end end
	     :count count
	     :key key))

(defmethod delete-if-not (test sequence &key from-end (start 0) end count key)
  (remove-if-not test sequence
		 :from-end from-end
		 :start start
		 :end end
		 :count count
		 :key key))


;; Removing Duplicates

(defmethod remove-duplicates (sequence &key from-end (test #'equal) (start 0) end key)
  (let ((key (or key #'identity))
	(collector (make-collector (empty-clone sequence) :front from-end)))

    (flet ((remove-duplicates ()
	     (let ((items (make-hash-table :test test)))
	       (doseq (item sequence :start start :end end :from-end from-end)
		 (unless (nth-value 1 (ensure-gethash (funcall key item) items))
		   (collect collector item))))))
      (collect-perform-op collector sequence #'remove-duplicates :start start :end end :from-end from-end)
      (collector-sequence collector))))

(defmethod delete-duplicates (sequence &key from-end (test #'equal) (start 0) end key)
  (remove-duplicates sequence
		     :from-end from-end
		     :test test
		     :start start
		     :end end
		     :key key))


;; Logical Sequence Operations

(defun every (test &rest seqs)
  "Same as CL:EVERY except it can be applied to any sequence for which
   there the iterator interface is implemented."

  (loop
     with iters = (make-iters seqs)
     until (some-endp iters)
     do
       (unless (apply test (get-elements iters))
	 (return-from every nil))
       (advance-all iters))
  t)

(defun some (test &rest seqs)
  "Same as CL:SOME except it can be applied to any sequence for which
   there the iterator interface is implemented."

  (loop
     with iters = (make-iters seqs)
     until (some-endp iters)
     do
       (aand (apply test (get-elements iters))
	     (return-from some it))
       (advance-all iters))
  nil)

(defun notevery (test &rest seqs)
  "Same as CL:NOTEVERY except it can be applied to any sequence for
   which there the iterator interface is implemented."

  (loop
     with iters = (make-iters seqs)
     until (some-endp iters)
     do
       (unless (apply test (get-elements iters))
	 (return-from notevery t))
       (advance-all iters))
  nil)

(defun notany (test &rest seqs)
  "Same as CL:NOTANY except it can be applied to any sequence for which
   there the iterator interface is implemented."

  (loop
     with iters = (make-iters seqs)
     until (some-endp iters)
     do
       (when (apply test (get-elements iters))
	 (return-from notany nil))
       (advance-all iters))
  t)


;;; Concatenation

(defun concatenate (sequence &rest sequences)
  "Returns a new sequence containing all the elements of SEQUENCE and
   of each sequence in SEQUENCES, in the order they are supplied."

  (apply #'nconcatenate (empty-clone sequence) sequence sequences))

(defun nconcatenate (result &rest sequences)
  "Destructively concatenates each sequence in SEQUENCES to the
   sequence RESULT."

  (let ((collector (make-collector result)))
    (dolist (seq sequences)
      (extend collector seq))
    (collector-sequence collector)))

(defun concatenate-to (type &rest sequences)
  "Returns a sequence of type TYPE containing all the elements of each
   sequence in SEQUENCES, in the order they are supplied."

  (apply #'nconcatenate (sequence-of-type type) sequences))


;;; Mapping

(defun map-into (result function &rest sequences)
  "Destructively replaces each element of RESULT with the result of
   applying FUNCTION to each element of RESULT and of each sequence in
   SEQUENCE.

   The shortest sequence of RESULT and SEQUENCE determines how many
   times FUNCTION is applied and how many elements are in the
   resulting sequence. If RESULT is longer than any sequence in
   SEQUENCE the remaining elements are unmodified.

   Unlike CL:MAP-INTO, if RESULT is a vector then FUNCTION is only
   applied on the elements up-to the fill-pointer, i.e. the
   fill-pointer is not ignored.

   Returns RESULT."

  (loop
     with iters = (make-iters (cons result sequences))
     with res-it = (first iters)
     until (some-endp iters)
     do
       (setf (at res-it) (apply function (get-elements iters)))
       (advance-all iters))

  result)

(defgeneric map-to (result function &rest sequences)
  (:documentation
   "Applies FUNCTION to each element of each sequence in SEQUENCES and
    stores the result in RESULT.

    If RESULT is a sequence, the results are directly stored in the
    sequence.

    If RESULT is a symbol designating a sequence type, a new sequence
    of that type is created and the result of applying FUNCTION is
    stored in that sequence.

    Returns the sequence in which the results of applying function are
    stored."))

(defmethod map-to (result function &rest sequences)
  (let ((collector (make-collector result)))
    (loop
       with iters = (make-iters sequences)
       until (some-endp iters)
       do
	 (collect collector (apply function (get-elements iters)))
	 (advance-all iters))

    (collector-sequence collector)))

(defmethod map-to ((type symbol) function &rest sequences)
  (apply #'map-to (sequence-of-type type) function sequences))

(defun map (function sequence &rest sequences)
  "Creates a new sequence, of the same type as SEQUENCE (by
   EMPTY-CLONE), containing the result of applying FUNCTION to each
   element of SEQUENCE and each element of SEQUENCES."

  (apply #'map-to (empty-clone sequence) function (cons sequence sequences)))


;;;; Iteration Utility Functions

(defun make-iters (seqs)
  "Returns a list of iterators for each sequence in SEQS."

  (mapcar #'iterator seqs))

(defun get-elements (iters)
  "Returns a list containing the elements at the positions of each
   iterator in ITERS (by AT)."

  (mapcar #'at iters))

(defun some-endp (iters)
  "Returns true if at least on of the iterators in ITERS is at the end
   of its sequence (by ENDP."

  (cl:some #'endp iters))

(defun advance-all (iters)
  "Advances each iterator in ITERS to its next position (by ADVANCE)."

  (mapc #'advance iters))


;;;; Utility Functions

(defun collect-perform-op (collector sequence op &key start end from-end)
  "Collects the elements of SEQUENCE in the range [0, START), and from
   END till the end of the sequence. Calls OP to perform an operation
   on the remaining range.

   If FROM-END is true COLLECTOR should be a collector to the front of
   a sequence."

  (flet ((copy-till-start ()
	   (extend collector (iterator sequence :start 0 :end start :from-end from-end)))

	 (copy-till-end ()
	   (when end
	     (extend collector (iterator sequence :start end :from-end from-end)))))
    (cond
      (from-end
       (copy-till-end)
       (funcall op)
       (copy-till-start))

      (t
       (copy-till-start)
       (funcall op)
       (copy-till-end)))))