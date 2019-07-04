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

;; ELT

(defmethod elt (sequence index)
  "Returns the element at index INDEX of the generic sequence SEQUENCE.

   This is implemented by creating an iterator for sequence with start
   position INDEX, and returning the first element returned by the
   iterator."

  (at (iterator sequence :start index)))

(defmethod (setf elt) (value sequence index)
  "Sets the value of the element at index INDEX of the generic
   sequence SEQUENCE.

   This is implemented by creating an iterator for sequence with start
   position INDEX, and setting the value of the element at the
   iterator's starting position."

  (setf (at (iterator sequence :start index)) value))


;; FIRST

(defmethod first (sequence)
  "Returns the first element of the generic sequence. Implemented
   using ELT."

  (elt sequence 0))


;; LAST

(defmethod last (sequence &optional (n 0))
  "Returns the nth element from the last element of the generic
   sequence. Implemented using ELT and LENGTH."

  (elt sequence (cl:- (length sequence) 1 n)))


;;; Length

(defmethod length (sequence)
  "Returns the number of elements in SEQUENCE, where SEQUENCE is of a
   type for which the iterator interface is implemented.

   The type is computed by iterating through the entire sequence, thus
   this is a linear O(n) operation, in the number of elements in the
   sequence."

  (length (iterator sequence)))

(defmethod length ((it iterator))
  "Returns the number of elements between the current position of the
   iterator and its end. This is achieved by iterating until ENDP
   returns true using a copy of IT."

  (do ((it (copy it))
       (n 0 (cl:1+ n)))
      ((endp it) n)
    (advance it)))

(defmethod emptyp (sequence)
  "Returns true if an iterator created for SEQUENCE is immediately at
   its end position (ENDP returns true)."

  (endp (iterator sequence)))


;;; Subsequence

(defmethod subseq (seq start &optional end)
  (let ((collector (make-collector (cleared seq))))
    (extend collector (iterator seq :start start :end end))
    (collector-sequence collector)))

(defmethod (setf subseq) (new seq start &optional end)
  (doiters ((seq-it seq :start start :end end)
	    (new-it new))
    (setf (at seq-it) (at new-it)))
  new)


;;; Sequence Operations

;; Replacing elements of a sequence

(defmethod fill (seq item &key (start 0) end)
  (doiter (it seq :start start :end end)
    (setf (at it) item))
  seq)

(defmethod replace (seq1 seq2 &key (start1 0) end1 (start2 0) end2)
  (doiters ((it1 seq1 :start start1 :end end1)
	    (it2 seq2 :start start2 :end end2))
    (setf (at it1) (at it2)))
  seq1)


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
		  (setf elem (funcall key (at iter)))
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
	   (if from-end
	       (cl:- (or end (length sequence)) 1 pos)
	       (cl:+ start pos))))
    (let ((key (or key #'identity))
	  (pos 0))
      (doseq (elem sequence :from-end from-end :start start :end end)
	(when (funcall test (funcall key elem))
	  (return (compute-pos pos)))
	(incf pos)))))

(defmethod position-if-not (test sequence &key from-end (start 0) end key)
  (position-if (test-not test) sequence :from-end from-end :start start :end end :key key))


;; Searching for/Comparing subsequences

(defmethod search (seq1 seq2 &key from-end (test #'equalp) key (start1 0) (start2 0) end1 end2)
  (let* ((key (or key #'identity))
	 (it1-start (iterator seq1 :start start1 :end end1 :from-end from-end))
	 (it1 (copy it1-start))
	 (index 0)
	 index-start)

    (flet ((test (a b)
	     (funcall test (funcall key a) (funcall key b)))

	   (compute-pos (start end)
	     (if from-end
		 (cl:- (or end2 (length seq2)) end)
		 (cl:+ start2 start))))

      (cond
	((endp it1) ;; Check whether SEQ1 is empty
	 (compute-pos 0 0))

	(t
	 (doseq (elem2 seq2 :start start2 :end end2 :from-end from-end)
	   (cond
	     ((endp it1)
	      (return-from search (compute-pos index-start index)))

	     ((test (at it1) elem2)
	      (advance it1)
	      (unless index-start (setf index-start index)))

	     (index-start ;; Reset IT1 and INDEX-START
	      (setf it1 (copy it1-start))
	      (setf index-start nil)))

	   (cl:incf index))

	 (when (and index-start (endp it1))
	   (compute-pos index-start index)))))))

(defmethod mismatch (seq1 seq2 &key from-end (test #'equalp) key (start1 0) (start2 0) end1 end2)
  (flet ((compute-pos (pos)
	   (if from-end
	       (cl:- (or end1 (length seq1)) pos)
	       (cl:+ start1 pos))))
    (let ((key (or key #'identity)))
      (loop
	 with it1 = (iterator seq1 :start start1 :end end1 :from-end from-end)
	 with it2 = (iterator seq2 :start start2 :end end2 :from-end from-end)
	 for pos = 0 then (cl:1+ pos)
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
  (let* ((collector (make-collector (cleared seq))))
    (extend collector (iterator seq :from-end t))
    (collector-sequence collector)))

(defmethod nreverse (seq)
  (reverse seq))


;; Sorting

(defmethod merge (seq1 seq2 test &key key)
  (let ((key (or key #'identity))
	(collector (make-collector (cleared seq1)))
	(it1 (iterator seq1))
	(it2 (iterator seq2)))

    (flet ((less (a b)
	     (funcall test (funcall key a) (funcall key b))))
      (loop
	 until (or (endp it1) (endp it2))
	 do
	   (let ((elem1 (at it1))
		 (elem2 (at it2)))
	     (cond
	       ((less elem2 elem1)
		(accumulate collector elem2)
		(advance it2))

	       (t
		(accumulate collector elem1)
		(advance it1)))))

      (cond
	((not (endp it1))
	 (extend collector it1))

	((not (endp it2))
	 (extend collector it2)))

      (collector-sequence collector))))

(defmethod nmerge (seq1 seq2 predicate &key key)
  (merge seq1 seq2 predicate :key key))


(defmethod sort (sequence &key (test #'lessp) key)
  "Returns a sequence, of the same type as sequence, with the elements
   sorted, by the order of TEST, using the merge sort algorithm."

  (labels ((sort (it)
	     (multiple-value-bind (it1 it2) (split it)
	       (if it1
		   (merge-sort it1 it2)
		   (make-seq it))))

	   (merge-sort (it1 it2)
	     (nmerge (sort it1) (sort it2) test :key key))

	   (make-seq (it)
	     (if (endp it)
		 (cleared sequence)
		 (seq-with-item (at it))))

	   (seq-with-item (item)
	     (let ((collector (make-collector (cleared sequence))))
	       (accumulate collector item)
	       (collector-sequence collector)))

	   (split (it)
	     (let ((mid (floor (length it) 2)))
	       (when (cl:plusp mid)
		 (values
		  (subseq it 0 mid)
		  (subseq it mid))))))

    (sort (iterator sequence))))

(defmethod stable-sort (seq &key (test #'lessp) key)
  "Simply calls SORT as the default method is a stable sort."

  (sort seq :test test :key key))

(defmethod nsort (seq &key (test #'lessp) key)
  (sort seq :test test :key key))

(defmethod stable-nsort (seq &key (test #'lessp) key)
  (stable-sort seq :test test :key key))


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
	(collector (make-collector (cleared sequence) :front from-end)))

    (flet ((substitute ()
	     (let ((n 0))
	       (doseq (item sequence :start start :end end :from-end from-end)
		 (cond
		   ((and count (cl:= n count))
		    (accumulate collector item))

		   ((funcall test (funcall key item))
		    (accumulate collector new)
		    (cl:incf n))

		   (t
		    (accumulate collector item)))))))

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
	(collector (make-collector (cleared sequence) :front from-end)))

    (flet ((remove-items ()
	     (let ((n 0))
	       (doseq (item sequence :start start :end end :from-end from-end)
		 (cond
		   ((and count (cl:= n count))
		    (accumulate collector item))

		   ((funcall test (funcall key item))
		    (cl:incf n))

		   (t
		    (accumulate collector item)))))))

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

(defmethod remove-duplicates (sequence &key from-end (test #'equalp) (start 0) end key)
  (let* ((key (or key #'identity))
	 (from-end (not from-end))
	 (collector (make-collector (cleared sequence) :front from-end)))

    (flet ((remove-duplicates-hash ()
	     (let ((items (make-hash-map-table :test test)))
	       (doseq (item sequence :start start :end end :from-end from-end)
		 (with-custom-hash-table
		   (let ((key (funcall key item)))
		    (unless (nth-value 1 (gethash key items))
		      (setf (gethash key items) t)
		      (accumulate collector item)))))))

	   (remove-duplicates-list ()
	     (let ((items nil))
	       (doseq (item sequence :start start :end end :from-end from-end)
		 (let ((elem (funcall key item)))
		  (unless (member elem items :test test)
		    (setf items (cons elem items))
		    (accumulate collector item)))))))

      (-<> (if (hash-map-test-p test)
	       #'remove-duplicates-hash
	       #'remove-duplicates-list)
	   (collect-perform-op collector sequence <> :start start :end end :from-end from-end))
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
  "Returns a new sequence, of the same type as SEQUENCE, containing
   all the elements of SEQUENCE and of each sequence in SEQUENCES, in
   the order they are supplied."

  (apply #'nconcatenate (cleared sequence) sequence sequences))

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

(defun map (function sequence &rest sequences)
  "Creates a new sequence, of the same type as SEQUENCE (by
   CLEARED), containing the result of applying FUNCTION to each
   element of SEQUENCE and each element of each SEQUENCE in
   SEQUENCES."

  (apply #'map-into (cleared sequence) function sequence sequences))

(defun nmap (result function &rest sequences)
  "Destructively replaces each element of RESULT with the result of
   applying FUNCTION to each element of RESULT and each element of
   each sequence in SEQUENCES.

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

(defun map-into (result function &rest sequences)
  "Applies FUNCTION on each element of each sequence in SEQUENCES and
   stores the result in RESULT, using the collector interface."

  (let ((collector (make-collector result)))
    (loop
       with iters = (make-iters sequences)
       until (some-endp iters)
       do
	 (accumulate collector (apply function (get-elements iters)))
	 (advance-all iters))

    (collector-sequence collector)))

(defun map-to (type function &rest sequences)
  "Applies FUNCTION to each element of each sequence in SEQUENCES and
   stores the result in a new sequence of type TYPE.  Returns the
   sequence in which the results of applying function are stored."

  (apply #'map-into (sequence-of-type type) function sequences))

(defun foreach (function &rest sequences)
  "Applies FUNCTION on each element of each sequence in SEQUENCES."

  (loop
     with iters = (make-iters sequences)
     until (some-endp iters)
     do
       (apply function (get-elements iters))
       (advance-all iters)))


;;;; Iteration Utility Functions

(defun make-iters (seqs)
  "Returns a list of iterators for each sequence in SEQS."

  (mapcar #'iterator seqs))

(defun get-elements (iters)
  "Returns a list containing the elements at the positions of each
   iterator in ITERS (by AT)."

  (mapcar #'at iters))

(defun some-endp (iters)
  "Returns true if at least one of the iterators in ITERS is at the
   end of its sequence (by ENDP."

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
