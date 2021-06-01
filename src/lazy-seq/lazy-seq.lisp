;;;; lazy-seq.lisp
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

;;;; Lazy Sequences

(in-package :generic-cl.lazy-seq)

;;;; Lazy Sequence

(defstruct (lazy-seq (:copier nil))
  "Lazy sequence where the elements are only computed when they are
   actually referenced. The first element of the sequence is stored in
   HEAD. TAIL stores a function which, when called, returns the
   `LAZY-SEQ' containing the remaining elements in the sequence. If
   there are no remaining elements the function in TAIL returns NIL."

  head
  tail)

(defmacro! thunk (form)
  "Creates a function, with no arguments, which, when called for the
   first time returns the result of evaluating FORM. This result is
   stored such that in future invocations of the function, it will be
   returned without re-evaluating FORM."

  `(let ((,g!result) (,g!computed?))
     (lambda ()
       (if ,g!computed?
	   ,g!result
	   (prog1 (setf ,g!result ,form)
	     (setf ,g!computed? t))))))

(defmacro lazy-seq (head &optional tail)
  "Creates a `LAZY-SEQ' where HEAD is the first element in the
   sequence and TAIL is a form which computes the remainder of the
   sequence. TAIL is wrapped in a function that stores the result of
   evaluating FORM, the first time it is invoked, and returns the
   stored result in future invocations."

  `(make-lazy-seq :head ,head :tail (thunk ,tail)))


;;;; Lazy Sequence Iterator

(defstruct (lazy-seq-iterator (:include iterator))
  "Unbounded lazy sequence iterator. Iterates through all elements of
   the `LAZY-SEQ' SEQ till the end of the sequence."

  seq)

(defstruct (bound-lazy-seq-iterator (:include lazy-seq-iterator))
  "Lazy sequence iterator which iterates through the elements of the
   sequence up till the element at index END."

  index
  end)


(defmethod make-iterator ((seq lazy-seq) start (end null))
  (make-lazy-seq-iterator :seq (sub-lazy-seq seq start)))

(defmethod make-iterator ((seq lazy-seq) start (end number))
  (make-bound-lazy-seq-iterator :seq (sub-lazy-seq seq start) :index start :end end))

(defmethod make-reverse-iterator ((seq lazy-seq) start end)
  "Accumulates the elements of SEQ, between START and END, into a
   list, in reverse order, and returns an iterator for the list."

  (-> (sub-lazy-seq seq start)
      (lazy-seq->list (if end (cl:- end start)))
      iterator))


(defun sub-lazy-seq (seq start)
  "Returns the subsequence of SEQ beginning at the element at index
   START."

  (loop
     for subseq = seq then (funcall (lazy-seq-tail subseq))
     repeat start
     while subseq
     finally (return subseq)))

(defun lazy-seq->list (seq count)
  "Accumulates COUNT elements of the `LAZY-SEQ' SEQ into a list, in
   reverse order."

  (nlet collect
      ((i 0)
       (seq seq)
       (list nil))

    (if (and (or (null count) (cl:< i count)) seq)
	(collect (cl:1+ i) (funcall (lazy-seq-tail seq)) (cons (lazy-seq-head seq) list))
	list)))


(defmethod at ((it lazy-seq-iterator))
  (-> it
      lazy-seq-iterator-seq
      lazy-seq-head))


(defmethod endp ((it lazy-seq-iterator))
  (null (lazy-seq-iterator-seq it)))

(defmethod endp ((it bound-lazy-seq-iterator))
  (or (call-next-method)
      (cl:>= (bound-lazy-seq-iterator-index it)
	     (bound-lazy-seq-iterator-end it))))


(defmethod advance ((it lazy-seq-iterator))
  (with-accessors ((seq lazy-seq-iterator-seq)) it
    (when seq
      (setf seq (funcall (lazy-seq-tail seq))))))

(defmethod advance ((it bound-lazy-seq-iterator))
  (call-next-method)
  (cl:incf (bound-lazy-seq-iterator-index it)))


(defmethod subseq ((it lazy-seq-iterator) start &optional end)
  (make-iterator (lazy-seq-iterator-seq it) start end))

(defmethod subseq ((it bound-lazy-seq-iterator) start &optional end)
  (->> (bound-lazy-seq-iterator-end it)
       (or end)
       (call-next-method it start)))


;;;; Lazy Sequence Collector

(defmethod make-collector ((seq lazy-seq) &key front)
  "Returns a collector for collecting items onto a list containing the
   same elements as the sequence."

  (make-collector (coerce seq 'list) :front front))


;;;; Sequence Function Methods

(defmethod cleared ((seq lazy-seq) &key)
  "Returns the empty list NIL."

  nil)

(defmethod first ((seq lazy-seq))
  (lazy-seq-head seq))

(defmethod emptyp ((seq lazy-seq))
  "Always returns NIL as NIL is used to indicate an empty
   `LAZY-SEQ'. a `LAZY-SEQ' object always contains at least one
   element."

  nil)

(defmethod subseq ((seq lazy-seq) start &optional end)
  (labels ((copy-subseq (seq end)
	     (when (more? seq end nil)
	       (lazy-seq
		(lazy-seq-head seq)
		(copy-subseq (funcall (lazy-seq-tail seq))
			     (and end (cl:1- end)))))))

    (copy-subseq (sub-lazy-seq seq start)
		 (and end (cl:- end start)))))


;;;; Sequence Operations

;;; REMOVE-IF

(defun more? (seq end count)
  "Returns true if there are more elements to be processed in the
   sequence SEQ. END is the index of the last element to process and
   COUNT is the maximum number of elements to be processed."

  (and seq (or (null end) (cl:plusp end)) (or (null count) (cl:plusp count))))

(defun copy-initial-lazy-seq (seq start op)
  "Returns a new `LAZY-SEQ' which contains the first START elements of
   SEQ. The function OP is applied on the remaining sequence of
   elements, of SEQ, to obtain the remainder of the sequence that is
   returned."

  (with-accessors ((head lazy-seq-head)
		   (tail lazy-seq-tail))
      seq

    (if (cl:plusp start)

	(lazy-seq
	 head
	 (copy-initial-lazy-seq
	  (funcall tail)
	  (cl:1- start)
	  op))

	(funcall op seq))))


;;; REMOVE

(defmethod remove-if (test (seq lazy-seq) &key from-end (start 0) end count key)
  "Lazily removes items from a `LAZY-SEQ'. A `LAZY-SEQ' is only
   returned if either FROM-END or COUNT is FALSE."

  (if (and from-end count)
      (call-next-method)

      (copy-initial-lazy-seq
       seq
       start
       (lambda (seq)
	 (remove-from-lazy-seq
	  seq
	  (and end (cl:- end start))
	  test
	  (or key #'identity)
	  count)))))

(defmethod delete-if (test (seq lazy-seq) &key from-end (start 0) end count key)
  "Same as the REMOVE-IF method specialized on `LAZY-SEQ'"

  (remove-if test seq
	     :from-end from-end
	     :start start
	     :end end
	     :count count
	     :key key))

(defun remove-from-lazy-seq (seq end test key count)
  "Returns a `LAZY-SEQ' that contains the elements of SEQ with the
   elements that satisfy TEST removed."

  (with-accessors ((head lazy-seq-head)
		   (tail lazy-seq-tail))
      seq

    (if (more? seq end count)

	(macrolet
	    ((remove-rest (count)
	       `(remove-from-lazy-seq
		 (funcall tail)
		 (and end (cl:1- end))
		 test
		 key
		 ,count)))

	  (if (funcall test (funcall key head))
	      (remove-rest (and count (cl:1- count)))
	      (lazy-seq head (remove-rest count))))

	seq)))


;;; REMOVE-DUPLICATES

(defmethod remove-duplicates ((seq lazy-seq) &key from-end (test #'equalp) (start 0) end key)
  "Lazily removes duplicate items from a `LAZY-SEQ'. Only returns a
   `LAZY-SEQ' if FROM-END is true."

  (if (not from-end)
      (call-next-method)

      (copy-initial-lazy-seq
       seq
       start
       (lambda (seq)
	 (if (hash-map-test-p test)
	     (remove-dups-from-lazy-seq-hash
	      seq
	      (and end (cl:- end start))
	      (or key #'identity)
	      (make-hash-map-table :test test))

	     (remove-dups-from-lazy-seq-list
	      seq
	      (and end (cl:- end start))
	      (or key #'identity)
	      test
	      nil))))))

(defmethod delete-duplicates ((seq lazy-seq) &key from-end (test #'equalp) (start 0) end key)
  "Same as the REMOVE-DUPLICATES method specialized on `LAZY-SEQ'."

  (remove-duplicates seq
		     :from-end from-end
		     :test test
		     :start start
		     :end end
		     :key key))


(defun remove-dups-from-lazy-seq-hash (seq end key items)
  "Returns a `LAZY-SEQ' containing all elements in SEQ with the
   elements in the `HASH-MAP' ITEMS removed."

  (with-accessors ((head lazy-seq-head)
		   (tail lazy-seq-tail))
      seq

    (if (more? seq end nil)
	(symbol-macrolet
	    ((rem (remove-dups-from-lazy-seq-hash (funcall tail) (and end (cl:1- end)) key items)))

	  (with-custom-hash-table
	    (let ((elem (funcall key head)))
	      (cond
		((nth-value 1 (gethash elem items))
		 rem)

		(t
		 (setf (gethash elem items) t)
		 (lazy-seq head rem))))))
	seq)))

(defun remove-dups-from-lazy-seq-list (seq end key test items)
  "Returns a `LAZY-SEQ' containing all elements in SEQ with the
   elements in the `LIST' ITEMS removed."

  (with-accessors ((head lazy-seq-head)
		   (tail lazy-seq-tail))
      seq

    (if (more? seq end nil)
	(macrolet
	    ((remove-rest (items)
	       `(remove-dups-from-lazy-seq-list
		 (funcall tail)
		 (and end (cl:1- end))
		 key
		 test
		 ,items)))

	  (let ((elem (funcall key head)))
	    (cond
	      ((member elem items :test test)
	       (remove-rest items))

	      (t
	       (lazy-seq head (remove-rest (cons elem items)))))))
	seq)))


;;; SUBSTITUTE-IF

(defmethod substitute-if (new test (seq lazy-seq) &key from-end (start 0) end count key)
  "Lazily substitutes elements, in a `LAZY-SEQ' which satisfy TEST,
   with NEW."

  (if (and from-end count)
      (call-next-method)

      (copy-initial-lazy-seq
       seq
       start
       (lambda (seq)
	 (substitue-in-lazy-seq
	  seq
	  (and end (cl:- end start))
	  new
	  test
	  (or key #'identity)
	  count)))))

(defmethod nsubstitute-if (new test (seq lazy-seq) &key from-end (start 0) end count key)
  "Same as the SUBSTITUTE-IF method specialized on `LAZY-SEQ'."

  (substitute-if new test seq
		 :from-end from-end
		 :start start
		 :end end
		 :count count
		 :key key))

(defun substitue-in-lazy-seq (seq end new test key count)
  "Returns a new `LAZY-SEQ' containing all elements of `SEQ' with the
   elements that satisfy TEST replaced with NEW."

  (with-accessors ((head lazy-seq-head)
		   (tail lazy-seq-tail))
      seq

    (if (more? seq end count)
	(macrolet
	    ((substitute-rest (count)
	       `(substitue-in-lazy-seq
		 (funcall tail)
		 (and end (cl:1- end))
		 new
		 test
		 key
		 ,count)))

	  (if (funcall test (funcall key head))
	      (->> (substitute-rest (and count (cl:1- count)))
		   (lazy-seq new))
	      (lazy-seq head (substitute-rest count))))
	seq)))


;;; Concatenation

(defmethod concatenate ((sequence lazy-seq) &rest sequences)
  "Lazily concatenates SEQUENCES to SEQUENCE."

  (apply #'concatenate-to 'lazy-seq sequence sequences))

(defmethod nconcatenate ((sequence lazy-seq) &rest sequences)
  "Lazily concatenates SEQUENCES to SEQUENCE."

  (apply #'concatenate-to 'lazy-seq sequence sequences))

(defmethod concatenate-to ((type (eql 'lazy-seq)) &rest sequences)
  "Lazily concatenates SEQUENCES"

  (labels ((concat-seqs (seqs)
	     (match seqs
	       ((list* seq seqs)
		(concat (iterator seq) seqs))))

	   (concat (it seqs)
	     (if (endp it)
		 (concat-seqs seqs)

		 (lazy-seq
		  (at it)
		  (concat (next-it it) seqs))))

	   (next-it (it)
	     (advance it)
	     it))

    (concat-seqs sequences)))


;;; Mapping

(defmethod map (function (sequence lazy-seq) &rest sequences)
  (apply #'map-to 'lazy-seq function sequence sequences))

(defmethod nmap (function (sequence lazy-seq) &rest sequences)
  (apply #'map-to 'lazy-seq function sequence sequences))

(defmethod map-into ((result lazy-seq) function &rest sequences)
  (->> (apply #'map-to 'lazy-seq function sequences)
       (concatenate-to 'lazy-seq result)))

(defmethod map-to ((type (eql 'lazy-seq)) function &rest sequences)
  (labels ((map-seqs (iters)
	     (unless (some-endp iters)
	       (lazy-seq
		(apply function (get-elements iters))
		(map-seqs (next-iters iters)))))

	   (next-iters (iters)
	     (advance-all iters)
	     iters))

    (map-seqs (make-iters sequences))))


(defmethod map-extend-to ((type (eql 'lazy-seq)) function &rest sequences)
  (map-extend-to-lazy-seq function sequences))

(defun map-extend-to-lazy-seq (function sequences)
  (labels ((map-seqs (iters)
	     (unless (some-endp iters)
	       (extend-seq
		(apply function (get-elements iters))
		(map-seqs (next-iters iters)))))

	   (extend-seq (seq next)
	     (append-seq (iterator seq) next))

	   (append-seq (iter next)
	     (if (endp iter)
		 next

		 (lazy-seq
		  (at iter)
		  (append-seq
		   (prog1 iter
		     (advance iter))
		   next))))

	   (next-iters (iters)
	     (advance-all iters)
	     iters))

    (map-seqs (make-iters sequences))))

(defmethod map-extend-into ((result lazy-seq) function &rest sequences)
  (->> (map-extend-to-lazy-seq function sequences)
       (concatenate-to 'lazy-seq result)))

(defmethod map-extend (function (sequence lazy-seq) &rest sequences)
  (map-extend-to-lazy-seq function (cons sequence sequences)))


;;;; Miscellaneous Methods

;;; Equality

(defmethod equalp ((a lazy-seq) (b lazy-seq))
  (do ((a a (funcall (lazy-seq-tail a)))
       (b b (funcall (lazy-seq-tail b))))
      ((or (null a) (null b)) (and (null a) (null b)))

    (unless (equalp (lazy-seq-head a) (lazy-seq-head b))
      (return nil))))


;;; Copy

(defmethod copy ((seq lazy-seq) &key deep)
  (with-accessors ((head lazy-seq-head)
		   (tail lazy-seq-tail))
      seq

    (lazy-seq
     (if deep (copy head :deep t) head)
     (copy (funcall tail) :deep deep))))


;;; Coerce

(defmethod coerce ((seq lazy-seq) (type (eql 'list)))
  (let ((c (make-collector nil)))
    (do ((seq seq (funcall (lazy-seq-tail seq))))
	((null seq))
      (accumulate c (lazy-seq-head seq)))
    (collector-sequence c)))


;;;; Utilities Implemented Using Lazy Sequences

(defun range (start &optional end (step 1))
  "Returns a `LAZY-SEQ' containing all numbers in the range [START,
   END). If END is NIL then an infinite sequence, without an upper
   bound is returned. STEP is the delta by which each number is
   incremented to obtain the next successive number."

  (when (or (null end) (< start end))
    (lazy-seq start (range (+ start step) end step))))
