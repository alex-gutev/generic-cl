;;;; cl-sequences.lisp
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

;;;; Common Lisp Sequence Methods

;;; Element Access

(defmethod elt ((sequence sequence) index)
  (cl:elt sequence index))

(defmethod (setf elt) (value (sequence sequence) index)
  (setf (cl:elt sequence index) value))


(defmethod first ((list list))
  (cl:first list))

(defmethod first ((vec vector))
  (aref vec 0))

(defmethod first ((array array))
  (row-major-aref array 0))


(defmethod last ((list list))
  (lastcar list))

(defmethod last ((vec vector))
  (aref vec (1- (length vec))))

(defmethod last ((array array))
  (row-major-aref array (1- (array-total-size array))))


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


;;; Subsequence

(defmethod subseq ((seq sequence) start &optional end)
  (cl:subseq seq start end))

(defmethod (setf subseq) (value (seq sequence) start &optional end)
  (setf (cl:subseq seq start end) value))


;;; Sequence Operations

;; Replacing elements of a sequence

(defmethod fill ((seq sequence) item &key (start 0) end)
  (cl:fill seq item :start start :end end))

(defmethod replace ((seq1 sequence) (seq2 sequence) &key (start1 0) (start2 0) end1 end2)
  (cl:replace seq1 seq2 :start1 start1 :start2 start2 :end1 end1 :end2 end2))


;; Reduction

(defmethod reduce (function (sequence sequence) &rest args &key key from-end (start 0) end initial-value)
  (declare (ignore key from-end start end initial-value))

  (apply #'cl:reduce function sequence args))


;; Count

(defmethod count (item (sequence sequence) &key from-end (start 0) end (test #'equalp) key)
  (cl:count item sequence
	    :from-end from-end
	    :start start
	    :end end
	    :key key
	    :test test))

(defmethod count-if (predicate (sequence sequence) &key from-end (start 0) end key)
  (cl:count-if predicate sequence :from-end from-end :start start :end end :key key))

(defmethod count-if-not (predicate (sequence sequence) &key from-end (start 0) end key)
  (cl:count-if-not predicate sequence :from-end from-end :start start :end end :key key))


;; Find

(defmethod find (item (sequence sequence) &key from-end (start 0) end (test #'equalp) key)
  (cl:find item sequence
	   :from-end from-end
	   :test test
	   :start start
	   :end end
	   :key key))

(defmethod find-if (predicate (sequence sequence) &key from-end (start 0) end key)
  (cl:find-if predicate sequence :from-end from-end :start start :end end :key key))

(defmethod find-if-not (predicate (sequence sequence) &key from-end (start 0) end key)
  (cl:find-if-not predicate sequence :from-end from-end :start start :end end :key key))


;; Position

(defmethod position (item (sequence sequence) &key from-end (start 0) end (test #'equalp) key)
  (cl:position item sequence
	       :from-end from-end
	       :test test
	       :start start
	       :end end
	       :key key))

(defmethod position-if (predicate (sequence sequence) &key from-end (start 0) end key)
  (cl:position-if predicate sequence :from-end from-end :start start :end end :key key))

(defmethod position-if-not (predicate (sequence sequence) &key from-end (start 0) end key)
  (cl:position-if-not predicate sequence :from-end from-end :start start :end end :key key))


;; Search for/Comparing subsequences

(defmethod search ((seq1 sequence) (seq2 sequence) &key from-end (test #'equalp) key (start1 0) (start2 0) end1 end2)
  (cl:search seq1 seq2
	     :from-end from-end
	     :test test
	     :key key
	     :start1 start1
	     :start2 start2
	     :end1 end1
	     :end2 end2))

(defmethod mismatch ((seq1 sequence) (seq2 sequence) &key from-end (test #'equalp) key (start1 0) (start2 0) end1 end2)
  (cl:mismatch seq1 seq2
	       :from-end from-end
	       :test test
	       :key (or key #'identity)
	       :start1 start1
	       :start2 start2
	       :end1 end1
	       :end2 end2))


;; Reversing

(defmethod reverse ((seq sequence))
  (cl:reverse seq))

(defmethod nreverse ((seq sequence))
  (cl:nreverse seq))


;; Sorting

(defmethod merge ((seq1 sequence) (seq2 sequence) predicate &key key)
  (nmerge (copy-seq seq1) (copy-seq seq2) predicate :key key))

(defmethod nmerge ((seq1 sequence) (seq2 sequence) predicate &key key)
  (-> (typecase seq1
	(vector 'vector)
	(otherwise 'list))
      (cl:merge seq1 seq2 predicate :key key)))


(defmethod sort ((seq sequence) &key (test #'<) key)
  (cl:sort (copy-seq seq) test :key key))

(defmethod stable-sort ((seq sequence) &key (test #'<) key)
  (cl:stable-sort (copy-seq seq) test :key key))

(defmethod nsort ((seq sequence) &key (test #'<) key)
  (cl:sort seq test :key key))

(defmethod stable-nsort ((seq sequence) &key (test #'<) key)
  (cl:stable-sort seq test :key key))


;; Substitute

(defmethod substitute (new old (seq sequence) &key from-end (test #'equalp) (start 0) end count key)
  (cl:substitute new old seq
		 :from-end from-end
		 :test test
		 :start start
		 :end end
		 :count count
		 :key key))

(defmethod nsubstitute (new old (seq sequence) &key from-end (test #'equalp) (start 0) end count key)
  (cl:nsubstitute new old seq
		 :from-end from-end
		 :test test
		 :start start
		 :end end
		 :count count
		 :key key))

(defmethod substitute-if (new predicate (sequence sequence) &key from-end (start 0) end count key)
  (cl:substitute-if new predicate sequence
		    :from-end from-end
		    :start start
		    :end end
		    :count count
		    :key key))

(defmethod nsubstitute-if (new predicate (sequence sequence) &key from-end (start 0) end count key)
  (cl:nsubstitute-if new predicate sequence
		    :from-end from-end
		    :start start
		    :end end
		    :count count
		    :key key))

(defmethod substitute-if-not (new predicate (sequence sequence) &key from-end (start 0) end count key)
  (cl:substitute-if-not new predicate sequence
			:from-end from-end
			:start start
			:end end
			:count count
			:key key))

(defmethod nsubstitute-if-not (new predicate (sequence sequence) &key from-end (start 0) end count key)
  (cl:nsubstitute-if-not new predicate sequence
			:from-end from-end
			:start start
			:end end
			:count count
			:key key))

;; Removing Items

(defmethod remove (item (sequence sequence) &key from-end (test #'equalp) end (start 0) count key)
  (cl:remove item sequence
	     :from-end from-end
	     :test test
	     :end end
	     :start start
	     :count count
	     :key key))

(defmethod delete (item (sequence sequence) &key from-end (test #'equalp) end (start 0) count key)
  (cl:delete item sequence
	     :from-end from-end
	     :test test
	     :end end
	     :start start
	     :count count
	     :key key))

(defmethod remove-if (test (sequence sequence) &key from-end end (start 0) count key)
  (cl:remove-if test sequence
		:from-end from-end
		:end end
		:start start
		:count count
		:key key))

(defmethod delete-if (test (sequence sequence) &key from-end end (start 0) count key)
  (cl:delete-if test sequence
		:from-end from-end
		:end end
		:start start
		:count count
		:key key))

(defmethod remove-if-not (test (sequence sequence) &key from-end end (start 0) count key)
  (cl:remove-if-not test sequence
		    :from-end from-end
		    :end end
		    :start start
		    :count count
		    :key key))

(defmethod delete-if-not (test (sequence sequence) &key from-end end (start 0) count key)
  (cl:delete-if-not test sequence
		    :from-end from-end
		    :end end
		    :start start
		    :count count
		    :key key))

;; Removing Duplicates

(defmethod remove-duplicates ((seq sequence) &key from-end (test #'equalp) (start 0) end key)
  (cl:remove-duplicates seq
			:from-end from-end
			:test test
			:start start
			:end end
			:key key))

(defmethod delete-duplicates ((seq sequence) &key from-end (test #'equalp) (start 0) end key)
  (cl:delete-duplicates seq
			:from-end from-end
			:test test
			:start start
			:end end
			:key key))
