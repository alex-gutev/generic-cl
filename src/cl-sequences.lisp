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


;;; Length

(defmethod length ((sequence sequence))
  "Generic CL:SEQUENCE method, calls CL:LENGTH."

  (cl:length sequence))

(defmethod length ((hash hash-table))
  "Returns the number of entries in the hash-table HASH."

  (hash-table-count hash))

(defmethod length ((vec vector))
  "Returns the number of elements in the vector VEC."

  (cl:length vec))

(defmethod length ((array array))
  "Returns the total number of elements in the multi-dimensional array
   ARRAY."

  (array-total-size array))


;;; Sequence Operations

(defmethod reduce (function (sequence sequence) &rest args &key key from-end (start 0) end initial-value)
  (declare (ignore key from-end start end initial-value))

  (apply #'cl:reduce function sequence args))


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


(defmethod mismatch ((seq1 sequence) (seq2 sequence) &key from-end (test #'equalp) key (start1 0) (start2 0) end1 end2)
  (cl:mismatch seq1 seq2
	       :from-end from-end
	       :test test
	       :key (or key #'identity)
	       :start1 start1
	       :start2 start2
	       :end1 end1
	       :end2 end2))
