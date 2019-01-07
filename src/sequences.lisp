;;;; sequences.lisp
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


;;;; Generic Sequence Functions

;;; Element Access

(defgeneric elt (sequence index)
  (:documentation
   "Returns the element at index INDEX of SEQUENCE."))

(defgeneric (setf elt) (value sequence index)
  (:documentation
   "Sets the element at index INDEX of sequence SEQUENCE to VALUE."))


;;; Length

(defgeneric length (sequence)
  (:documentation
   "Returns the number of elements in SEQUENCE."))


;;; Subsequence

(defgeneric subseq (sequence start &optional end)
  (:documentation
   "Returns a new sequence that is the sub-sequence of SEQUENCE
    between START and END."))

(defgeneric (setf subseq) (new-sequence sequence start &optional end)
  (:documentation
   "Replaces the elements of SEQUENCE between START and END with the
    elements of NEW-SEQUENCE. The shorter of the length of
    NEW-SEQUENCE and the number of elements between START and END
    determines how many elements of SEQUENCE are actually modified."))


;;; Sequence Operations

;; Replacing elements of a sequence

(defgeneric fill (sequence item &key start end)
  (:documentation
   "Destructively replaces the elements of SEQUENCE between START and
    END with ITEM. Returns SEQUENCE."))

(defgeneric replace (sequence1 sequence2 &key start1 end1 start2 end2)
  (:documentation
   "Same as CL:REPLACE however can is extensible to other sequence
    TYPES besides CL:SEQUENCE."))


;; Reduction

(defgeneric reduce (function sequence &key key from-end start end initial-value)
  (:documentation
   "Same as CL:REDUCE however is extensible to other sequence types
    besides CL:SEQUENCE."))


;; Count

(defgeneric count (item sequence &key from-end start end test key)
  (:documentation
   "Same as CL:COUNT however is extensible to other sequence types
    besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric count-if (predicate sequence &key from-end start end key)
  (:documentation
   "Same as CL:COUNT-IF however is extensible to other sequence types
    besides CL:SEQUENCE."))

(defgeneric count-if-not (predicate sequence &key from-end start end key)
  (:documentation
   "Same as CL:COUNT-IF-NOT however is extensible to other sequence
    types besides CL:SEQUENCE."))

;; Find

(defgeneric find (item sequence &key from-end start end test key)
  (:documentation
   "Same as CL:FIND however is extensible to other sequence types
    besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric find-if (predicate sequence &key from-end start end key)
  (:documentation
   "Same as CL:FIND-IF however is extensible to other sequence types
    besides CL:SEQUENCE."))

(defgeneric find-if-not (predicate sequence &key from-end start end key)
  (:documentation
   "Same as CL:FIND-IF-NOT however is extensible to other sequence
    types besides CL:SEQUENCE."))

;; Position

(defgeneric position (item sequence &key from-end start end test key)
  (:documentation
   "Same as CL:POSITION however is extensible to other sequence types
    besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric position-if (predicate sequence &key from-end start end key)
  (:documentation
   "Same as CL:POSITION-IF however is extensible to other sequence types
    besides CL:SEQUENCE."))

(defgeneric position-if-not (predicate sequence &key from-end start end key)
  (:documentation
   "Same as CL:POSITION-IF-NOT however is extensible to other sequence
    types besides CL:SEQUENCE."))


;; Searching for/Comparing subsequences

(defgeneric search (sequence-1 sequence-2 &key from-end test key start1 start2 end1 end2)
  (:documentation
   "Same as CL:SEARCH however is extensible to other sequence types
    besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric mismatch (sequence-1 sequence-2 &key from-end test key start1 start2 end1 end2)
  (:documentation
   "Same as CL:MISMATCH however is extensible to other sequence types
    besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))


;; Reversing

(defgeneric reverse (sequence)
  (:documentation
   "Returns a new sequence of the same type as SEQUENCE and with the
    same elements in reverse order."))

(defgeneric nreverse (sequence)
  (:documentation
   "Same as REVERSE however SEQUENCE may be modified."))


;; Sorting

(defgeneric sort (sequence &key predicate key)
  (:documentation
   "Returns a new sequence of the same type as SEQUENCE, with the same
    elements sorted according to the order determined by the function
    PREDICATE.

    PREDICATE is a function of two arguments, which should return true
    if and only if the first argument is strictly less than the second.

    If KEY is provided and is not NIL it is called on each element and
    the result returned by the function is passed on to PREDICATE."))

(defgeneric stable-sort (sequence &key predicate key)
  (:documentation
   "Same as SORT however the sort operation is guaranteed to be
    stable, that is the order of elements which compare equal, under
    PREDICATE, will be preserved."))


(defgeneric nsort (sequence &key predicate key)
  (:documentation
   "Same as SORT however is permitted to destructively modify
    SEQUENCE."))

(defgeneric stable-nsort (sequence &key predicate key)
  (:documentation
   "Same as STABLE-SORT however is permitted to destructively modify
    SEQUENCE."))


;; Substitute (Find and Replace)

(defgeneric substitute (new old sequence &key from-end test start end count key)
  (:documentation
   "Same as CL:SUBSTITUTE however is extensible to other sequence
    types besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric nsubstitute (new old sequence &key from-end test start end count key)
  (:documentation
   "Same as CL:NSUBSTITUTE however is extensible to other sequence
    types besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric substitute-if (new predicate sequence &key from-end start end count key)
  (:documentation
   "Same as CL:SUBSTITUTE-IF however is extensible to other sequence
    types besides CL:SEQUENCE."))

(defgeneric nsubstitute-if (new predicate sequence &key from-end start end count key)
  (:documentation
   "Same as CL:NSUBSTITUTE-IF however is extensible to other sequence
    types besides CL:SEQUENCE."))

(defgeneric substitute-if-not (new predicate sequence &key from-end start end count key)
  (:documentation
   "Same as CL:SUBSTITUTE-IF-NOT however is extensible to other
    sequence types besides CL:SEQUENCE."))

(defgeneric nsubstitute-if-not (new predicate sequence &key from-end start end count key)
  (:documentation
   "Same as CL:NSUBSTITUTE-IF-NOT however is extensible to other
    sequence types besides CL:SEQUENCE."))


;; Removing Items

(defgeneric remove (item sequence &key from-end test start end count key)
  (:documentation
   "Same as CL:REMOVE however is extensible to other sequence types
    besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric delete (item sequence &key from-end test start end count key)
  (:documentation
   "Same as CL:DELETE however is extensible to other sequence types
    besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric remove-if (test sequence &key from-end start end count key)
  (:documentation
   "Same as CL:REMOVE-IF however is extensible to other sequence types
    besides CL:SEQUENCE."))

(defgeneric delete-if (test sequence &key from-end start end count key)
  (:documentation
   "Same as CL:DELETE-IF however is extensible to other sequence types
    besides CL:SEQUENCE."))

(defgeneric remove-if-not (test sequence &key from-end start end count key)
  (:documentation
   "Same as CL:REMOVE-IF-NOT however is extensible to other sequence types
    besides CL:SEQUENCE."))

(defgeneric delete-if-not (test sequence &key from-end start end count key)
  (:documentation
   "Same as CL:DELETE-IF-NOT however is extensible to other sequence types
    besides CL:SEQUENCE."))


;; Removing Duplicates

(defgeneric remove-duplicates (sequence &key from-end test start end key)
  (:documentation
   "Same as CL:REMOVE-DUPLICATES however is extensible to other
    sequence types besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))

(defgeneric delete-duplicates (sequence &key from-end test start end key)
  (:documentation
   "Same as CL:DELETE-DUPLICATES however is extensible to other
    sequence types besides CL:SEQUENCE.

    The default TEST function is GENERIC-CL:EQUALP. The TEST-NOT
    argument is removed."))


;; Logical Sequence Operations

(defun every (test &rest seqs)
  "Same as CL:EVERY except it can be applied to any sequence for which
   there the iterator interface is implemented."

  (flet ((some-endp (iters)
	   (loop for it in iters thereis (endp it)))

	 (advance-all (iters)
	   (mapc #'advance iters))

	 (make-iters (seqs)
	   (mapcar #'iterator seqs))

	 (get-elems (iters)
	   (mapcar #'at iters)))

    (loop
       with iters = (make-iters seqs)
       until (some-endp iters)
       do
	 (unless (apply test (get-elems iters))
	   (return-from every nil))
	 (advance-all iters))
    t))

(defun some (test &rest seqs)
  "Same as CL:SOME except it can be applied to any sequence for which
   there the iterator interface is implemented."

  (flet ((some-endp (iters)
	   (loop for it in iters thereis (endp it)))

	 (advance-all (iters)
	   (mapc #'advance iters))

	 (make-iters (seqs)
	   (mapcar #'iterator seqs))

	 (get-elems (iters)
	   (mapcar #'at iters)))

    (loop
       with iters = (make-iters seqs)
       until (some-endp iters)
       do
	 (aand (apply test (get-elems iters))
	       (return-from some it))
	 (advance-all iters))
    nil))

(defun notevery (test &rest seqs)
  "Same as CL:NOTEVERY except it can be applied to any sequence for
   which there the iterator interface is implemented."

  (flet ((some-endp (iters)
	   (loop for it in iters thereis (endp it)))

	 (advance-all (iters)
	   (mapc #'advance iters))

	 (make-iters (seqs)
	   (mapcar #'iterator seqs))

	 (get-elems (iters)
	   (mapcar #'at iters)))

    (loop
       with iters = (make-iters seqs)
       until (some-endp iters)
       do
	 (unless (apply test (get-elems iters))
	   (return-from notevery t))
	 (advance-all iters))
    nil))

(defun notany (test &rest seqs)
  "Same as CL:NOTANY except it can be applied to any sequence for which
   there the iterator interface is implemented."

  (flet ((some-endp (iters)
	   (loop for it in iters thereis (endp it)))

	 (advance-all (iters)
	   (mapc #'advance iters))

	 (make-iters (seqs)
	   (mapcar #'iterator seqs))

	 (get-elems (iters)
	   (mapcar #'at iters)))

    (loop
       with iters = (make-iters seqs)
       until (some-endp iters)
       do
	 (when (apply test (get-elems iters))
	   (return-from notany nil))
	 (advance-all iters))
    t))


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
  "Results a sequence of type TYPE containing all the elements of each
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
