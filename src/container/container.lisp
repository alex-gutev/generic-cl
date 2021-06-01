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

(in-package :generic-cl.container)

;;; Elements

(defgeneric elt (sequence index)
  (:documentation
   "Returns the element at index INDEX of SEQUENCE."))

(defgeneric (setf elt) (value sequence index)
  (:documentation
   "Sets the element at index INDEX of sequence SEQUENCE to VALUE."))

(defgeneric first (sequence)
  (:documentation
   "Returns the first element of SEQUENCE."))

(defgeneric last (sequence &optional n)
  (:documentation
   "Returns the N'th (default 0) element from the last element of
    SEQUENCE."))

(defgeneric erase (sequence index)
  (:documentation
   "Removes the element at index INDEX from the sequence SEQUENCE."))

;;; Length

(defgeneric length (sequence)
  (:documentation
   "Returns the number of elements in SEQUENCE."))

(defgeneric emptyp (sequence)
  (:documentation
   "Returns true if SEQUENCE is empty."))

(defgeneric clear (sequence)
  (:documentation
   "Destructively removes all elements from SEQUENCE."))

(defgeneric adjust-size (sequence size &key element)
  (:documentation
   "Returns a new sequence that is a copy of SEQUENCE with size
    changed to SIZE.

    If SIZE is less than the current number of elements in SEQUENCE,
    the remaining elements after the first SIZE elements, are
    removed. If SIZE is greater than the number of elements in
    SEQUENCE, elements, with value ELEMENT (default NIL), should be
    appended to SEQUENCE such that it has SIZE elements."))

(defgeneric nadjust-size (sequence size &key element)
  (:documentation
   "Same as ADJUST however is permitted to destructively modify
    SEQUENCE."))


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
