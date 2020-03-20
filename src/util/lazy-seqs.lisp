;;;; lazy-seqs.lisp
;;;;
;;;; Copyright 2020 Alexander Gutev
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

;;;; Utility functions for creating Lazy Sequences

(in-package :generic-cl.util)


;;; Creating Lazy Sequences

(defun repeat (x &optional n type)
  "Create a lazy sequence containing N elements with the value X. If N
   is NIL or not provided, an infinite sequence is returned.

   If TYPE is non-nil a sequence of type TYPE is returned rather than
   a `LAZY-SEQ'."

  (labels ((infinite-seq ()
	     (lazy-seq x (infinite-seq)))

	   (bounded-seq (n)
	     (when (cl:plusp n)
	       (lazy-seq x (bounded-seq (cl:1- n)))))

	   (other-seq ()
	     (check-type n (integer 0))

	     (let ((c (make-collector (sequence-of-type type))))
	       (loop repeat n
		  do
		    (accumulate c x))

	       (collector-sequence c))))

    (cond
      (type (other-seq))
      (n (bounded-seq n))
      (t (infinite-seq)))))

(defun repeatedly (f &optional n type)
  "Create a lazy sequence containing N elements, with each element
   each being the result of an application of the function F on no
   arguments. If N is NIL or not provided, an infinite sequence is
   returned.

   If TYPE is non-nil a sequence of type TYPE is returned rather than
   a `LAZY-SEQ'."

  (labels ((infinite-seq ()
	     (lazy-seq (funcall f) (infinite-seq)))

	   (bounded-seq (n)
	     (when (cl:plusp n)
	       (lazy-seq (funcall f) (bounded-seq (cl:1- n)))))

	   (other-seq ()
	     (check-type n (integer 0))

	     (let ((c (make-collector (sequence-of-type type))))
	       (loop repeat n
		  do
		    (accumulate c (funcall f)))

	       (collector-sequence c))))
    (cond
      (type (other-seq))
      (n (bounded-seq n))
      (t (infinite-seq)))))

(defun iterate (f x &key initial)
  "Return an infinite lazy sequence where each element is the result
   of applying the function F on the previous element. If INITIAL is
   true the first element is X otherwise the first element is the
   result of applying F on X."

  (labels ((make-seq (x)
	     (lazy-seq x (make-seq (funcall f x)))))

    (make-seq
     (if initial x (funcall f x)))))

(defun fiterate (f x &key initial)
  "Deprecated alias for the ITERATE function"

  (iterate f x :initial initial))

(defun cycle (sequence)
  "Return a lazy sequence containing an infinite repetition of the
   elements in SEQUENCE.

   The resulting sequence contains the elements of SEQUENCE in order,
   with the last element of SEQUENCE followed by the first, and
   remaining, elements."

  (labels ((make-seq (it)
	     (if (endp it)
		 (make-seq (iterator sequence))
		 (prog1
		     (lazy-seq (at it) (make-seq it))
		   (advance it)))))
    (make-seq (iterator sequence))))
