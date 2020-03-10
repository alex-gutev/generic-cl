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

(defun repeat (x &optional n)
  "Create a lazy sequence containing N elements with the value X. If N
   is NIL or not provided, an infinite sequence is returned."

  (labels ((infinite-seq ()
	     (lazy-seq x (infinite-seq)))

	   (bounded-seq (n)
	     (when (cl:plusp n)
	       (lazy-seq x (bounded-seq (cl:1- n))))))

    (if n
	(bounded-seq n)
	(infinite-seq))))

(defun repeatedly (f &optional n)
  "Create a lazy sequence containing N elements, with each element
   each being the result of an application of the function F on no
   arguments. If N is NIL or not provided, an infinite sequence is
   returned."

  (labels ((infinite-seq ()
	     (lazy-seq (funcall f) (infinite-seq)))

	   (bounded-seq (n)
	     (when (cl:plusp n)
	       (lazy-seq (funcall f) (bounded-seq (cl:1- n))))))
    (if n
	(bounded-seq n)
	(infinite-seq))))

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
