;;;; lazy-seqs.lisp
;;;;
;;;; Copyright 2020-2021 Alexander Gutev
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

;;;; Unit tests for lazy sequence utilities

(in-package :generic-cl.util/test)


;;; Test Suite Definition

(def-suite lazy-seq
    :description "Test lazy sequence utilities"
    :in generic-cl.util)

(in-suite lazy-seq)


;;; Tests

(test REPEAT
  "Test REPEAT function"

  (is (= '(1 1 1 1 1) (coerce (repeat 1 5) 'list)))
  (is (= '(x x x x x x) (coerce (subseq (repeat 'x) 0 6) 'list)))

  (is (= '(a a a) (repeat 'a 3 'list)))
  (is (= #(z z z z z) (repeat 'z 5 'vector))))

(test repeatedly
  "Test REPEATEDLY function"

  (let ((n 0))
    (flet ((f ()
	     (prog1 n
	       (incf n))))

      (is (= '(0 1 2 3 4) (coerce (repeatedly #'f 5) 'list)))

      (is (= '(5 6 7 8 9 10 11 12 13 14)
	     (coerce (subseq (repeatedly #'f) 0 10) 'list)))

      (setf n 0)

      (is (= '(0 1 2) (repeatedly #'f 3 'list)))
      (is (= #(3 4 5 6 7) (repeatedly #'f 5 'vector))))))

(test iterate
  "Test ITERATE function"

  (flet ((double (n)
	   (* n 2)))

    (is (= '(2 4 8 16 32)
	   (coerce (subseq (iterate #'double 1) 0 5) 'list)))

    (is (= '(1 2 4 8 16)
	   (coerce (subseq (iterate #'double 1 :initial t) 0 5) 'list)))

    (is (= '(20 40 80 160 320)
	   (coerce (subseq (iterate #'double 10) 0 5) 'list)))

    (is (= '(10 20 40 80 160)
	   (coerce (subseq (iterate #'double 10 :initial t) 0 5) 'list)))))

(test cycle
  "Test CYCLE function"

  (is (= '(a b c d a b c d a b)
	 (coerce (subseq (cycle '(a b c d)) 0 10) 'list)))

  (is (= '(1 2 3 1 2 3 1 2 3 1)
	 (coerce (subseq (cycle #(1 2 3)) 0 10) 'list))))
