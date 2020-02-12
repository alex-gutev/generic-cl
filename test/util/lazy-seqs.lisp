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

;;;; Unit tests for lazy sequence utilities

(in-package :generic-cl.util.test)

(plan 4)

(subtest "Test REPEAT"
  (is (coerce (repeat 1 5) 'list) '(1 1 1 1 1) :test #'equalp)
  (is (coerce (subseq (repeat 'x) 0 6) 'list) '(x x x x x x) :test #'equalp))

(subtest "Test REPEATEDLY"
  (let ((n 0))
    (flet ((f ()
	     (prog1 n
	       (incf n))))

      (is (coerce (repeatedly #'f 5) 'list) '(0 1 2 3 4) :test #'equalp)
      (is (coerce (subseq (repeatedly #'f) 0 10) 'list)
	  '(5 6 7 8 9 10 11 12 13 14)
	  :test #'equalp))))

(subtest "Test FITERATE"
  (flet ((double (n)
	   (* n 2)))

    (is (coerce (subseq (fiterate #'double 1) 0 5) 'list)
	'(2 4 8 16 32)
	:test #'equalp)

    (is (coerce (subseq (fiterate #'double 10) 0 5) 'list)
	'(20 40 80 160 320)
	:test #'equalp)))

(subtest "Test CYCLE"
  (is (coerce (subseq (cycle '(a b c d)) 0 10) 'list)
      '(a b c d a b c d a b)
      :test #'equalp)

  (is (coerce (subseq (cycle #(1 2 3)) 0 10) 'list)
      '(1 2 3 1 2 3 1 2 3 1)
      :test #'equalp))

(finalize)
