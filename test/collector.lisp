;;;; collector.lisp
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

;;;; Unit tests for the collector interface

(in-package :generic-cl.test)

(plan nil)

(subtest "Test Collector Interface"
  (subtest "List Collector"
    (diag "Test EMPTY-CLONE")
    (is (empty-clone '(1 2 3 4)) nil)
    (is (empty-clone (cons 'a 'b)) nil)
    (is (empty-clone nil) nil)

    (diag "Back Collector")
    (let ((collector (make-collector (list 1 2 3))))
      (accumulate collector 4)
      (accumulate collector 5)
      (accumulate collector 6)
      (extend collector '(7 8))
      (extend collector #(9 10))
      (is (collector-sequence collector) '(1 2 3 4 5 6 7 8 9 10)))

    (diag "Front Collector")
    (let ((collector (make-collector (list 3 2 1) :front t)))
      (accumulate collector 4)
      (accumulate collector 5)
      (accumulate collector 6)
      (extend collector '(7 8))
      (extend collector #(9 10))
      (is (collector-sequence collector) '(10 9 8 7 6 5 4 3 2 1))))

  (subtest "Vector Collector"
    (diag "Test EMPTY-CLONE")
    (let ((clone (empty-clone #(1 2 3 4))))
      (is-type clone 'array)
      (is (array-dimension clone 0) 4 "Same Length")
      (ok (adjustable-array-p clone) "Adjustable")
      (is (fill-pointer clone) 0 "Fill-Pointer"))

    (diag "Back Collector")
    (let ((collector (make-collector (make-array '3 :adjustable t :fill-pointer t :initial-contents '(1 2 3)))))
      (accumulate collector 4)
      (accumulate collector 5)
      (accumulate collector 6)
      (extend collector '(7 8))
      (extend collector #(9 10))
      (is (collector-sequence collector) #(1 2 3 4 5 6 7 8 9 10) :test #'equalp))

    (diag "Front Collector")
    (let ((collector (make-collector (make-array '3 :adjustable t :fill-pointer t :initial-contents '(3 2 1)) :front t)))
      (accumulate collector 4)
      (accumulate collector 5)
      (accumulate collector 6)
      (extend collector '(7 8))
      (extend collector #(9 10))
      (is (collector-sequence collector) #(10 9 8 7 6 5 4 3 2 1) :test #'equalp)))

  (subtest "Hash-Map Collector"
    (diag "Test EMPTY-CLONE")
    (let ((clone (empty-clone (alist-hash-map '((a . 1) (b . 2) (c . 3))))))
      (is-type clone 'hash-map)
      (is (length clone) 0 "Empty"))

    (diag "Collector")
    (let ((collector (make-collector (make-hash-map))))
      (accumulate collector '(a . 1))
      (accumulate collector '(b . 2))
      (accumulate collector '(c . 3))
      (extend collector '((d . 4) ("e" . 5)))
      (is (collector-sequence collector)
	  (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4) ("e" . 5)))
	  :test #'equalp))))

(finalize)
