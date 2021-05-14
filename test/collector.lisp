;;;; collector.lisp
;;;;
;;;; Copyright 2019-2021 Alexander Gutev
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

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite collector
    :description "Test collector interface"
    :in generic-cl)

(in-suite collector)


;;; Test List Collector

(test list-cleared
  "Test CLEARED on lists"

  (is (= nil (cleared '(1 2 3 4))))
  (is (= nil (cleared (cons 'a 'b))))
  (is (= nil (cleared nil))))

(test list-back-collector
  "Test collecting to the end of a list"

   (let ((collector (make-collector (list 1 2 3))))
     (accumulate collector 4)
     (accumulate collector 5)
     (accumulate collector 6)
     (extend collector '(7 8))
     (extend collector #(9 10))

     (is (= '(1 2 3 4 5 6 7 8 9 10) (collector-sequence collector)))))

(test list-front-collector
  "Test collecting to the front of a list"

  (let ((collector (make-collector (list 3 2 1) :front t)))
    (accumulate collector 4)
    (accumulate collector 5)
    (accumulate collector 6)
    (extend collector '(7 8))
    (extend collector #(9 10))

    (is (= '(10 9 8 7 6 5 4 3 2 1) (collector-sequence collector)))))


;;; Test Vector Collector

(test vector-cleared
  "Test CLEARED on vectors"

  (let ((clone (cleared #(1 2 3 4))))
    (is (typep clone 'array))
    (is (= 4 (array-dimension clone 0)))
    (is-true (adjustable-array-p clone))
    (is (= 0 (fill-pointer clone)))))

(test vector-back-collector
  "Test collecting to the end of a vector"

  (let ((collector (make-collector (make-array '3 :adjustable t :fill-pointer t :initial-contents '(1 2 3)))))
    (accumulate collector 4)
    (accumulate collector 5)
    (accumulate collector 6)
    (extend collector '(7 8))
    (extend collector #(9 10))

    (is (= #(1 2 3 4 5 6 7 8 9 10) (collector-sequence collector)))))

(test vector-front-collector
  "Test collecting to the front of a vector"

  (let ((collector (make-collector (make-array '3 :adjustable t :fill-pointer t :initial-contents '(3 2 1)) :front t)))
    (accumulate collector 4)
    (accumulate collector 5)
    (accumulate collector 6)
    (extend collector '(7 8))
    (extend collector #(9 10))
    (is (= #(10 9 8 7 6 5 4 3 2 1) (collector-sequence collector)))))


;;; Test Hash Map/Table Collector

(test cleared-hashmap
  "Test CLEARED on Hash-Maps"

  (let ((clone (cleared (alist-hash-map '((a . 1) (b . 2) (c . 3))))))
    (is (typep clone 'hash-map))
    (is (= 0 (length clone)))))

(test hashmap-collector
  "Test collecting to a hash-map"

  (let ((collector (make-collector (make-hash-map))))
    (accumulate collector '(a . 1))
    (accumulate collector '(b . 2))
    (accumulate collector '(c . 3))
    (extend collector '((d . 4) ("e" . 5)))

    (is (= (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4) ("e" . 5)))
	   (collector-sequence collector)))))
