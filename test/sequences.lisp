;;;; sequences.lisp
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

;;;; Unit tests for the generic sequences interface

(in-package :generic-cl.test)

(plan nil)

(defstruct list-wrapper
  list)

(defun list-wrap (&rest elems)
  (make-list-wrapper :list elems))

(defmethod equalp ((a list-wrapper) (b list-wrapper))
  (equalp (list-wrapper-list a) (list-wrapper-list b)))

(defmethod make-iterator ((seq list-wrapper) start end)
  (make-iterator (list-wrapper-list seq) start end))

(defmethod make-reverse-iterator ((seq list-wrapper) start end)
  (make-reverse-iterator (list-wrapper-list seq) start end))


(defmethod empty-clone ((seq list-wrapper))
  (make-list-wrapper))


(defstruct list-wrapper-collector
  collector)

(defmethod make-collector ((seq list-wrapper) &key front)
  (make-list-wrapper-collector :collector (make-collector (list-wrapper-list seq) :front front)))

(defmethod collect ((c list-wrapper-collector) item)
  (collect (list-wrapper-collector-collector c) item))

(defmethod collector-sequence ((c list-wrapper-collector))
  (make-list-wrapper :list (collector-sequence (list-wrapper-collector-collector c))))

(subtest "Test Sequence Functions"
  (subtest "Test FIRST and LAST"
    (is (first '(1 2 3 4)) 1)
    (is (first #(a b c d)) 'a)
    (is (first (make-list-wrapper :list '(x y z))) 'x)

    (is (last '(1 2 3 4)) 4)
    (is (last #(a b c d)) 'd)
    (is (last (make-list-wrapper :list '(x y z))) 'z))

  (subtest "Test LENGTH"
    (is (length '(1 2 3 4)) 4)
    (is (length #(a b c d e)) 5)
    (is (length (make-array 7 :adjustable t :initial-element 0 :fill-pointer 3)) 3)
    (is (length (alist-hash-map '((a . 1) (b . 2) (c . 3)))) 3))

  (subtest "Test SUBSEQ"
    (diag "CL Sequences")
    (is (subseq '(1 2 3 4 5) 1 3) '(2 3))
    (is (subseq '(1 2 3 4 5) 2) '(3 4 5))

    (diag "Generic Sequences")
    (is (subseq (make-list-wrapper :list '(1 2 3 4 5)) 1 3)
	(make-list-wrapper :list '(2 3))
	:test #'equalp)
    (is (subseq (make-list-wrapper :list '(1 2 3 4 5)) 2)
	(make-list-wrapper :list '(3 4 5))
	:test #'equalp)

    (subtest "Test SETF Methods"
      (diag "CL Sequences")
      (let ((list (list 1 2 3 4 5)))
	(setf (subseq list 1 3) '(x y))
	(is list '(1 x y 4 5)))

      (diag "Generic Sequences")
      (let ((wrapper (make-list-wrapper :list '(1 2 3 4 5))))
	(setf (subseq wrapper 1 3) '(x y))
	(is wrapper (make-list-wrapper :list '(1 x y 4 5)) :test #'equalp))))

  (subtest "Test FILL"
    (diag "CL Sequences")
    (is (fill (list 1 2 3 4 5) 'a) '(a a a a a))
    (is (fill (list 1 2 3 4 5) 0 :start 2) '(1 2 0 0 0))
    (is (fill (list 1 2 3 4 5) 0 :start 1 :end 3) '(1 0 0 4 5))

    (diag "Generic Sequences")
    (is (fill (list-wrap 1 2 3 4 5) 'a) (list-wrap 'a 'a 'a 'a 'a) :test #'equalp)
    (is (fill (list-wrap 1 2 3 4 5) 0 :start 2) (list-wrap 1 2 0 0 0) :test #'equalp)
    (is (fill (list-wrap 1 2 3 4 5) 0 :start 1 :end 3) (list-wrap 1 0 0 4 5) :test #'equalp))

  (subtest "Test REPLACE"
    (diag "CL Sequences")

    (is (replace (list 1 2 3 4 5) '(x y)) '(x y 3 4 5))
    (is (replace (list 1 2 3 4 5) '(w x y) :start1 2 :start2 1)
	'(1 2 x y 5))
    (is (replace (list 1 2 3 4 5) '(w x y z) :start1 1 :end1 4 :start2 1 :end2 3)
	'(1 x y 4 5))
    (is (replace (list 1 2 3 4 5) '(w x y z) :start1 1 :end1 2 :start2 1 :end2 3)
	'(1 x 3 4 5))

    (diag "Generic Sequences")

    (is (replace (list-wrap 1 2 3 4 5) '(x y)) (list-wrap 'x 'y 3 4 5)
	:test #'equalp)
    (is (replace (list-wrap 1 2 3 4 5) (list-wrap 'w 'x 'y) :start1 2 :start2 1)
	(list-wrap 1 2 'x 'y 5) :test #'equalp)
    (is (replace (list-wrap 1 2 3 4 5) '(w x y z) :start1 1 :end1 4 :start2 1 :end2 3)
	(list-wrap 1 'x 'y 4 5) :test #'equalp)
    (is (replace (list-wrap 1 2 3 4 5) '(w x y z) :start1 1 :end1 2 :start2 1 :end2 3)
	(list-wrap 1 'x 3 4 5) :test #'equalp))

  (subtest "Test REDUCE"
    (subtest "CL Sequences"
      (is (reduce #'list '(1 2 3 4)) '(((1 2) 3) 4))
      (is (reduce #'list '(1 2 3 4) :initial-value 0) '((((0 1) 2) 3) 4))
      (is (reduce #'list '(1 2 3 4) :initial-value 0 :key #'1+) '((((0 2) 3) 4) 5))
      (is (reduce #'list '(1 2 3 4) :start 2) '(3 4))
      (is (reduce #'list '(1 2 3 4) :start 1 :end 3) '(2 3))
      (is (reduce #'cl:+ nil) 0)
      (is (reduce #'cl:+ '(1)) 1)
      (is (reduce #'cl:+ '(1) :initial-value 2) 3)
      (is (reduce #'list nil :initial-value 1) 1)
      (is (reduce #'list nil :initial-value 1 :key #'1+) 1)

      (diag "From End")

      (is (reduce #'list '(1 2 3 4) :from-end t) '(1 (2 (3 4))))
      (is (reduce #'list '(1 2 3 4) :initial-value 0 :from-end t) '(1 (2 (3 (4 0)))))
      (is (reduce #'list '(1 2 3 4) :initial-value 0 :key #'1+ :from-end t) '(2 (3 (4 (5 0)))))
      (is (reduce #'list '(1 2 3 4) :start 2 :from-end t) '(3 4))
      (is (reduce #'list '(1 2 3 4) :start 1 :end 3 :from-end t) '(2 3))
      (is (reduce #'cl:+ nil :from-end t) 0)
      (is (reduce #'cl:+ '(1) :from-end t) 1)
      (is (reduce #'list '(1) :initial-value 2 :from-end t) '(1 2))
      (is (reduce #'list nil :initial-value 1 :from-end t) 1)
      (is (reduce #'list nil :initial-value 1 :key #'1+ :from-end t) 1))

    (subtest "Generic Sequences"
      (is (reduce #'list (list-wrap 1 2 3 4)) '(((1 2) 3) 4))
      (is (reduce #'list (list-wrap 1 2 3 4) :initial-value 0) '((((0 1) 2) 3) 4))
      (is (reduce #'list (list-wrap 1 2 3 4) :initial-value 0 :key #'1+) '((((0 2) 3) 4) 5))
      (is (reduce #'list (list-wrap 1 2 3 4) :start 2) '(3 4))
      (is (reduce #'list (list-wrap 1 2 3 4) :start 1 :end 3) '(2 3))
      (is (reduce #'cl:+ (list-wrap)) 0)
      (is (reduce #'cl:+ (list-wrap 1)) 1)
      (is (reduce #'cl:+ (list-wrap 1) :initial-value 2) 3)
      (is (reduce #'list (list-wrap) :initial-value 1) 1)
      (is (reduce #'list (list-wrap) :initial-value 1 :key #'1+) 1)

      (diag "From End")

      (is (reduce #'list (list-wrap 1 2 3 4) :from-end t) '(1 (2 (3 4))))
      (is (reduce #'list (list-wrap 1 2 3 4) :initial-value 0 :from-end t) '(1 (2 (3 (4 0)))))
      (is (reduce #'list (list-wrap 1 2 3 4) :initial-value 0 :key #'1+ :from-end t) '(2 (3 (4 (5 0)))))
      (is (reduce #'list (list-wrap 1 2 3 4) :start 2 :from-end t) '(3 4))
      (is (reduce #'list (list-wrap 1 2 3 4) :start 1 :end 3 :from-end t) '(2 3))
      (is (reduce #'cl:+ (list-wrap) :from-end t) 0)
      (is (reduce #'cl:+ (list-wrap 1) :from-end t) 1)
      (is (reduce #'list (list-wrap 1) :initial-value 2 :from-end t) '(1 2))
      (is (reduce #'list (list-wrap) :initial-value 1 :from-end t) 1)
      (is (reduce #'list (list-wrap) :initial-value 1 :key #'1+ :from-end t) 1)))

  (subtest "Test COUNT Functions"
    (subtest "Test COUNT"
      (diag "CL Sequences")
      (is (count "a" '("a" "b" "c" "a" "d")) 2)
      (is (count "a" '("a" "b" "c" "a" "d") :from-end t) 2)
      (is (count "a" '("a" "b" "c" "a" "d") :start 1) 1)
      (is (count "a" '("a" "b" "c" "a" "d") :start 1 :end 3) 0)
      (is (count 1 '(0 1 2 0 3) :key #'1+) 2)

      (diag "Generic Sequences")
      (is (count "a" (list-wrap "a" "b" "c" "a" "d")) 2)
      (is (count "a" (list-wrap "a" "b" "c" "a" "d") :from-end t) 2)
      (is (count "a" (list-wrap "a" "b" "c" "a" "d") :start 1) 1)
      (is (count "a" (list-wrap "a" "b" "c" "a" "d") :start 1 :end 3) 0)
      (is (count 1 (list-wrap 0 1 2 0 3) :key #'1+) 2))

    (subtest "Test COUNT-IF"
      (diag "CL Sequences")
      (is (count-if #'evenp '(1 2 3 4 5)) 2)
      (is (count-if #'evenp '(1 2 3 4 5) :from-end t) 2)
      (is (count-if #'evenp '(1 2 3 4 5) :start 2) 1)
      (is (count-if #'evenp '(1 2 3 4 5) :start 2 :end 3) 0)
      (is (count-if (curry #'< 2) '(0 1 2 0 3) :key #'1+) 2)

      (diag "Generic Sequences")
      (is (count-if #'evenp (list-wrap 1 2 3 4 5)) 2)
      (is (count-if #'evenp (list-wrap 1 2 3 4 5) :from-end t) 2)
      (is (count-if #'evenp (list-wrap 1 2 3 4 5) :start 2) 1)
      (is (count-if #'evenp (list-wrap 1 2 3 4 5) :start 2 :end 3) 0)
      (is (count-if (curry #'< 2) (list-wrap 0 1 2 0 3) :key #'1+) 2))

    (subtest "Test COUNT-IF-NOT"
      (diag "CL Sequences")
      (is (count-if-not #'evenp '(1 2 3 4 5)) 3)
      (is (count-if-not #'evenp '(1 2 3 4 5) :from-end t) 3)
      (is (count-if-not #'evenp '(1 2 3 4 5) :start 2) 2)
      (is (count-if-not #'evenp '(1 2 3 4 5) :start 2 :end 3) 1)
      (is (count-if-not (curry #'< 2) '(0 1 2 0 3) :key #'1+) 3)

      (diag "Generic Sequences")
      (is (count-if-not #'evenp (list-wrap 1 2 3 4 5)) 3)
      (is (count-if-not #'evenp (list-wrap 1 2 3 4 5) :from-end t) 3)
      (is (count-if-not #'evenp (list-wrap 1 2 3 4 5) :start 2) 2)
      (is (count-if-not #'evenp (list-wrap 1 2 3 4 5) :start 2 :end 3) 1)
      (is (count-if-not (curry #'< 2) (list-wrap 0 1 2 0 3) :key #'1+) 3)))

  (subtest "Test FIND Functions"
    (subtest "Test FIND"
      (diag "CL Sequences")
      (is (find "a" '("b" "a" "c" "d" "a")) "a")
      (is (find "a" '("b" "a" "c" "d" "a") :from-end t) "a")
      (is (find "a" '("b" "a" "c" "d" "a") :start 2) "a")
      (is (find "a" '("b" "a" "c" "d" "a") :start 2 :end 3) nil)
      (is (find 2 '(1 3 5 6) :key #'1+) 1)

      (diag "Generic Sequences")
      (is (find "a" (list-wrap "b" "a" "c" "d" "a")) "a")
      (is (find "a" (list-wrap "b" "a" "c" "d" "a") :from-end t) "a")
      (is (find "a" (list-wrap "b" "a" "c" "d" "a") :start 2) "a")
      (is (find "a" (list-wrap "b" "a" "c" "d" "a") :start 2 :end 3) nil)
      (is (find 2 (list-wrap 1 3 5 6) :key #'1+) 1))

    (subtest "Test FIND-IF"
      (diag "CL Sequences")
      (is (find-if #'evenp '(1 3 4 5 6 7)) 4)
      (is (find-if #'evenp '(1 3 4 5 6 7) :from-end t) 6)
      (is (find-if #'evenp '(1 3 4 5 6 7) :start 3) 6)
      (is (find-if #'evenp '(1 3 4 5 6 7) :start 3 :end 4) nil)
      (is (find-if #'evenp '(1 3 4 5 6 7) :key #'1+) 1)

      (diag "Generic Sequences")
      (is (find-if #'evenp (list-wrap 1 3 4 5 6 7)) 4)
      (is (find-if #'evenp (list-wrap 1 3 4 5 6 7) :from-end t) 6)
      (is (find-if #'evenp (list-wrap 1 3 4 5 6 7) :start 3) 6)
      (is (find-if #'evenp (list-wrap 1 3 4 5 6 7) :start 3 :end 4) nil)
      (is (find-if #'evenp (list-wrap 1 3 4 5 6 7) :key #'1+) 1))

    (subtest "Test FIND-IF-NOT"
      (diag "CL Sequences")
      (is (find-if-not #'evenp '(1 3 4 5 6 7)) 1)
      (is (find-if-not #'evenp '(1 3 4 5 6 7) :from-end t) 7)
      (is (find-if-not #'evenp '(1 3 4 5 6 7) :start 3) 5)
      (is (find-if-not #'evenp '(1 3 4 5 6 7) :start 2 :end 3) nil)
      (is (find-if-not #'evenp '(1 3 4 5 6 7) :key #'1+) 4)

      (diag "Generic Sequences")
      (is (find-if-not #'evenp (list-wrap 1 3 4 5 6 7)) 1)
      (is (find-if-not #'evenp (list-wrap 1 3 4 5 6 7) :from-end t) 7)
      (is (find-if-not #'evenp (list-wrap 1 3 4 5 6 7) :start 3) 5)
      (is (find-if-not #'evenp (list-wrap 1 3 4 5 6 7) :start 2 :end 3) nil)
      (is (find-if-not #'evenp (list-wrap 1 3 4 5 6 7) :key #'1+) 4)))

  (subtest "Test POSITION Functions"
    (subtest "Test POSITION"
      (diag "CL Sequences")
      (is (position "a" '("b" "a" "c" "d" "a")) 1)
      (is (position "a" '("b" "a" "c" "d" "a") :from-end t) 4)
      (is (position "a" '("b" "a" "c" "d" "a") :start 2) 4)
      (is (position "a" '("b" "a" "c" "d" "a") :start 2 :end 3) nil)
      (is (position 2 '(1 3 5 6) :key #'1+) 0)

      (diag "Generic Sequences")
      (is (position "a" (list-wrap "b" "a" "c" "d" "a")) 1)
      (is (position "a" (list-wrap "b" "a" "c" "d" "a") :from-end t) 4)
      (is (position "a" (list-wrap "b" "a" "c" "d" "a") :start 2) 4)
      (is (position "a" (list-wrap "b" "a" "c" "d" "a") :start 2 :end 3) nil)
      (is (position 2 (list-wrap 1 3 5 6) :key #'1+) 0))

    (subtest "Test POSITION-IF"
      (diag "CL Sequences")
      (is (position-if #'evenp '(1 3 4 5 6 7)) 2)
      (is (position-if #'evenp '(1 3 4 5 6 7) :from-end t) 4)
      (is (position-if #'evenp '(1 3 4 5 6 7) :start 3) 4)
      (is (position-if #'evenp '(1 3 4 5 6 7) :start 3 :end 4) nil)
      (is (position-if #'evenp '(1 3 4 5 6 7) :key #'1+) 0)

      (diag "Generic Sequences")
      (is (position-if #'evenp (list-wrap 1 3 4 5 6 7)) 2)
      (is (position-if #'evenp (list-wrap 1 3 4 5 6 7) :from-end t) 4)
      (is (position-if #'evenp (list-wrap 1 3 4 5 6 7) :start 3) 4)
      (is (position-if #'evenp (list-wrap 1 3 4 5 6 7) :start 3 :end 4) nil)
      (is (position-if #'evenp (list-wrap 1 3 4 5 6 7) :key #'1+) 0))

    (subtest "Test FIND-IF-NOT"
      (diag "CL Sequences")
      (is (position-if-not #'evenp '(1 3 4 5 6 7)) 0)
      (is (position-if-not #'evenp '(1 3 4 5 6 7) :from-end t) 5)
      (is (position-if-not #'evenp '(1 3 4 5 6 7) :start 3) 3)
      (is (position-if-not #'evenp '(1 3 4 5 6 7) :start 2 :end 3) nil)
      (is (position-if-not #'evenp '(1 3 4 5 6 7) :key #'1+) 2)

      (diag "Generic Sequences")
      (is (position-if-not #'evenp (list-wrap 1 3 4 5 6 7)) 0)
      (is (position-if-not #'evenp (list-wrap 1 3 4 5 6 7) :from-end t) 5)
      (is (position-if-not #'evenp (list-wrap 1 3 4 5 6 7) :start 3) 3)
      (is (position-if-not #'evenp (list-wrap 1 3 4 5 6 7) :start 2 :end 3) nil)
      (is (position-if-not #'evenp (list-wrap 1 3 4 5 6 7) :key #'1+) 2))))

(finalize)
