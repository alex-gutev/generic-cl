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
      (is (position-if-not #'evenp (list-wrap 1 3 4 5 6 7) :key #'1+) 2)))

  (subtest "Test MISMATCH"
    (diag "CL Sequences")
    (is (mismatch '("alex" "bob" "john" "jack") '("alex" "bob" "john" "jack")) nil)
    (is (mismatch '("alex" "bob" "john" "jack") '("alex" "Bob" "john" "Jack")) 1)
    (is (mismatch '("alex" "bob" "john" "jack") '("alex" "bob")) 2)
    (is (mismatch '("alex" "bob") '("alex" "bob" "john" "jack")) 2)
    (is (mismatch '("alex" "bob" "john" "jack") '("alex" "Bob" "john" "Jack") :key #'string-upcase)
	nil)
    (is (mismatch '("alex" "bob" "john" "jack") '("alex" "Bob" "john" "Jack") :from-end t) 4)
    (is (mismatch '("alex" "bob" "john" "jack") '("bob" "john" "jack") :start1 1) nil)
    (is (mismatch '("alex" "bob" "john" "jack") '("Alex" "Pete" "bob" "john" "jack") :start1 1 :start2 2) nil)
    (is (mismatch '("alex" "bob" "john" "jack") '("Alex" "Pete" "bob" "John" "jack") :start1 1 :start2 2) 2)
    (is (mismatch '("alex" "bob" "john" "jack") '("Alex" "alex" "bob" "john" "Pete" "jack") :start1 1 :end1 3 :start2 2 :end2 4) nil)
    (is (mismatch '("alex" "john" "jack" "bob") '("alex" "bob" "john" "jack" "pete") :from-end t :start1 1 :end1 3 :start2 2 :end2 4) nil)
    (is (mismatch '("alex" "john" "jack" "bob") '("alex" "bob" "john" "jack" "pete") :from-end t :end1 3 :end2 4) 1)

    (diag "Generic Sequences")
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("alex" "bob" "john" "jack")) nil)
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("alex" "Bob" "john" "Jack")) 1)
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("alex" "bob")) 2)
    (is (mismatch (list-wrap "alex" "bob") '("alex" "bob" "john" "jack")) 2)
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("alex" "Bob" "john" "Jack") :key #'string-upcase)
	nil)
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("alex" "Bob" "john" "Jack") :from-end t) 4)
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("bob" "john" "jack") :start1 1) nil)
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("Alex" "Pete" "bob" "john" "jack") :start1 1 :start2 2) nil)
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("Alex" "Pete" "bob" "John" "jack") :start1 1 :start2 2) 2)
    (is (mismatch (list-wrap "alex" "bob" "john" "jack") '("Alex" "alex" "bob" "john" "Pete" "jack") :start1 1 :end1 3 :start2 2 :end2 4) nil)
    (is (mismatch (list-wrap "alex" "john" "jack" "bob") '("alex" "bob" "john" "jack" "pete") :from-end t :start1 1 :end1 3 :start2 2 :end2 4) nil)
    (is (mismatch (list-wrap "alex" "john" "jack" "bob") '("alex" "bob" "john" "jack" "pete") :from-end t :end1 3 :end2 4) 1))

  (subtest "Test Reversing"
    (subtest "Test REVERSE"
      (diag "CL Sequences")

      (let ((list '(1 2 3 4)))
	(is (reverse list) '(4 3 2 1))
	(is list '(1 2 3 4) "Not Modified"))

      (is (reverse '(1)) '(1))
      (is (reverse nil) nil)

      (is (reverse #(a b c d)) #(d c b a) :test #'equalp)

      (diag "Generic Sequences")

      (let ((seq (list-wrap 1 2 3 4)))
	(is (reverse seq) (list-wrap 4 3 2 1) :test #'equalp)
	(is seq (list-wrap 1 2 3 4) "Not Modified" :test #'equalp))

      (is (reverse (list-wrap 1)) (list-wrap 1) :test #'equalp)
      (is (reverse (list-wrap)) (list-wrap) :test #'equalp))

    (subtest "Test NREVERSE"
      (diag "CL Sequences")

      (is (nreverse '(1 2 3 4)) '(4 3 2 1))
      (is (nreverse '(1)) '(1))
      (is (nreverse nil) nil)

      (is (nreverse #(a b c d)) #(d c b a) :test #'equalp)

      (diag "Generic Sequences")

      (is (nreverse (list-wrap 1 2 3 4)) (list-wrap 4 3 2 1) :test #'equalp)
      (is (nreverse (list-wrap 1)) (list-wrap 1) :test #'equalp)
      (is (nreverse (list-wrap)) (list-wrap) :test #'equalp)))

  (subtest "Test Merging"
    (subtest "Test MERGE"
      (diag "CL Sequences")

      (let ((seq1 '(1 2 3 4))
	    (seq2 '(5 6 7 8)))
	(is (merge seq1 seq2 #'lessp) '(1 2 3 4 5 6 7 8))
	(is seq1 '(1 2 3 4) "Sequence1 Not Modified")
	(is seq2 '(5 6 7 8) "Sequence2 Not Modified"))

      (let ((seq1 '(1 3 5 9))
	    (seq2 '(2 4 6 7 8)))
	(is (merge seq1 seq2 #'lessp) '(1 2 3 4 5 6 7 8 9))
	(is seq1 '(1 3 5 9) "Sequence1 Not Modified")
	(is seq2 '(2 4 6 7 8) "Sequence2 Not Modified"))

      (is (merge '((a 1) (b 2) (c 5) (d 8)) '((e 3) (f 4) (g 6) (h 7)) #'lessp :key #'cadr)
	  '((a 1) (b 2) (e 3) (f 4) (c 5) (g 6) (h 7) (d 8)))

      ;; Test Stability
      (is (merge '((b 1) (d 1) (a 99)) '((e 1) (h 1) (f 32) (c 74)) #'lessp :key #'cadr)
	  '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99)))

      (diag "Generic Sequences")

      (let ((seq1 (list-wrap 1 2 3 4))
	    (seq2 (list-wrap 5 6 7 8)))
	(is (merge seq1 seq2 #'lessp) (list-wrap 1 2 3 4 5 6 7 8) :test #'equalp)
	(is seq1 (list-wrap 1 2 3 4) "Sequence1 Not Modified" :test #'equalp)
	(is seq2 (list-wrap 5 6 7 8) "Sequence2 Not Modified" :test #'equalp))

      (let ((seq1 (list-wrap 1 3 5 9))
	    (seq2 (list-wrap 2 4 6 7 8)))
	(is (merge seq1 seq2 #'lessp) (list-wrap 1 2 3 4 5 6 7 8 9) :test #'equalp)
	(is seq1 (list-wrap 1 3 5 9) "Sequence1 Not Modified" :test #'equalp)
	(is seq2 (list-wrap 2 4 6 7 8) "Sequence2 Not Modified" :test #'equalp))

      (is (merge (list-wrap '(a 1) '(b 2) '(c 5) '(d 8)) (list-wrap '(e 3) '(f 4) '(g 6) '(h 7)) #'lessp :key #'cadr)
	  (list-wrap '(a 1) '(b 2) '(e 3) '(f 4) '(c 5) '(g 6) '(h 7) '(d 8))
	  :test #'equalp)

      ;; Test Stability
      (is (merge (list-wrap '(b 1) '(d 1) '(a 99)) (list-wrap '(e 1) '(h 1) '(f 32) '(c 74)) #'lessp :key #'cadr)
	  (list-wrap '(b 1) '(d 1) '(e 1) '(h 1) '(f 32) '(c 74) '(a 99))
	  :test #'equalp))

    (subtest "Test NMERGE"
      (diag "CL Sequences")

      (is (nmerge (list 1 2 3 4) (list 5 6 7 8) #'lessp) '(1 2 3 4 5 6 7 8))
      (is (nmerge (list 1 3 5 9) (list 2 4 6 7 8) #'lessp) '(1 2 3 4 5 6 7 8 9))
      (is (nmerge (list '(a 1) '(b 2) '(c 5) '(d 8)) (list '(e 3) '(f 4) '(g 6) '(h 7)) #'lessp :key #'cadr)
	  '((a 1) (b 2) (e 3) (f 4) (c 5) (g 6) (h 7) (d 8)))

      ;; Test Stability
      (is (nmerge (list '(b 1) '(d 1) '(a 99)) (list '(e 1) '(h 1) '(f 32) '(c 74)) #'lessp :key #'cadr)
	  '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99)))

      (diag "Generic Sequences")

      (is (nmerge (list-wrap 1 2 3 4) (list-wrap 5 6 7 8) #'lessp) (list-wrap 1 2 3 4 5 6 7 8) :test #'equalp)
      (is (nmerge (list-wrap 1 3 5 9) (list-wrap 2 4 6 7 8) #'lessp) (list-wrap 1 2 3 4 5 6 7 8 9) :test #'equalp)
      (is (nmerge (list-wrap '(a 1) '(b 2) '(c 5) '(d 8)) (list-wrap '(e 3) '(f 4) '(g 6) '(h 7)) #'lessp :key #'cadr)
	  (list-wrap '(a 1) '(b 2) '(e 3) '(f 4) '(c 5) '(g 6) '(h 7) '(d 8))
	  :test #'equalp)

      ;; Test Stability
      (is (nmerge (list-wrap '(b 1) '(d 1) '(a 99)) (list-wrap '(e 1) '(h 1) '(f 32) '(c 74)) #'lessp :key #'cadr)
	  (list-wrap '(b 1) '(d 1) '(e 1) '(h 1) '(f 32) '(c 74) '(a 99))
	  :test #'equalp)))

  (subtest "Test Sorting"
    (subtest "Test SORT Functions"
      (subtest "Test SORT"
	(diag "CL Sequences")

	(let ((seq '("aac" "zzz" "aaa" "aab" "bac" "baa")))
	  (is (sort seq) '("aaa" "aab" "aac" "baa" "bac" "zzz") :test #'equalp)
	  (is seq '("aac" "zzz" "aaa" "aab" "bac" "baa") :test #'equalp "Not Modified"))

	(is (sort '(99 3 74 56 1 32 49) :test #'greaterp) '(99 74 56 49 32 3 1))
	(is (sort #(99 3 74 56 1 32 49) :test #'greaterp) #(99 74 56 49 32 3 1) :test #'equalp)

	(is (sort '((a 99) (b 3) (c 74) (d 56) (e 1) (f 32) (h 49)) :key #'cadr)
	    '((e 1) (b 3) (f 32) (h 49) (d 56) (c 74) (a 99)))

	(diag "Generic Sequences")

	(let ((seq (list-wrap "aac" "zzz" "aaa" "aab" "bac" "baa")))
	  (is (sort seq) (list-wrap "aaa" "aab" "aac" "baa" "bac" "zzz") :test #'equalp)
	  (is seq (list-wrap "aac" "zzz" "aaa" "aab" "bac" "baa") :test #'equalp "Not Modified"))

	(is (sort (list-wrap 99 3 74 56 1 32 49) :test #'greaterp)
	    (list-wrap 99 74 56 49 32 3 1)
	    :test #'equalp)

	(is (sort (list-wrap '(a 99) '(b 3) '(c 74) '(d 56) '(e 1) '(f 32) '(h 49)) :key #'cadr)
	    (list-wrap '(e 1) '(b 3) '(f 32) '(h 49) '(d 56) '(c 74) '(a 99))
	    :test #'equalp))

      (subtest "Test NSORT"
	(diag "CL Sequences")

	(is (nsort (list "aac" "zzz" "aaa" "aab" "bac" "baa"))
	    '("aaa" "aab" "aac" "baa" "bac" "zzz")
	    :test #'equalp)

	(is (nsort (list 99 3 74 56 1 32 49) :test #'greaterp) '(99 74 56 49 32 3 1))
	(is (nsort (vector 99 3 74 56 1 32 49) :test #'greaterp) #(99 74 56 49 32 3 1) :test #'equalp)

	(is (nsort (list '(a 99) '(b 3) '(c 74) '(d 56) '(e 1) '(f 32) '(h 49)) :key #'cadr)
	    '((e 1) (b 3) (f 32) (h 49) (d 56) (c 74) (a 99)))

	(diag "Generic Sequences")

	(is (nsort (list-wrap "aac" "zzz" "aaa" "aab" "bac" "baa"))
	    (list-wrap "aaa" "aab" "aac" "baa" "bac" "zzz")
	    :test #'equalp)

	(is (nsort (list-wrap 99 3 74 56 1 32 49) :test #'greaterp)
	    (list-wrap 99 74 56 49 32 3 1)
	    :test #'equalp)

	(is (nsort (list-wrap '(a 99) '(b 3) '(c 74) '(d 56) '(e 1) '(f 32) '(h 49)) :key #'cadr)
	    (list-wrap '(e 1) '(b 3) '(f 32) '(h 49) '(d 56) '(c 74) '(a 99))
	    :test #'equalp)))

    (subtest "Test STABLE-SORT Functions"
      (subtest "Test STABLE-SORT"
	(diag "CL Sequences")

	(let ((seq '("aac" "zzz" "aaa" "aab" "bac" "baa")))
	  (is (stable-sort seq) '("aaa" "aab" "aac" "baa" "bac" "zzz") :test #'equalp)
	  (is seq '("aac" "zzz" "aaa" "aab" "bac" "baa") :test #'equalp "Not Modified"))

	(is (stable-sort '(99 3 74 56 1 32 49) :test #'greaterp) '(99 74 56 49 32 3 1))
	(is (stable-sort #(99 3 74 56 1 32 49) :test #'greaterp) #(99 74 56 49 32 3 1) :test #'equalp)

	;; Test Stability
	(is (stable-sort '((a 99) (b 1) (c 74) (d 1) (e 1) (f 32) (h 1)) :key #'cadr)
	    '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99)))

	(diag "Generic Sequences")

	(let ((seq (list-wrap "aac" "zzz" "aaa" "aab" "bac" "baa")))
	  (is (stable-sort seq) (list-wrap "aaa" "aab" "aac" "baa" "bac" "zzz") :test #'equalp)
	  (is seq (list-wrap "aac" "zzz" "aaa" "aab" "bac" "baa") :test #'equalp "Not Modified"))

	(is (stable-sort (list-wrap 99 3 74 56 1 32 49) :test #'greaterp)
	    (list-wrap 99 74 56 49 32 3 1)
	    :test #'equalp)

	;; Test Stability
	(is (stable-sort (list-wrap '(a 99) '(b 1) '(c 74) '(d 1) '(e 1) '(f 32) '(h 1)) :key #'cadr)
	    (list-wrap '(b 1) '(d 1) '(e 1) '(h 1) '(f 32) '(c 74) '(a 99))
	    :test #'equalp))

      (subtest "Test STABLE-NSORT"
	(diag "CL Sequences")

	(is (stable-nsort (list "aac" "zzz" "aaa" "aab" "bac" "baa"))
	    (list "aaa" "aab" "aac" "baa" "bac" "zzz")
	    :test #'equalp)

	(is (stable-nsort (list 99 3 74 56 1 32 49) :test #'greaterp) '(99 74 56 49 32 3 1))
	(is (stable-nsort (vector 99 3 74 56 1 32 49) :test #'greaterp) #(99 74 56 49 32 3 1) :test #'equalp)

	;; Test Stability
	(is (stable-nsort (list '(a 99) '(b 1) '(c 74) '(d 1) '(e 1) '(f 32) '(h 1)) :key #'cadr)
	    '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99)))

	(diag "Generic Sequences")

	(is (stable-nsort (list-wrap "aac" "zzz" "aaa" "aab" "bac" "baa"))
	    (list-wrap "aaa" "aab" "aac" "baa" "bac" "zzz")
	    :test #'equalp)

	(is (stable-nsort (list-wrap 99 3 74 56 1 32 49) :test #'greaterp)
	    (list-wrap 99 74 56 49 32 3 1)
	    :test #'equalp)

	;; Test Stability
	(is (stable-nsort (list-wrap '(a 99) '(b 1) '(c 74) '(d 1) '(e 1) '(f 32) '(h 1)) :key #'cadr)
	    (list-wrap '(b 1) '(d 1) '(e 1) '(h 1) '(f 32) '(c 74) '(a 99))
	    :test #'equalp))))

  (macrolet ((test-seq-fn ((&rest lists) &body tests)
	       `(progn
		  (symbol-macrolet ,lists
		    (diag "CL Sequence")
		    ,@tests)
		  (symbol-macrolet
		      ,(loop for (var list) in lists
			  collect `(,var (make-list-wrapper :list ,list)))
		    (diag "Generic Sequence")
		    ,@tests)))

	     (test-not-modified ((var seq) &body tests)
	       `(progn
		  (let ((,var ,seq))
		    ,@tests
		    (is ,var ,seq :test #'equalp "Not Modified")))))

    (subtest "Test SUBSTITUTE Functions"
      (subtest "Test SUBSTITUTE"
	(test-seq-fn
	 ((list '("a" "b" "old" "c" "old" "d"))
	  (res '("a" "b" "new" "c" "new" "d")))

	 (test-not-modified
	  (seq list)
	  (is (substitute "new" "old" seq) res :test #'equalp)))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(x 2 3 x 4 5 x)))
	 (is (substitute 'x 1 seq) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(x 2 3 x 4 5 1)))
	 (is (substitute 'x 1 seq :count 2) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(1 2 3 x 4 5 x)))
	 (is (substitute 'x 1 seq :count 2 :from-end t) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(1 2 3 x 4 5 x)))
	 (is (substitute 'x 1 seq :start 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(1 2 3 x 4 5 1)))
	 (is (substitute 'x 1 seq :start 1 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(1 2 3 x 4 5 1)))
	 (is (substitute 'x 1 seq :start 1 :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(x 2 3 x 4 5 1)))
	 (is (substitute 'x 1 seq :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(x 2 3 1 4 5 1)))
	 (is (substitute 'x 1 seq :end 5 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '((a 1) (b 2) (c 3) (d 1) (e 4) (f 5) (g 1)))
	  (res '(x (b 2) (c 3) x (e 4) (f 5) x)))
	 (is (substitute 'x 1 seq :key #'cadr) res :test #'equalp)))

      (subtest "Test NSUBSTITUTE"
	(test-seq-fn
	 ((seq (list "a" "b" "old" "c" "old" "d"))
	  (res (list "a" "b" "new" "c" "new" "d")))

	 (is (nsubstitute "new" "old" seq) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 1 4 5 1))
	  (res (list 'x 2 3 'x 4 5 'x)))
	 (is (nsubstitute 'x 1 seq) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 1 4 5 1))
	  (res (list 'x 2 3 'x 4 5 1)))
	 (is (nsubstitute 'x 1 seq :count 2) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 1 4 5 1))
	  (res (list 1 2 3 'x 4 5 'x)))
	 (is (nsubstitute 'x 1 seq :count 2 :from-end t) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 1 4 5 1))
	  (res (list 1 2 3 'x 4 5 'x)))
	 (is (nsubstitute 'x 1 seq :start 1) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 1 4 5 1))
	  (res (list 1 2 3 'x 4 5 1)))
	 (is (nsubstitute 'x 1 seq :start 1 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 1 4 5 1))
	  (res (list 1 2 3 'x 4 5 1)))
	 (is (nsubstitute 'x 1 seq :start 1 :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 1 4 5 1))
	  (res (list 'x 2 3 'x 4 5 1)))
	 (is (nsubstitute 'x 1 seq :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 1 4 5 1))
	  (res (list 'x 2 3 1 4 5 1)))
	 (is (nsubstitute 'x 1 seq :end 5 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq (list '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1)))
	  (res (list 'x '(b 2) '(c 3) 'x '(e 4) '(f 5) 'x)))
	 (is (nsubstitute 'x 1 seq :key #'cadr) res :test #'equalp)))

      (subtest "Test SUBSTITUTE-IF"
	(test-seq-fn
	 ((list '(1 2 3 4 5 6 7 8))
	  (res '(1 x 3 x 5 x 7 x)))

	 (test-not-modified
	  (seq list)
	  (is (substitute-if 'x #'evenp seq) res :test #'equalp)))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 x 3 x 5 6 7 8)))
	 (is (substitute-if 'x #'evenp seq :count 2) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 2 3 4 5 x 7 x)))
	 (is (substitute-if 'x #'evenp seq :count 2 :from-end t) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 2 3 x 5 x 7 x)))
	 (is (substitute-if 'x #'evenp seq :start 2) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 2 3 x 5 6 7 8)))
	 (is (substitute-if 'x #'evenp seq :start 2 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 2 3 x 5 x 7 8)))
	 (is (substitute-if 'x #'evenp seq :start 2 :end 6) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 x 3 x 5 6 7 8)))
	 (is (substitute-if 'x #'evenp seq :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 x 3 4 5 6 7 8)))
	 (is (substitute-if 'x #'evenp seq :end 5 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
	  (res '((a 1) x (c 3) x (e 5) x (g 7))))
	 (is (substitute-if 'x #'evenp seq :key #'cadr) res :test #'equalp)))

      (subtest "Test NSUBSTITUTE-IF"
	(test-seq-fn
	 ((list (list 1 2 3 4 5 6 7 8))
	  (res (list 1 'x 3 'x 5 'x 7 'x)))

	 (test-not-modified
	  (seq list)
	  (is (substitute-if 'x #'evenp seq) res :test #'equalp)))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 'x 3 'x 5 6 7 8)))
	 (is (substitute-if 'x #'evenp seq :count 2) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 2 3 4 5 'x 7 'x)))
	 (is (substitute-if 'x #'evenp seq :count 2 :from-end t) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 2 3 'x 5 'x 7 'x)))
	 (is (substitute-if 'x #'evenp seq :start 2) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 2 3 'x 5 6 7 8)))
	 (is (substitute-if 'x #'evenp seq :start 2 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 2 3 'x 5 'x 7 8)))
	 (is (substitute-if 'x #'evenp seq :start 2 :end 6) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 'x 3 'x 5 6 7 8)))
	 (is (substitute-if 'x #'evenp seq :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 'x 3 4 5 6 7 8)))
	 (is (substitute-if 'x #'evenp seq :end 5 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq (list '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7)))
	  (res (list '(a 1) 'x '(c 3) 'x '(e 5) 'x '(g 7))))
	 (is (substitute-if 'x #'evenp seq :key #'cadr) res :test #'equalp)))

      (subtest "Test SUBSTITUTE-IF-NOT"
	(diag "CL Sequences")

	(test-seq-fn
	 ((list '(1 2 3 4 5 6 7 8))
	  (res '(x 2 x 4 x 6 x 8)))

	 (test-not-modified
	  (seq list)
	  (is (substitute-if-not 'x #'evenp seq) res :test #'equalp)))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(x 2 x 4 5 6 7 8)))

	 (is (substitute-if-not 'x #'evenp seq :count 2) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 2 3 4 x 6 x 8)))
	 (is (substitute-if-not 'x #'evenp seq :count 2 :from-end t) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 2 x 4 x 6 x 8)))
	 (is (substitute-if-not 'x #'evenp seq :start 2) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 2 x 4 5 6 7 8)))
	 (is (substitute-if-not 'x #'evenp seq :start 2 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(1 2 x 4 x 6 7 8)))
	 (is (substitute-if-not 'x #'evenp seq :start 2 :end 6) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(x 2 x 4 x 6 7 8)))
	 (is (substitute-if-not 'x #'evenp seq :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 4 5 6 7 8))
	  (res '(x 2 3 4 5 6 7 8)))
	 (is (substitute-if-not 'x #'evenp seq :end 5 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
	  (res '(x (b 2) x (d 4) x (f 6) x)))
	 (is (substitute-if-not 'x #'evenp seq :key #'cadr) res :test #'equalp)))

      (subtest "Test NSUBSTITUTE-IF-NOT"
	(diag "CL Sequences")

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 'x 2 'x 4 'x 6 'x 8)))

	 (is (nsubstitute-if-not 'x #'evenp seq) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 'x 2 'x 4 5 6 7 8)))

	 (is (nsubstitute-if-not 'x #'evenp seq :count 2) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 2 3 4 'x 6 'x 8)))
	 (is (substitute-if-not 'x #'evenp seq :count 2 :from-end t) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 2 'x 4 'x 6 'x 8)))
	 (is (nsubstitute-if-not 'x #'evenp seq :start 2) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 2 'x 4 5 6 7 8)))
	 (is (nsubstitute-if-not 'x #'evenp seq :start 2 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 2 'x 4 'x 6 7 8)))
	 (is (nsubstitute-if-not 'x #'evenp seq :start 2 :end 6) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 'x 2 'x 4 'x 6 7 8)))
	 (is (nsubstitute-if-not 'x #'evenp seq :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 'x 2 3 4 5 6 7 8)))
	 (is (nsubstitute-if-not 'x #'evenp seq :end 5 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq (list '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7)))
	  (res (list 'x '(b 2) 'x '(d 4) 'x '(f 6) 'x)))
	 (is (nsubstitute-if-not 'x #'evenp seq :key #'cadr) res :test #'equalp))))))

(finalize)
