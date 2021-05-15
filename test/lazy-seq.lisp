;;;; lazy-seq.lisp
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

;;;; Lazy Sequence Tests

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite lazy-seq
    :description "Test lazy sequences"
    :in generic-cl)

(in-suite lazy-seq)


;;; Test Utilities

(defmacro lseq (&rest items)
  (cl:reduce
   (lambda (item seq)
     `(lazy-seq ,item ,seq))
   items
   :from-end t
   :initial-value nil))

(defmacro test-lazy-seq-fn ((type expected) form)
  (with-gensyms (result)
    `(let ((,result ,form))
       (is (= ,(case type
		 (lazy-seq
		  `(lseq ,@(map (curry #'list 'quote) expected)))
		 (otherwise
		  `',expected))
	      ,result))

       (is (typep ,result ',type)))))


;;; Test Lazy Sequence Creation

(test lazy-seq-creation
  "Test lazy sequence creation"

  (let ((seq (lazy-seq 1 (lazy-seq 2 (lazy-seq 3 nil)))))
    (is (lazy-seq-head seq) 1)

    (is (->> seq
	     lazy-seq-tail
	     funcall
	     lazy-seq-head
	     (= 2)))

    (is (->> seq
	     lazy-seq-tail
	     funcall
	     lazy-seq-tail
	     funcall
	     lazy-seq-head
	     (= 3)))

    (is (->> seq
	     lazy-seq-tail
	     funcall
	     lazy-seq-tail
	     funcall
	     lazy-seq-tail
	     funcall
	     (= nil)))))

(test lazy-seq-evaluation
  "Test that lazy sequence elements evaluated only when first accessed"

  (let ((times 0))
    (flet ((get-elem (n)
	     (incf times)
	     n))
      (let ((seq (lazy-seq 1 (lazy-seq (get-elem 2) (lazy-seq (get-elem 3) nil)))))
	(is (= 0 times))

	(funcall (lazy-seq-tail seq))
	(is (= 1 times))

	(funcall (lazy-seq-tail (funcall (lazy-seq-tail seq))))
	(is (= 2 times))))))


;;; Lazy Sequence Iterators

(test lazy-seq-iterator-unbounded
  "Test unbounded lazy sequence iterator"

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c)))
    1 2 3 a b c)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :from-end t))
    c b a 3 2 1)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c)) :step 2 :length 6)
    1 3 b)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :from-end t) :step 2 :length 6)
    c a 2))

(test lazy-seq-iterator-bounded
  "Test bounded lazy sequence iterator"

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :start 2))
    3 a b c)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :start 2 :end 4))
    3 a)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :start 2 :from-end t))
    c b a 3)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :start 2 :end 4 :from-end t))
    a 3)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :start 2) :step 3 :length 4)
    3 c)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :start 2 :end 4) :step 2 :length 2)
    3)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :start 2 :from-end t) :step 2 :length 4)
    c a)

  (is-iter-elements ((iterator (lseq 1 2 3 'a 'b 'c) :start 2 :end 4 :from-end t) :step 3 :length 2)
    a))

(test lazy-seq-subseq-iterator
  "Test SUBSEQ on lazy sequence iterator"

  (is-iter-elements ((subseq (iterator (lseq 1 2 3 'a 'b 'c)) 2))
    3 a b c)

  (is-iter-elements ((subseq (iterator (lseq 1 2 3 'a 'b 'c)) 2 nil))
    3 a b c)

  (is-iter-elements ((subseq (iterator (lseq 1 2 3 'a 'b 'c)) 4))
    b c)

  (is-iter-elements ((subseq (iterator (lseq 1 2 3 'a 'b 'c) :from-end t) 2))
    a 3 2 1)

  (is-iter-elements ((subseq (iterator (lseq 1 2 3 'a 'b 'c) :from-end t) 2 nil))
    a 3 2 1)

  (is-iter-elements ((subseq (iterator (lseq 1 2 3 'a 'b 'c) :from-end t) 2 4))
    a 3))

(test lazy-seq-iterator-single-element
  "Test lazy sequence iterator on single element"

  (is-iter-elements ((iterator (lseq 'a)))
    a)

  (is-iter-elements ((iterator (lseq 'a) :from-end t))
    a)

  (is-iter-elements ((iterator (lseq 'a) :start 1)))

  (is-iter-elements ((iterator (lseq 'a) :start 1 :from-end t)))

  (is-iter-elements ((iterator (lseq 'a) :start 1 :end 1)))

  (is-iter-elements ((iterator (lseq 'a) :start 1 :end 1 :from-end t)))

  (is-iter-elements ((iterator (lseq 'a) :from-end t) :step 2)
    a)

  (is-iter-elements ((iterator (lseq 'a) :start 1) :step 3))

  (is-iter-elements ((iterator (lseq 'a) :start 1 :from-end t) :step 4))

  (is-iter-elements ((iterator (lseq 'a) :start 1 :end 1) :step 2))

  (is-iter-elements ((iterator (lseq 'a) :start 1 :end 1 :from-end t) :step 2)))


;;; Test Lazy Sequence Equality

(test lazy-seq-equality
  "Test equality interface on lazy sequence"

  (is-true (equalp (lseq 1 2 3) (lseq 1 2 3)))
  (is-true (equalp (lseq 1 2 3)
		   (lseq 1 (cl:1+ 1) (cl:+ 2 1))))

  (is-true (= (lseq (lseq 1 2 3) (lseq 'x) (lseq "Alex" "Bob"))
	      (lseq (lseq 1 (cl:1+ 1) (cl:+ 2 1)) (lseq 'x) (lseq "Alex" "Bob"))))

  (is-true (/= (lseq 1 2 3) (lseq 4 5 6)))
  (is-true (/= (lseq 1 2 3) (lseq 1 2 4)))
  (is-true (/= (lseq 1 2 3) (lseq 1 2 3 4)))
  (is-true (/= (lseq 1 2 3) (lseq 1 2))))


;;; Test CLEARED, FIRST and EMPTY on lazy sequences

(test lazy-seq-cleared
  "Test CLEARED on lazy sequences"

  (is (= nil (cleared (lazy-seq 1 (lazy-seq 2 (lazy-seq 3 nil)))))))

(test lazy-seq-first
  "Test FIRST on lazy sequences"

  (is (= 1 (first (lazy-seq 1 (lazy-seq 2 (lazy-seq 3)))))))

(test lazy-seq-emptyp
  "Test EMPTYP on lazy sequences"

  (is-false (emptyp (lazy-seq 1 nil)))
  (is-false (emptyp (lazy-seq 1 (lazy-seq 2 nil)))))


;;; Test SUBSEQ

(test lazy-seq-subseq
  "Test SUBSEQ on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (2 3))
      (subseq (lseq 1 2 3 4 5) 1 3))

  (test-lazy-seq-fn
      (lazy-seq (3 4 5))
      (subseq (lseq 1 2 3 4 5) 2)))


;;; Test SUBSTITUTE Functions

(test lazy-seq-substitute
  "Test SUBSTITUTE on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq ("a" "b" "new" "c" "new" "d"))
      (substitute "new" "old" (lseq "a" "b" "old" "c" "old" "d")))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 x 4 5 x))
      (substitute 'x 1 (lseq 1 2 3 1 4 5 1)))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 x 4 5 1))
      (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 x 4 5 x))
      (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 x 4 5 1))
      (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :start 1 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 x 4 5 1))
      (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :end 5))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 1 4 5 1))
      (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (x (b 2) (c 3) x (e 4) (f 5) x))
      (substitute 'x 1 (lseq '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1))
		  :key #'cadr)))

(test lazy-seq-nsubstitute
  "Test NSUBSTITUTE on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq ("a" "b" "new" "c" "new" "d"))
      (nsubstitute "new" "old" (lseq "a" "b" "old" "c" "old" "d")))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 x 4 5 x))
      (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1)))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 x 4 5 1))
      (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 x 4 5 x))
      (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 x 4 5 1))
      (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :start 1 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 x 4 5 1))
      (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :end 5))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 1 4 5 1))
      (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (x (b 2) (c 3) x (e 4) (f 5) x))
      (nsubstitute 'x 1 (lseq '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1))
		   :key #'cadr)))

(test lazy-seq-substitute-if
  "Test SUBSTITUTE-IF on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (1 x 3 x 5 x 7 x))
      (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)))

  (test-lazy-seq-fn
      (lazy-seq (1 x 3 x 5 6 7 8))
      (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8) :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 5 x 7 x))
      (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 x 5 x 7 x))
      (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8) :start 2))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 x 5 6 7 8))
      (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :start 2 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 x 5 x 7 8))
      (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :start 2 :end 6))

  (test-lazy-seq-fn
      (lazy-seq (1 x 3 x 5 6 7 8))
      (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :end 5))

  (test-lazy-seq-fn
      (lazy-seq (1 x 3 4 5 6 7 8))
      (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq ((a 1) x (c 3) x (e 5) x (g 7)))
      (substitute-if 'x #'evenp (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
		     :key #'cadr)))

(test lazy-seq-nsubstitute-if
  "Test NSUBSTITUTE-IF on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (1 x 3 x 5 x 7 x))
      (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)))

  (test-lazy-seq-fn
      (lazy-seq (1 x 3 x 5 6 7 8))
      (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8) :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 5 x 7 x))
      (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		      :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 x 5 x 7 x))
      (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8) :start 2))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 x 5 6 7 8))
      (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		      :start 2 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 x 5 x 7 8))
      (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		      :start 2 :end 6))

  (test-lazy-seq-fn
      (lazy-seq (1 x 3 x 5 6 7 8))
      (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		      :end 5))

  (test-lazy-seq-fn
      (lazy-seq (1 x 3 4 5 6 7 8))
      (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
		      :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq ((a 1) x (c 3) x (e 5) x (g 7)))
      (nsubstitute-if 'x #'evenp (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
		      :key #'cadr)))

(test lazy-seq-subtitute-if-not
  "Test SUBSTITUTE-IF-NOT on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (x 2 x 4 x 6 x 8))
      (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)))

  (test-lazy-seq-fn
      (lazy-seq (x 2 x 4 5 6 7 8))
      (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			 :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 x 6 x 8))
      (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			 :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 x 4 x 6 x 8))
      (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			 :start 2))

  (test-lazy-seq-fn
      (lazy-seq (1 2 x 4 5 6 7 8))
      (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			 :start 2 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 x 4 x 6 7 8))
      (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			 :start 2 :end 6))

  (test-lazy-seq-fn
      (lazy-seq (x 2 x 4 x 6 7 8))
      (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			 :end 5))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 4 5 6 7 8))
      (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			 :count 1 :end 5))

  (test-lazy-seq-fn
      (lazy-seq (x (b 2) x (d 4) x (f 6) x))
      (substitute-if-not 'x #'evenp
			 (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
			 :key #'cadr)))

(test lazy-seq-nsubtitute-if-not
  "Test NSUBSTITUTE-IF-NOT on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (x 2 x 4 x 6 x 8))
      (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)))

  (test-lazy-seq-fn
      (lazy-seq (x 2 x 4 5 6 7 8))
      (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 x 6 x 8))
      (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 x 4 x 6 x 8))
      (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2))

  (test-lazy-seq-fn
      (lazy-seq (1 2 x 4 5 6 7 8))
      (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 x 4 x 6 7 8))
      (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2 :end 6))

  (test-lazy-seq-fn
      (lazy-seq (x 2 x 4 x 6 7 8))
      (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :end 5))

  (test-lazy-seq-fn
      (lazy-seq (x 2 3 4 5 6 7 8))
      (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :count 1 :end 5))

  (test-lazy-seq-fn
      (lazy-seq (x (b 2) x (d 4) x (f 6) x))
      (nsubstitute-if-not 'x #'evenp
			  (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
			  :key #'cadr)))


;;; Test REMOVE Functions

(test lazy-seq-remove
  "Test REMOVE on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq ("a" "b" "c" "d"))
      (remove "old" (lseq "a" "b" "old" "c" "old" "d")))

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5))
      (remove 1 (lseq 1 2 3 1 4 5 1)))

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5 1))
      (remove 1 (lseq 1 2 3 1 4 5 1) :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 5))
      (remove 1 (lseq 1 2 3 1 4 5 1)
	      :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4 5))
      (remove 1 (lseq 1 2 3 1 4 5 1) :start 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4 5 1))
      (remove 1 (lseq 1 2 3 1 4 5 1)
	      :start 1 :end 5))

  (test-lazy-seq-fn
      (lazy-seq (2 3 1 4 5 1))
      (remove 1 (lseq 1 2 3 1 4 5 1)
	      :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq ((b 2) (c 3) (e 4) (f 5)))
      (remove 1 (lseq '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1))
	      :key #'cadr)))

(test lazy-seq-delete
  "Test DELETE on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq ("a" "b" "c" "d"))
      (delete "old" (lseq "a" "b" "old" "c" "old" "d")))

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5))
      (delete 1 (lseq 1 2 3 1 4 5 1)))

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5 1))
      (delete 1 (lseq 1 2 3 1 4 5 1) :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 5))
      (delete 1 (lseq 1 2 3 1 4 5 1)
	      :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4 5))
      (delete 1 (lseq 1 2 3 1 4 5 1) :start 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4 5 1))
      (delete 1 (lseq 1 2 3 1 4 5 1)
	      :start 1 :end 5))

  (test-lazy-seq-fn
      (lazy-seq (2 3 1 4 5 1))
      (delete 1 (lseq 1 2 3 1 4 5 1)
	      :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq ((b 2) (c 3) (e 4) (f 5)))
      (delete 1 (lseq '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1))
	      :key #'cadr)))

(test lazy-seq-remove-if
  "Test REMOVE-IF on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (1 3 5 7))
      (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)))

  (test-lazy-seq-fn
      (lazy-seq (1 3 5 6 7 8))
      (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 5 7))
      (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 5 7))
      (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :start 2))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 5 6 7 8))
      (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :start 2 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 5 7 8))
      (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :start 2 :end 6))

  (test-lazy-seq-fn
      (lazy-seq (1 3 5 6 7 8))
      (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :end 5))

  (test-lazy-seq-fn
      (lazy-seq (1 3 4 5 6 7 8))
      (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq ((a 1) (c 3) (e 5) (g 7)))
      (remove-if #'evenp (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
		 :key #'cadr)))

(test lazy-seq-delete-if
  "Test DELETE-IF on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (1 3 5 7))
      (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)))

  (test-lazy-seq-fn
      (lazy-seq (1 3 5 6 7 8))
      (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 5 7))
      (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 5 7))
      (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :start 2))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 5 6 7 8))
      (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :start 2 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 5 7 8))
      (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :start 2 :end 6))

  (test-lazy-seq-fn
      (lazy-seq (1 3 5 6 7 8))
      (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :end 5))

  (test-lazy-seq-fn
      (lazy-seq (1 3 4 5 6 7 8))
      (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
		 :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq ((a 1) (c 3) (e 5) (g 7)))
      (delete-if #'evenp (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
		 :key #'cadr)))

(test lazy-seq-remove-if-not
  "Test REMOVE-IF-NOT on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (2 4 6 8))
      (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)))

  (test-lazy-seq-fn
      (lazy-seq (2 4 5 6 7 8))
      (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 6 8))
      (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 4 6 8))
      (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :start 2))

  (test-lazy-seq-fn
      (lazy-seq (1 2 4 5 6 7 8))
      (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :start 2 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 4 6 7 8))
      (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :start 2 :end 6))

  (test-lazy-seq-fn
      (lazy-seq (2 4 6 7 8))
      (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :end 5))

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5 6 7 8))
      (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq ((b 2) (d 4) (f 6)))
      (remove-if-not #'evenp
		     (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
		     :key #'cadr)))

(test lazy-seq-delete-if-not
  "Test DELETE-IF-NOT on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (2 4 6 8))
      (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)))

  (test-lazy-seq-fn
      (lazy-seq (2 4 5 6 7 8))
      (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :count 2))

  (test-lazy-seq-fn
      (list (1 2 3 4 6 8))
      (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :count 2 :from-end t))

  (test-lazy-seq-fn
      (lazy-seq (1 2 4 6 8))
      (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :start 2))

  (test-lazy-seq-fn
      (lazy-seq (1 2 4 5 6 7 8))
      (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :start 2 :count 1))

  (test-lazy-seq-fn
      (lazy-seq (1 2 4 6 7 8))
      (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :start 2 :end 6))

  (test-lazy-seq-fn
      (lazy-seq (2 4 6 7 8))
      (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :end 5))

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5 6 7 8))
      (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
		     :end 5 :count 1))

  (test-lazy-seq-fn
      (lazy-seq ((b 2) (d 4) (f 6)))
      (delete-if-not #'evenp
		     (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
		     :key #'cadr)))


;;; Test REMOVE-DUPLICATES Functions

(test lazy-seq-remove-duplicates
  "Test REMOVE-DUPLICATES on lazy sequences"

  (test-lazy-seq-fn
      (list ("bob" "alex" "jack" "john"))
      (remove-duplicates
       (lseq "alex" "john" "bob" "alex" "jack" "john")))

  (test-lazy-seq-fn
      (lazy-seq ("alex" "john" "bob" "jack"))
      (remove-duplicates
       (lseq "alex" "john" "bob" "alex" "jack" "john")
       :from-end t))

  (test-lazy-seq-fn
      (lazy-seq ("alex" "john" "bob" "alex" "jack"))
      (remove-duplicates
       (lseq "alex" "john" "bob" "alex" "jack" "john")
       :from-end t :start 1))

  (test-lazy-seq-fn
      (lazy-seq ("alex" "john" "bob" "alex" "jack" "john"))
      (remove-duplicates
       (lseq "alex" "john" "bob" "alex" "jack" "john")
       :from-end t :start 1 :end 4))

  (test-lazy-seq-fn
      (lazy-seq ((a 1) (b 2) (d 3) (f 4)))
      (remove-duplicates
       (lseq '(a 1) '(b 2) '(c 1) '(d 3) '(e 2) '(f 4))
       :from-end t :key #'cadr))

  (test-lazy-seq-fn
      (lazy-seq (#\a #\B #\c #\D))
      (remove-duplicates
       (lseq #\a #\B #\c #\D #\A #\b #\C #\d)
       :from-end t :test #'char-equal)))

(test lazy-seq-delete-duplicates
  "Test DELETE-DUPLICATES on lazy sequences"

  (test-lazy-seq-fn
      (list ("bob" "alex" "jack" "john"))
      (remove-duplicates
       (lseq "alex" "john" "bob" "alex" "jack" "john")))

  (test-lazy-seq-fn
      (lazy-seq ("alex" "john" "bob" "jack"))
      (remove-duplicates
       (lseq "alex" "john" "bob" "alex" "jack" "john")
       :from-end t))

  (test-lazy-seq-fn
      (lazy-seq ("alex" "john" "bob" "alex" "jack"))
      (remove-duplicates
       (lseq "alex" "john" "bob" "alex" "jack" "john")
       :from-end t :start 1))

  (test-lazy-seq-fn
      (lazy-seq ("alex" "john" "bob" "alex" "jack" "john"))
      (remove-duplicates
       (lseq "alex" "john" "bob" "alex" "jack" "john")
       :from-end t :start 1 :end 4))

  (test-lazy-seq-fn
      (lazy-seq ((a 1) (b 2) (d 3) (f 4)))
      (remove-duplicates
       (lseq '(a 1) '(b 2) '(c 1) '(d 3) '(e 2) '(f 4))
       :from-end t :key #'cadr))

  (test-lazy-seq-fn
      (lazy-seq (#\a #\B #\c #\D))
      (remove-duplicates
       (lseq #\a #\B #\c #\D #\A #\b #\C #\d)
       :from-end t :test #'char-equal)))


;;; Test CONCATENATE Functions

(test lazy-seq-concatenate
  "Test CONCATENATE on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4 5 6 7 8 9))
      (concatenate (lseq 1 2 3) nil '(4 5 6) (lseq 7 8 9) nil)))

(test lazy-seq-nconcatenate
  "Test NCONCATENATE on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4 5 6 7 8 9))
      (concatenate (lseq 1 2 3) nil '(4 5 6) #(7 8 9) nil)))

(test lazy-seq-concatenate-to
  "Test CONCATENATE-TO on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4 5 6 7 8 9))
      (concatenate-to 'lazy-seq #(1 2 3) nil '(4 5 6) (lseq 7 8 9) nil)))


;;; Test Mapping Functions

(test lazy-seq-map
  "Test MAP on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5))
      (map #'1+ (lseq 1 2 3 4)))

  (test-lazy-seq-fn
      (lazy-seq (3 5 7 9))
      (map #'+ (lseq 1 2 3 4) #(2 3 4 5))))

(test lazy-seq-nmap
  "Test NMAP on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5))
      (nmap #'1+ (lseq 1 2 3 4)))

  (test-lazy-seq-fn
      (lazy-seq (3 5 7 9))
      (nmap #'+ (lseq 1 2 3 4) #(2 3 4 5))))

(test lazy-seq-map-into
  "Test MAP-INTO on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4))
      (map-into (lseq 1 2) #'1+ '(2 3)))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 4))
      (map-into (lseq 1 2) #'+ '(2 3) (lseq 1 1))))

(test lazy-seq-map-to
  "Test MAP-TO on lazy sequences"

  (test-lazy-seq-fn
      (lazy-seq (2 3 4 5))
      (map-to 'lazy-seq #'1+ '(1 2 3 4)))

  (test-lazy-seq-fn
      (lazy-seq (2 4 6 8))
      (map-to 'lazy-seq #'+ '(1 2 3 4) #(1 2 3 4))))

(test lazy-seq-map-extend
  "Test MAP-EXTEND on lazy sequences"

  (flet ((f (x)
	   (list x (* 2 x))))
    (test-lazy-seq-fn
	(lazy-seq (1 2 2 4 3 6))
	(map-extend #'f (lseq 1 2 3))))

  (test-lazy-seq-fn
      (lazy-seq (1 a 2 b 3 c))
      (map-extend #'vector (lseq 1 2 3) '(a b c))))

(test lazy-seq-map-extend-into
  "Test MAP-EXTEND-INTO on lazy sequences"

  (flet ((f (x)
	   (list x (* 2 x))))
    (test-lazy-seq-fn
	(lazy-seq (1 2 3 6 4 8))
	(map-extend-into (lseq 1 2) #'f '(3 4))))

  (test-lazy-seq-fn
      (lazy-seq (1 2 3 a 4 b 5 c))
      (map-extend-into (lseq 1 2) #'vector (lseq 3 4 5) '(a b c))))

(test lazy-seq-map-extend-to
  "Test MAP-EXTEND-TO on lazy sequences"

  (flet ((f (x)
	   (list x (* 2 x))))
    (test-lazy-seq-fn
	(lazy-seq (1 2 2 4 3 6))
	(map-extend-to 'lazy-seq #'f '(1 2 3))))

  (test-lazy-seq-fn
      (lazy-seq (1 a 2 b 3 c))
      (map-extend-to 'lazy-seq #'vector '(1 2 3) #(a b c))))


;;; Test Lazy Sequence COERCE Methods

(test lazy-seq-coerce-list
  "Test COERCE lazy sequence to list"

  (is (= '(1 2 3 4 5 6)
	 (coerce (lseq 1 2 3 4 5 6) 'list))))


;;; Test Copying

(test lazy-seq-copy-shallow
  "Test shallow copying lazy sequence"

  (let* ((seq (lseq '(1) '(2) '(3)))
	 (copy (copy seq)))

    (is (typep copy 'lazy-seq))

    (is-true (every #'eq seq copy))
    (is-true (= (length seq) (length copy)))))

(test lazy-seq-copy-deep
  "Test deep copying lazy sequence"

  (let* ((seq (lseq '(1) '(2) '(3)))
	 (copy (copy seq :deep t)))

    (is (typep copy 'lazy-seq))

    (is-true
     (every
      (lambda (o c)
	(and (= o c)
	     (not (eq o c))))
      seq copy))

    (is-true (= (length seq) (length copy)))))


;;; Test RANGE

(test lazy-seq-range
  "Test RANGE function"

  (is-true (every #'= (range 0) '(0 1 2 3 4 5)))

  (is-true (every #'= (range 0 nil 2) '(0 2 4 6 8 10)))

  (is (= '(0 1 2 3 4 5) (coerce (range 0 6) 'list)))

  (is (= '(0 2 4 6 8 10) (coerce (range 0 11 2) 'list))))
