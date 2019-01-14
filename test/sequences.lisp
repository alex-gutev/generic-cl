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

(macrolet ((test-seq-fn ((&rest lists) &body tests)
	     "Tests a function on a CL:SEQUENCE and LIST-WRAPPER simultaneously.
              Each element of LISTS is of the form (SYM LIST) where
              SYM is a symbol and list is a LIST (evaluated). The
              forms in TESTS are evaluated twice, first with each SYM
              bound to the corresponding LIST, then with each SYM
              bound to a `LIST-WRAPPER' with the contents of the
              corresponding LIST. Both bindings are established by
              SYMBOL-MACROLET."

	     `(progn
		(symbol-macrolet ,lists
		  (diag "CL Sequence")
		  ,@tests)
		(symbol-macrolet
		    ,(loop for (var list) in lists
			collect `(,var (make-list-wrapper :list ,list)))
		  (diag "Generic Sequence")
		  ,@tests)))

	   (test-not-modified ((&rest seqs) &body tests)
	     "Tests that a sequence is not modified after the
              evaluation of TESTS. Each element of SEQS is of the
              form (SYM SEQ) where SYM is the symbol to which the
              result of the evaluation of SEQ is bound. Tests are
              evaluated in the environment of the bindings to each
              SYM. After the evaluation of TESTS, further tests are
              performed that check whether each SYM is equal to the
              corresponding SEQ."

	     `(progn
		(let ,seqs
		  ,@tests
		  ,@(loop for (var seq) in seqs
		       collect `(is ,var ,seq :test #'equalp "Not Modified"))))))

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
      (test-seq-fn
       ((seq (list 1 2 3 4 5))
	(res  '(a a a a a)))
       (is (fill seq 'a) res :test #'equalp))

      (test-seq-fn
       ((seq (list 1 2 3 4 5))
	(res  '(1 2 0 0 0)))
       (is (fill seq 0 :start 2) res :test #'equalp))

      (test-seq-fn
       ((seq (list 1 2 3 4 5))
	(res  '(1 0 0 4 5)))
       (is (fill seq 0 :start 1 :end 3) res :test #'equalp)))

    (subtest "Test REPLACE"
      (test-seq-fn
       ((seq1 (list 1 2 3 4 5))
	(seq2 '(x y))
	(res '(x y 3 4 5)))
       (is (replace seq1 seq2) res :test #'equalp))

      (test-seq-fn
       ((seq1 (list 1 2 3 4 5))
	(seq2 '(w x y))
	(res '(1 2 x y 5)))
       (is (replace seq1 seq2 :start1 2 :start2 1) res :test #'equalp))

      (test-seq-fn
       ((seq1 (list 1 2 3 4 5))
	(seq2 '(w x y z))
	(res '(1 x y 4 5)))
       (is (replace seq1 seq2 :start1 1 :end1 4 :start2 1 :end2 3) res :test #'equalp))

      (test-seq-fn
       ((seq1 (list 1 2 3 4 5))
	(seq2 '(w x y z))
	(res '(1 x 3 4 5)))
       (is (replace seq1 seq2 :start1 1 :end1 2 :start2 1 :end2 3) res :test #'equalp)))

    (subtest "Test REDUCE"
      (subtest "Left Reduction (:FROM-END NIL)"
	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq) '(((1 2) 3) 4)))

	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :initial-value 0) '((((0 1) 2) 3) 4)))

	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :initial-value 0 :key #'1+) '((((0 2) 3) 4) 5)))

	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :start 2) '(3 4)))

	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :start 1 :end 3) '(2 3)))

	(test-seq-fn
	 ((seq nil))
	 (is (reduce #'cl:+ seq) 0))

	(test-seq-fn
	 ((seq '(1)))
	 (is (reduce #'cl:+ seq) 1))

	(test-seq-fn
	 ((seq '(1)))
	 (is (reduce #'cl:+ seq :initial-value 2) 3))

	(test-seq-fn
	 ((seq nil))
	 (is (reduce #'list seq :initial-value 1) 1))

	(test-seq-fn
	 ((seq nil))
	 (is (reduce #'list seq :initial-value 1 :key #'1+) 1)))

      (subtest "Right Reduction (:FROM-END T)"
	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :from-end t) '(1 (2 (3 4)))))

	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :initial-value 0 :from-end t) '(1 (2 (3 (4 0))))))

	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :initial-value 0 :key #'1+ :from-end t) '(2 (3 (4 (5 0))))))

	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :start 2 :from-end t) '(3 4)))

	(test-seq-fn
	 ((seq '(1 2 3 4)))
	 (is (reduce #'list seq :start 1 :end 3 :from-end t) '(2 3)))

	(test-seq-fn
	 ((seq nil))
	 (is (reduce #'cl:+ seq :from-end t) 0))

	(test-seq-fn
	 ((seq '(1)))
	 (is (reduce #'cl:+ seq :from-end t) 1))

	(test-seq-fn
	 ((seq '(1)))
	 (is (reduce #'list seq :initial-value 2 :from-end t) '(1 2)))

	(test-seq-fn
	 ((seq nil))
	 (is (reduce #'list seq :initial-value 1 :from-end t) 1))

	(test-seq-fn
	 ((seq nil))
	 (is (reduce #'list seq :initial-value 1 :key #'1+ :from-end t) 1))))

    (subtest "Test COUNT Functions"
      (subtest "Test COUNT"
	(test-seq-fn
	 ((seq '("a" "b" "c" "a" "d")))
	 (is (count "a" seq) 2))

	(test-seq-fn
	 ((seq '("a" "b" "c" "a" "d")))
	 (is (count "a" seq :from-end t) 2))

	(test-seq-fn
	 ((seq '("a" "b" "c" "a" "d")))
	 (is (count "a" seq :start 1) 1))

	(test-seq-fn
	 ((seq '("a" "b" "c" "a" "d")))
	 (is (count "a" seq :start 1 :end 3) 0))

	(test-seq-fn
	 ((seq '(0 1 2 0 3)))
	 (is (count 1 seq :key #'1+) 2)))

      (subtest "Test COUNT-IF"
	(test-seq-fn
	 ((seq '(1 2 3 4 5)))
	 (is (count-if #'evenp seq) 2))

	(test-seq-fn
	 ((seq '(1 2 3 4 5)))
	 (is (count-if #'evenp seq :from-end t) 2))

	(test-seq-fn
	 ((seq '(1 2 3 4 5)))
	 (is (count-if #'evenp seq :start 2) 1))

	(test-seq-fn
	 ((seq '(1 2 3 4 5)))
	 (is (count-if #'evenp seq :start 2 :end 3) 0))

	(test-seq-fn
	 ((seq '(0 1 2 0 3)))
	 (is (count-if (curry #'< 2) seq :key #'1+) 2)))

      (subtest "Test COUNT-IF-NOT"
	(test-seq-fn
	 ((seq '(1 2 3 4 5)))
	 (is (count-if-not #'evenp seq) 3))

	(test-seq-fn
	 ((seq '(1 2 3 4 5)))
	 (is (count-if-not #'evenp seq :from-end t) 3))

	(test-seq-fn
	 ((seq '(1 2 3 4 5)))
	 (is (count-if-not #'evenp seq :start 2) 2))

	(test-seq-fn
	 ((seq '(1 2 3 4 5)))
	 (is (count-if-not #'evenp seq :start 2 :end 3) 1))

	(test-seq-fn
	 ((seq '(0 1 2 0 3)))
	 (is (count-if-not (curry #'< 2) seq :key #'1+) 3))))

    (subtest "Test FIND Functions"
      (subtest "Test FIND"
	(test-seq-fn
	 ((seq '("b" "a" "c" "d" "a")))
	 (is (find "a" seq) "a"))

	(test-seq-fn
	 ((seq '("b" "a" "c" "d" "a")))
	 (is (find "a" seq :from-end t) "a"))

	(test-seq-fn
	 ((seq '("b" "a" "c" "d" "a")))
	 (is (find "a" seq :start 2) "a"))

	(test-seq-fn
	 ((seq '("b" "a" "c" "d" "a")))
	 (is (find "a" seq :start 2 :end 3) nil))

	(test-seq-fn
	 ((seq '(1 3 5 6)))
	 (is (find 2 seq :key #'1+) 1)))

      (subtest "Test FIND-IF"
	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if #'evenp seq) 4))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if #'evenp seq :from-end t) 6))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if #'evenp seq :start 3) 6))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if #'evenp seq :start 3 :end 4) nil))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if #'evenp seq :key #'1+) 1)))

      (subtest "Test FIND-IF-NOT"
	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if-not #'evenp seq) 1))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if-not #'evenp seq :from-end t) 7))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if-not #'evenp seq :start 3) 5))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if-not #'evenp seq :start 2 :end 3) nil))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (find-if-not #'evenp seq :key #'1+) 4))))

    (subtest "Test POSITION Functions"
      (subtest "Test POSITION"
	(test-seq-fn
	 ((seq '("b" "a" "c" "d" "a")))
	 (is (position "a" seq) 1))

	(test-seq-fn
	 ((seq '("b" "a" "c" "d" "a")))
	 (is (position "a" seq :from-end t) 4))

	(test-seq-fn
	 ((seq '("b" "a" "c" "d" "a")))
	 (is (position "a" seq :start 2) 4))

	(test-seq-fn
	 ((seq '("b" "a" "c" "d" "a")))
	 (is (position "a" seq :start 2 :end 3) nil))

	(test-seq-fn
	 ((seq '(1 3 5 6)))
	 (is (position 2 seq :key #'1+) 0)))

      (subtest "Test POSITION-IF"
	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if #'evenp seq) 2))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if #'evenp seq :from-end t) 4))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if #'evenp seq :start 3) 4))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if #'evenp seq :start 3 :end 4) nil))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if #'evenp seq :key #'1+) 0)))

      (subtest "Test POSITION-IF-NOT"
	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if-not #'evenp seq) 0))

	(is (position-if-not #'evenp '(1 3 4 5 6 7) :from-end t) 5)
	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if-not #'evenp seq :start 3) 3))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if-not #'evenp seq :start 2 :end 3) nil))

	(test-seq-fn
	 ((seq '(1 3 4 5 6 7)))
	 (is (position-if-not #'evenp seq :key #'1+) 2))))

    (subtest "Test MISMATCH"
      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("alex" "bob" "john" "jack")) nil))

      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("alex" "Bob" "john" "Jack")) 1))

      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("alex" "bob")) 2))

      (test-seq-fn
       ((seq '("alex" "bob")))
       (is (mismatch seq '("alex" "bob" "john" "jack")) 2))

      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("alex" "Bob" "john" "Jack") :key #'string-upcase)
	   nil))

      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("alex" "Bob" "john" "Jack") :from-end t) 4))

      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("bob" "john" "jack") :start1 1) nil))

      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("Alex" "Pete" "bob" "john" "jack") :start1 1 :start2 2) nil))

      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("Alex" "Pete" "bob" "John" "jack") :start1 1 :start2 2) 2))

      (test-seq-fn
       ((seq '("alex" "bob" "john" "jack")))
       (is (mismatch seq '("Alex" "alex" "bob" "john" "Pete" "jack") :start1 1 :end1 3 :start2 2 :end2 4) nil))

      (test-seq-fn
       ((seq '("alex" "john" "jack" "bob")))
       (is (mismatch seq '("alex" "bob" "john" "jack" "pete") :from-end t :start1 1 :end1 3 :start2 2 :end2 4) nil))

      (test-seq-fn
       ((seq '("alex" "john" "jack" "bob")))
       (is (mismatch seq '("alex" "bob" "john" "jack" "pete") :from-end t :end1 3 :end2 4) 1)))

    (subtest "Test Reversing"
      (subtest "Test REVERSE"
	(diag "CL Sequences")

	(test-seq-fn
	 ((list '(1 2 3 4))
	  (res '(4 3 2 1)))

	 (test-not-modified
	  ((seq list))
	  (is (reverse seq) res :test #'equalp)))

	(test-seq-fn
	 ((seq '(1))
	  (res '(1)))
	 (is (reverse seq) res :test #'equalp))

	(test-seq-fn
	 ((seq nil)
	  (res nil))
	 (is (reverse seq) res :test #'equalp))

	(is (reverse #(a b c d)) #(d c b a) :test #'equalp))

      (subtest "Test NREVERSE"
	(test-seq-fn
	 ((seq (list 1 2 3 4))
	  (res (list 4 3 2 1)))
	 (is (nreverse seq) res :test #'equalp))

	(test-seq-fn
	 ((seq (list 1))
	  (res (list 1)))
	 (is (nreverse seq) res :test #'equalp))

	(test-seq-fn
	 ((seq nil)
	  (res nil))
	 (is (nreverse seq) res :test #'equalp))

	(is (nreverse (vector 'a 'b 'c 'd)) #(d c b a) :test #'equalp)))

    (subtest "Test Merging"
      (subtest "Test MERGE"
	(diag "CL Sequences")

	(test-seq-fn
	 ((list1 '(1 2 3 4))
	  (list2 '(5 6 7 8))
	  (res '(1 2 3 4 5 6 7 8)))

	 (test-not-modified
	  ((seq1 list1)
	   (seq2 list2))

	  (is (merge seq1 seq2 #'lessp) res :test #'equalp)))

	(test-seq-fn
	 ((list1 '(1 3 5 9))
	  (list2 '(2 4 6 7 8))
	  (res '(1 2 3 4 5 6 7 8 9)))

	 (test-not-modified
	  ((seq1 list1)
	   (seq2 list2))

	  (is (merge seq1 seq2 #'lessp) res :test #'equalp)))

	(test-seq-fn
	 ((seq1 '((a 1) (b 2) (c 5) (d 8)))
	  (seq2 '((e 3) (f 4) (g 6) (h 7)))
	  (res '((a 1) (b 2) (e 3) (f 4) (c 5) (g 6) (h 7) (d 8))))
	 (is (merge seq1 seq2 #'lessp :key #'cadr) res :test #'equalp))

	;; Test Stability
	(test-seq-fn
	 ((seq1 '((b 1) (d 1) (a 99)))
	  (seq2 '((e 1) (h 1) (f 32) (c 74)))
	  (res '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99))))
	 (is (merge seq1 seq2 #'lessp :key #'cadr) res :test #'equalp)))

      (subtest "Test NMERGE"
	(diag "CL Sequences")

	(test-seq-fn
	 ((seq1 (list 1 2 3 4))
	  (seq2 (list 5 6 7 8))
	  (res '(1 2 3 4 5 6 7 8)))
	 (is (nmerge seq1 seq2 #'lessp) res :test #'equalp))

	(test-seq-fn
	 ((seq1 (list 1 3 5 9))
	  (seq2 (list 2 4 6 7 8))
	  (res '(1 2 3 4 5 6 7 8 9)))
	 (is (nmerge seq1 seq2 #'lessp) res :test #'equalp))

	(test-seq-fn
	 ((seq1 (list '(a 1) '(b 2) '(c 5) '(d 8)))
	  (seq2 (list '(e 3) '(f 4) '(g 6) '(h 7)))
	  (res '((a 1) (b 2) (e 3) (f 4) (c 5) (g 6) (h 7) (d 8))))
	 (is (nmerge seq1 seq2 #'lessp :key #'cadr) res :test #'equalp))

	;; Test Stability
	(test-seq-fn
	 ((seq1 (list '(b 1) '(d 1) '(a 99)))
	  (seq2 (list '(e 1) '(h 1) '(f 32) '(c 74)))
	  (res '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99))))
	 (is (nmerge seq1 seq2 #'lessp :key #'cadr) res :test #'equalp))))

    (subtest "Test Sorting"
      (subtest "Test SORT Functions"
	(subtest "Test SORT"
	  (diag "CL Sequences")

	  (test-seq-fn
	   ((list '("aac" "zzz" "aaa" "aab" "bac" "baa"))
	    (res '("aaa" "aab" "aac" "baa" "bac" "zzz")))

	   (test-not-modified
	    ((seq list))
	    (is (sort seq) res :test #'equalp)))

	  (test-seq-fn
	   ((seq '(99 3 74 56 1 32 49))
	    (res '(99 74 56 49 32 3 1)))
	   (is (sort seq :test #'greaterp) res :test #'equalp))

	  (test-seq-fn
	   ((seq '((a 99) (b 3) (c 74) (d 56) (e 1) (f 32) (h 49)))
	    (res '((e 1) (b 3) (f 32) (h 49) (d 56) (c 74) (a 99))))
	   (is (sort seq :key #'cadr) res :test #'equalp))

	  (is (sort #(99 3 74 56 1 32 49) :test #'greaterp) #(99 74 56 49 32 3 1) :test #'equalp))

	(subtest "Test NSORT"
	  (test-seq-fn
	   ((seq (list "aac" "zzz" "aaa" "aab" "bac" "baa"))
	    (res '("aaa" "aab" "aac" "baa" "bac" "zzz")))
	   (is (nsort seq) res :test #'equalp))

	  (test-seq-fn
	   ((seq (list 99 3 74 56 1 32 49))
	    (res '(99 74 56 49 32 3 1)))
	   (is (nsort seq :test #'greaterp) res :test #'equalp))

	  (test-seq-fn
	   ((seq (list '(a 99) '(b 3) '(c 74) '(d 56) '(e 1) '(f 32) '(h 49)))
	    (res '((e 1) (b 3) (f 32) (h 49) (d 56) (c 74) (a 99))))
	   (is (nsort seq :key #'cadr) res :test #'equalp))

	  (is (nsort (vector 99 3 74 56 1 32 49) :test #'greaterp) #(99 74 56 49 32 3 1) :test #'equalp)))

      (subtest "Test STABLE-SORT Functions"
	(subtest "Test STABLE-SORT"
	  (test-seq-fn
	   ((list '("aac" "zzz" "aaa" "aab" "bac" "baa"))
	    (res '("aaa" "aab" "aac" "baa" "bac" "zzz")))

	   (test-not-modified
	    ((seq list))
	    (is (stable-sort seq) res :test #'equalp)))

	  (test-seq-fn
	   ((seq '(99 3 74 56 1 32 49))
	    (res '(99 74 56 49 32 3 1)))
	   (is (stable-sort seq :test #'greaterp) res :test #'equalp))

	  (test-seq-fn
	   ((seq '(99 3 74 56 1 32 49))
	    (res '(99 74 56 49 32 3 1)))
	   (is (stable-sort seq :test #'greaterp) res :test #'equalp))

	  (is (stable-sort #(99 3 74 56 1 32 49) :test #'greaterp) #(99 74 56 49 32 3 1) :test #'equalp)

	  ;; Test Stability
	  (test-seq-fn
	   ((seq '((a 99) (b 1) (c 74) (d 1) (e 1) (f 32) (h 1)))
	    (res '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99))))
	   (is (stable-sort seq :key #'cadr) res :test #'equalp)))


	(subtest "Test STABLE-NSORT"
	  (diag "CL Sequences")

	  (test-seq-fn
	   ((seq (list "aac" "zzz" "aaa" "aab" "bac" "baa"))
	    (res (list "aaa" "aab" "aac" "baa" "bac" "zzz")))
	   (is (stable-nsort seq) res :test #'equalp))

	  (test-seq-fn
	   ((seq (list 99 3 74 56 1 32 49))
	    (res '(99 74 56 49 32 3 1)))
	   (is (stable-nsort seq :test #'greaterp) res :test #'equalp))

	  (is (stable-nsort (vector 99 3 74 56 1 32 49) :test #'greaterp) #(99 74 56 49 32 3 1) :test #'equalp)

	  ;; Test Stability
	  (test-seq-fn
	   ((seq (list '(a 99) '(b 1) '(c 74) '(d 1) '(e 1) '(f 32) '(h 1)))
	    (res '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99))))
	   (is (stable-nsort seq :key #'cadr) res :test #'equalp)))))

    (subtest "Test SUBSTITUTE Functions"
      (subtest "Test SUBSTITUTE"
	(test-seq-fn
	 ((list '("a" "b" "old" "c" "old" "d"))
	  (res '("a" "b" "new" "c" "new" "d")))

	 (test-not-modified
	  ((seq list))
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
	  ((seq list))
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
	 ((seq (list 1 2 3 4 5 6 7 8))
	  (res (list 1 'x 3 'x 5 'x 7 'x)))

	 (is (substitute-if 'x #'evenp seq) res :test #'equalp))

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
	(test-seq-fn
	 ((list '(1 2 3 4 5 6 7 8))
	  (res '(x 2 x 4 x 6 x 8)))

	 (test-not-modified
	  ((seq list))
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
	 (is (nsubstitute-if-not 'x #'evenp seq :key #'cadr) res :test #'equalp))))

    (subtest "Test REMOVE Functions"
      (subtest "Test REMOVE"
	(test-seq-fn
	 ((list '("a" "b" "old" "c" "old" "d"))
	  (res '("a" "b" "c" "d")))

	 (test-not-modified
	  ((seq list))
	  (is (remove "old" seq) res :test #'equalp)))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(2 3 4 5)))
	 (is (remove 1 seq) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(2 3 4 5 1)))
	 (is (remove 1 seq :count 2) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(1 2 3 4 5)))
	 (is (remove 1 seq :count 2 :from-end t) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(1 2 3 4 5)))
	 (is (remove 1 seq :start 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(1 2 3 4 5 1)))
	 (is (remove 1 seq :start 1 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(1 2 3 4 5 1)))
	 (is (remove 1 seq :start 1 :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(2 3 4 5 1)))
	 (is (remove 1 seq :end 5) res :test #'equalp))

	(test-seq-fn
	 ((seq '(1 2 3 1 4 5 1))
	  (res '(2 3 1 4 5 1)))
	 (is (remove 1 seq :end 5 :count 1) res :test #'equalp))

	(test-seq-fn
	 ((seq '((a 1) (b 2) (c 3) (d 1) (e 4) (f 5) (g 1)))
	  (res '((b 2) (c 3) (e 4) (f 5))))
	 (is (remove 1 seq :key #'cadr) res :test #'equalp)))

      (subtest "Test DELETE"
      	(test-seq-fn
      	 ((seq (list "a" "b" "old" "c" "old" "d"))
      	  (res (list "a" "b" "c" "d")))

      	 (is (delete "old" seq) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 1 4 5 1))
      	  (res (list 2 3 4 5)))
      	 (is (delete 1 seq) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 1 4 5 1))
      	  (res (list 2 3 4 5 1)))
      	 (is (delete 1 seq :count 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 1 4 5 1))
      	  (res (list 1 2 3 4 5)))
      	 (is (delete 1 seq :count 2 :from-end t) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 1 4 5 1))
      	  (res (list 1 2 3 4 5)))
      	 (is (delete 1 seq :start 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 1 4 5 1))
      	  (res (list 1 2 3 4 5 1)))
      	 (is (delete 1 seq :start 1 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 1 4 5 1))
      	  (res (list 1 2 3 4 5 1)))
      	 (is (delete 1 seq :start 1 :end 5) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 1 4 5 1))
      	  (res (list 2 3 4 5 1)))
      	 (is (delete 1 seq :end 5) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 1 4 5 1))
      	  (res (list 2 3 1 4 5 1)))
      	 (is (delete 1 seq :end 5 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1)))
      	  (res (list '(b 2) '(c 3) '(e 4) '(f 5))))
      	 (is (delete 1 seq :key #'cadr) res :test #'equalp)))

      (subtest "Test REMOVE-IF"
      	(test-seq-fn
      	 ((list '(1 2 3 4 5 6 7 8))
      	  (res '(1 3 5 7)))

      	 (test-not-modified
      	  ((seq list))
      	  (is (remove-if #'evenp seq) res :test #'equalp)))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 3 5 6 7 8)))
      	 (is (remove-if #'evenp seq :count 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 2 3 4 5 7)))
      	 (is (remove-if #'evenp seq :count 2 :from-end t) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 2 3 5 7)))
      	 (is (remove-if #'evenp seq :start 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 2 3 5 6 7 8)))
      	 (is (remove-if #'evenp seq :start 2 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 2 3 5 7 8)))
      	 (is (remove-if #'evenp seq :start 2 :end 6) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 3 5 6 7 8)))
      	 (is (remove-if #'evenp seq :end 5) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 3 4 5 6 7 8)))
      	 (is (remove-if #'evenp seq :end 5 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
      	  (res '((a 1) (c 3) (e 5) (g 7))))
      	 (is (remove-if #'evenp seq :key #'cadr) res :test #'equalp)))

      (subtest "Test DELETE-IF"
      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 3 5 7)))

      	 (is (delete-if #'evenp seq) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 3 5 6 7 8)))
      	 (is (delete-if #'evenp seq :count 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 2 3 4 5 7)))
      	 (is (delete-if #'evenp seq :count 2 :from-end t) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 2 3 5 7)))
      	 (is (delete-if #'evenp seq :start 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 2 3 5 6 7 8)))
      	 (is (delete-if #'evenp seq :start 2 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 2 3 5 7 8)))
      	 (is (delete-if #'evenp seq :start 2 :end 6) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 3 5 6 7 8)))
      	 (is (delete-if #'evenp seq :end 5) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 3 4 5 6 7 8)))
      	 (is (delete-if #'evenp seq :end 5 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7)))
      	  (res (list '(a 1) '(c 3) '(e 5) '(g 7))))
      	 (is (delete-if #'evenp seq :key #'cadr) res :test #'equalp)))

      (subtest "Test REMOVE-IF-NOT"
      	(test-seq-fn
      	 ((list '(1 2 3 4 5 6 7 8))
      	  (res '(2 4 6 8)))

      	 (test-not-modified
      	  ((seq list))
      	  (is (remove-if-not #'evenp seq) res :test #'equalp)))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(2 4 5 6 7 8)))

      	 (is (remove-if-not #'evenp seq :count 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 2 3 4 6 8)))
      	 (is (remove-if-not #'evenp seq :count 2 :from-end t) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 2 4 6 8)))
      	 (is (remove-if-not #'evenp seq :start 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 2 4 5 6 7 8)))
      	 (is (remove-if-not #'evenp seq :start 2 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(1 2 4 6 7 8)))
      	 (is (remove-if-not #'evenp seq :start 2 :end 6) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(2 4 6 7 8)))
      	 (is (remove-if-not #'evenp seq :end 5) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '(1 2 3 4 5 6 7 8))
      	  (res '(2 3 4 5 6 7 8)))
      	 (is (remove-if-not #'evenp seq :end 5 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
      	  (res '((b 2) (d 4) (f 6))))
      	 (is (remove-if-not #'evenp seq :key #'cadr) res :test #'equalp)))

      (subtest "Test DELETE-IF-NOT"
      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 2 4 6 8)))

      	 (is (delete-if-not #'evenp seq) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 2 4 5 6 7 8)))

      	 (is (delete-if-not #'evenp seq :count 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 2 3 4 6 8)))
      	 (is (delete-if-not #'evenp seq :count 2 :from-end t) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 2 4 6 8)))
      	 (is (delete-if-not #'evenp seq :start 2) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 2 4 5 6 7 8)))
      	 (is (delete-if-not #'evenp seq :start 2 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 1 2 4 6 7 8)))
      	 (is (delete-if-not #'evenp seq :start 2 :end 6) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 2 4 6 7 8)))
      	 (is (delete-if-not #'evenp seq :end 5) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list 1 2 3 4 5 6 7 8))
      	  (res (list 2 3 4 5 6 7 8)))
      	 (is (delete-if-not #'evenp seq :end 5 :count 1) res :test #'equalp))

      	(test-seq-fn
      	 ((seq (list '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7)))
      	  (res (list '(b 2) '(d 4) '(f 6))))
      	 (is (delete-if-not #'evenp seq :key #'cadr) res :test #'equalp))))))

(finalize)
