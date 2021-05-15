;;;; sequences.lisp
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

;;;; Unit tests for the generic sequences interface

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite sequences
    :description "Test generic sequence functions"
    :in generic-cl)

(in-suite sequences)


;;; Custom Sequence Type

(defstruct list-wrapper
  "Custom sequence type used for testing the generic sequence
   functions. The sequence simply wraps a list which is stored in the
   LIST slot."

  list)

(defun list-wrap (&rest elems)
  "Create a `LIST-WRAPPER' with elements ELEMS."

  (make-list-wrapper :list elems))

(defmethod equalp ((a list-wrapper) (b list-wrapper))
  (equalp (list-wrapper-list a) (list-wrapper-list b)))


;;;; Custom Sequence Iterator

(defstruct (list-wrapper-iterator (:include iterator)
				  (:copier nil))

  "Custom Sequence Iterator."
  it)

(defmethod make-iterator ((seq list-wrapper) start end)
  (make-list-wrapper-iterator :it (make-iterator (list-wrapper-list seq) start end)))

(defmethod make-reverse-iterator ((seq list-wrapper) start end)
  (make-list-wrapper-iterator :it (make-reverse-iterator (list-wrapper-list seq) start end)))

(defmethod copy ((it list-wrapper-iterator) &key)
  (make-list-wrapper-iterator :it (copy (list-wrapper-iterator-it it))))


(defmethod at ((it list-wrapper-iterator))
  (at (list-wrapper-iterator-it it)))

(defmethod (setf at) (value (it list-wrapper-iterator))
  (setf (at (list-wrapper-iterator-it it)) value))

(defmethod advance ((it list-wrapper-iterator))
  (advance (list-wrapper-iterator-it it)))

(defmethod endp ((it list-wrapper-iterator))
  (endp (list-wrapper-iterator-it it)))


;;;; Custom Sequence Collector

(defmethod cleared ((seq list-wrapper) &key)
  (make-list-wrapper))

(defmethod make-sequence-of-type ((type (eql 'list-wrapper)) (args t))
  (make-list-wrapper))


(defstruct list-wrapper-collector
  "Custom Sequence Collector."

  collector)

(defmethod make-collector ((seq list-wrapper) &key front)
  (make-list-wrapper-collector :collector (make-collector (list-wrapper-list seq) :front front)))

(defmethod accumulate ((c list-wrapper-collector) item)
  (accumulate (list-wrapper-collector-collector c) item))

(defmethod collector-sequence ((c list-wrapper-collector))
  (make-list-wrapper :list (collector-sequence (list-wrapper-collector-collector c))))


;;; Test Utilities

(defmacro test-seq-fn (name (&rest lists) &body tests)
  "Tests a function on a CL:SEQUENCE and LIST-WRAPPER simultaneously.
   Each element of LISTS is of the form (SYM LIST) where SYM is a
   symbol and list is a LIST (evaluated). The forms in TESTS are
   evaluated twice, first with each SYM bound to the corresponding
   LIST, then with each SYM bound to a `LIST-WRAPPER' with the
   contents of the corresponding LIST. Both bindings are established
   by SYMBOL-MACROLET."

  (destructuring-bind (name &rest args) (ensure-list name)
    `(progn
       (test ,(alet (symbolicate 'sequence- name)
		(if args `(,it ,@args) it))

	 ,(format nil "Test ~a on CL sequences" name)

	 (symbol-macrolet ,lists
	   ,@tests))

       (test ,(alet (symbolicate 'generic- name)
		(if args `(,it ,@args) it))

	 ,(format nil "Test ~a on generic sequences" name)

	 (symbol-macrolet
	     ,(loop for (var list) in lists
		 collect `(,var (make-list-wrapper :list ,list)))
	   ,@tests)))))

(defmacro test-not-modified ((&rest seqs) &body tests)
  "Tests that a sequence is not modified after the evaluation of
   TESTS. Each element of SEQS is of the form (SYM SEQ) where SYM is
   the symbol to which the result of the evaluation of SEQ is
   bound. Tests are evaluated in the environment of the bindings to
   each SYM. After the evaluation of TESTS, further tests are
   performed that check whether each SYM is equal to the corresponding
   SEQ."

  `(progn
     (let ,seqs
       ,@tests
       ,@(loop for (var seq) in seqs
	    collect
	      `(is (= ,seq ,var)
		   "~%~TSequence:~%~%~s~%~%modified to:~%~%~s~%~%~Tafter non-destructive operation."
		   ,seq ,var)))))


;;; Test Size Functions

(test length
  "Test LENGTH function"

  (is-every =
    (4 (length '(1 2 3 4)))
    (5 (length #(a b c d e)))
    (3 (length (make-array 7 :adjustable t :initial-element 0 :fill-pointer 3)))
    (3 (length (alist-hash-map '((a . 1) (b . 2) (c . 3)))))))

(test emptyp
  "Test EMPTYP function"

  (is-true (emptyp nil))
  (is-true (emptyp (make-hash-map)))
  (is-true (list-wrap))

  (is-false (emptyp '(1 2 3)))
  (is-false (emptyp #(a b c)))
  (is-false (emptyp #2A((1 2 3) (4 5 6))))
  (is-false (emptyp (alist-hash-map '((a . 1) (b . 2)))))
  (is-false (emptyp (list-wrap 1 2 3))))

(test clear
  "Test CLEAR function"

  (alet (make-array 5 :initial-contents '(1 2 3 4 5) :adjustable t)
    (clear it)
    (is (= 0 (cl:length it))))

  (alet (make-array 5 :initial-contents '(1 2 3 4 5) :adjustable t :fill-pointer t)
    (clear it)
    (is (= 0 (cl:length it))))

  (signals type-error (clear #(1 2 3))))

(test list-adjust-size
  "Test ADJUST-SIZE on lists"

  (is-every =
    ('(1 2) (adjust-size '(1 2 3 4 5) 2))
    ('(1 2 3) (adjust-size (list 1 2 3 4 5) 3))
    ('(1 2 3 4 5 nil nil) (adjust-size '(1 2 3 4 5) 7))
    ('(a b c d x x) (adjust-size '(a b c d) 6 :element 'x))
    ('(1 2 3 4) (adjust-size '(1 2 3 4) 4))
    ('() (adjust-size '(1 2 3 4) 0))
    ('(4 4 4) (adjust-size nil 3 :element 4)))

  (signals type-error (adjust-size '(1 2 3) -1))
  (signals type-error (adjust-size '(1 2 3) 0.5)))

(test vector-adjust-size
  "Test ADJUST-SIZE on vectors"

  (is-every =
    (#(1 2) (adjust-size #(1 2 3 4 5) 2))
    (#(1 2 3) (adjust-size (make-array 5 :initial-contents '(1 2 3 4 5)) 3))
    (#(1 2 3 4 5 nil nil) (adjust-size #(1 2 3 4 5) 7))
    (#(a b c d x x) (adjust-size #(a b c d) 6 :element 'x))
    (#(1 2 3 4) (adjust-size #(1 2 3 4) 4))
    (#() (adjust-size #(1 2 3 4) 0))
    (#(4 4 4) (adjust-size (vector) 3 :element 4)))

  (signals type-error (adjust-size #(1 2 3) -1))
  (signals type-error (adjust-size #(1 2 3) 0.5)))

(test custom-sequence-adjust-size
  "Test ADJUST-SIZE on custom sequence type"

  (is-every =
    ((list-wrap 1 2) (adjust-size (list-wrap 1 2 3 4 5) 2))
    ((list-wrap 1 2 3) (adjust-size (list-wrap 1 2 3 4 5) 3))
    ((list-wrap 1 2 3 4 5 nil nil) (adjust-size (list-wrap 1 2 3 4 5) 7))
    ((list-wrap 'a 'b 'c 'd 'x 'x) (adjust-size (list-wrap 'a 'b 'c 'd) 6 :element 'x))
    ((list-wrap 1 2 3 4) (adjust-size (list-wrap 1 2 3 4) 4))
    ((list-wrap) (adjust-size (list-wrap 1 2 3 4) 0))
    ((list-wrap 1 1 1) (adjust-size (list-wrap) 3 :element 1)))

  (signals type-error (adjust-size (list-wrap 1 2 3) -1))
  (signals type-error (adjust-size (list-wrap 1 2 3) 0.5)))

(test list-nadjust-size
  "Test NADJUST-SIZE on lists"

  (is-every =
    ('(1 2) (nadjust-size (list 1 2 3 4 5) 2))
    ('(1 2 3) (nadjust-size (list 1 2 3 4 5) 3))
    ('(1 2 3 4 5 nil nil) (nadjust-size (list 1 2 3 4 5) 7))
    ('(a b c d x x) (nadjust-size (list 'a 'b 'c 'd) 6 :element 'x))
    ('(1 2 3 4) (nadjust-size (list 1 2 3 4) 4))
    ('() (nadjust-size (list 1 2 3 4) 0))
    ('(4 4 4) (nadjust-size nil 3 :element 4)))

  (signals type-error (nadjust-size (list 1 2 3) -1))
  (signals type-error (nadjust-size (list 1 2 3) 0.5)))

(test vector-nadjust-size
  "Test NADJUST-SIZE on vectors"

  (is-every =
    (#(1 2) (nadjust-size (vector 1 2 3 4 5) 2))
    (#(1 2 3) (nadjust-size (make-array 5 :initial-contents '(1 2 3 4 5)) 3))
    (#(1 2 3 4 5 nil nil) (nadjust-size (vector 1 2 3 4 5) 7))
    (#(a b c d x x) (nadjust-size (vector 'a 'b 'c 'd) 6 :element 'x))
    (#(1 2 3 4) (nadjust-size (vector 1 2 3 4) 4))
    (#() (nadjust-size (vector 1 2 3 4) 0))
    (#(4 4 4) (nadjust-size (vector) 3 :element 4)))

  (signals type-error (nadjust-size (vector 1 2 3) -1))
  (signals type-error (nadjust-size (vector 1 2 3) 0.5)))

(test custom-sequence-nadjust-size
  "Test NADJUST-SIZE on custom sequence type"

  (is-every =
    ((list-wrap 1 2) (nadjust-size (list-wrap 1 2 3 4 5) 2))
    ((list-wrap 1 2 3) (nadjust-size (list-wrap 1 2 3 4 5) 3))
    ((list-wrap 1 2 3 4 5 nil nil) (nadjust-size (list-wrap 1 2 3 4 5) 7))
    ((list-wrap 'a 'b 'c 'd 'x 'x) (nadjust-size (list-wrap 'a 'b 'c 'd) 6 :element 'x))
    ((list-wrap 1 2 3 4) (nadjust-size (list-wrap 1 2 3 4) 4))
    ((list-wrap) (nadjust-size (list-wrap 1 2 3 4) 0))
    ((list-wrap 1 1 1) (nadjust-size (list-wrap) 3 :element 1)))

  (signals type-error (nadjust-size (list-wrap 1 2 3) -1))
  (signals type-error (nadjust-size (list-wrap 1 2 3) 0.5)))


;;; Test element functions

(test elt
  "Test generic ELT function"

  (loop for i below 4
     do
       (is-every =
	 ((cl:elt '(1 2 3 4) i) (elt '(1 2 3 4) i))
	 ((cl:elt #(a b c d) i) (elt #(a b c d) i))
	 ((row-major-aref #2A((1 2) (3 4)) i) (elt #2A((1 2) (3 4)) i))
	 ((nth i '(w x y z)) (elt (list-wrap 'w 'x 'y 'z) i))
	 ((cl:elt '(10 20 30 40) i)
          (elt (alist-hash-map '((0 . 10) (1 . 20) (2 . 30) (3 . 40))) i)))

     with vals = '(10 20 30 40)
     with test-hash-table = (loop with h = (make-hash-table)
                               for i from 0
                               for e in vals do (setf (gethash i h) e)
                               finally (return h))

     do (is (= (cl:elt '(10 20 30 40) i) (elt test-hash-table i)))))

(test setf-elt
  "Test generic (SETF ELT) function"

  (alet (list 1 2 3 4)
    (is (= 'x (setf (elt it 2) 'x)))
    (is (= '(1 2 x 4) it)))

  (alet (vector 1 2 3 4)
    (is (= 'x (setf (elt it 1) 'x)))
    (is (= #(1 x 3 4) it)))

  (alet (make-array '(2 2) :initial-contents '((1 2) (3 4)))
    (is (= 'a (setf (elt it 3) 'a)))
    (is (= 'b (setf (elt it 0) 'b)))
    (is (= #2A((b 2) (3 a)) it)))

  (alet (list-wrap 1 2 3 4)
    (is (= 'z (setf (elt it 2) 'z)))
    (is (= (list-wrap 1 2 'z 4) it)))

  (alet (loop with h = (make-hash-table)
	   for i from 0
	   for e in '(10 20 30 40) do (setf (gethash i h) e)
           finally (return h))

    (is (= 100 (setf (elt it 1) 100)))

    (is (= (ensure-hash-map  (loop with h = (make-hash-table)
				for i from 0
				for e in '(10 100 30 40) do (setf (gethash i h) e)
				finally (return h)))

           (ensure-hash-map it))))

  (alet (alist-hash-map '((0 . 10) (1 . 20) (2 . 30) (3 . 40)))
    (is (= 100 (setf (elt it 1) 100)))
    (is (= (alist-hash-map '((0 . 10) (1 . 100) (2 . 30) (3 . 40))) it))))

(test multi-dimensional-array-elt
  "Test ELT and (SETF ELT) on multi-dimensional arrays"

  (alet (make-array '(3 2 2) :initial-contents '(((1 0) (0 1)) ((2 3) (4 5)) ((5 6) (7 8))))
    (is (= 1 (elt it 0)))
    (is (= #2A((2 3) (4 5)) (elt it '(1))))
    (setf (elt it '(1 0)) #(200 300))
    (is (= #2A((200 300) (4 5)) (elt it '(1))))
    (setf (elt it '(1)) #2A((2000 3000) (4000 5000)))
    (is (= #2A((2000 3000) (4000 5000)) (elt it '(1))))
    (setf (elt it nil) #3A(((0 0) (0 0)) ((0 0) (0 0)) ((0 0) (0 0))))
    (is (= #3A(((0 0) (0 0)) ((0 0) (0 0)) ((0 0) (0 0))) it))))

(test first
  "Test generic FIRST function"

  (is-every =
    (1 (first '(1 2 3 4)))
    ('a (first #(a b c d)))
    (1 (first #2A((1 2) (3 4))))
    ('x (first (list-wrap 'x 'y 'z)))))

(test last
  "Test generic LAST function"

  (is-every =
    (4 (last '(1 2 3 4)))
    ('d (last #(a b c d)))
    (4 (last #2A((1 2) (3 4))))
    ('z (last (list-wrap 'x 'y 'z))))

  (macrolet ((test-last (seq n)
	       `(is (= (elt ,seq (- (length ,seq) 1 ,n)) (last ,seq ,n)))))

    (loop for n below 4
       do
	 (test-last '(1 2 3 4) n)
	 (test-last #(a b c d) n)
	 (test-last #2A((1 2) (3 4)) n)
	 (test-last (list-wrap 'w 'x 'y 'z) n))))

(test lastcdr
  "Test LASTCDR function"

  (let ((list '(1 2 3 4)))
    (is-every eq
      ((cl:last list) (lastcdr list))
      ((cl:last list 1) (lastcdr list 1))
      ((cl:last list 3) (lastcdr list 3)))))

(test erase
  "Test generic ERASE function"

  (alet (make-array 5 :initial-contents '(1 2 3 4 5) :adjustable t)
    (erase it 2)
    (is (= #(1 2 4 5) it))

    (erase it 0)
    (is (= #(2 4 5) it)))

  (alet (make-array 5 :initial-contents '(1 2 3 4 5) :adjustable t :fill-pointer t)
    (erase it 2)
    (is (= #(1 2 4 5) it))

    (erase it 0)
    (is (= #(2 4 5) it)))

  (signals type-error (erase #(1 2 3 4) 1)))


;;; Test SUBSEQ

(test sequence-subseq
  "Test SUBSEQ on CL sequences"

  (is (= '(2 3) (subseq '(1 2 3 4 5) 1 3)))
  (is (= '(3 4 5) (subseq '(1 2 3 4 5) 2))))

(test generic-subseq
  "Test SUBSEQ on generic sequences"

  (is (= (make-list-wrapper :list '(2 3))
	 (subseq (make-list-wrapper :list '(1 2 3 4 5)) 1 3)))

  (is (= (make-list-wrapper :list '(3 4 5))
	 (subseq (make-list-wrapper :list '(1 2 3 4 5)) 2))))

(test sequence-setf-subseq
  "Test (SETF SUBSEQ) on CL sequences"

  (let ((list (list 1 2 3 4 5)))
    (setf (subseq list 1 3) '(x y))
    (is (= '(1 x y 4 5) list))))

(test generic-setf-subseq
  "Test (SETF SUBSEQ on generic sequences"

  (let ((wrapper (make-list-wrapper :list (list 1 2 3 4 5))))
    (setf (subseq wrapper 1 3) '(x y))
    (is (= (make-list-wrapper :list '(1 x y 4 5)) wrapper))))


;;; Test FILL

(test-seq-fn fill
    ((seq (list 1 2 3 4 5))
     (res  '(a a a a a)))

  (is (= res (fill seq 'a))))

(test-seq-fn fill-start
    ((seq (list 1 2 3 4 5))
     (res  '(1 2 0 0 0)))

  (is (= res (fill seq 0 :start 2))))

(test-seq-fn fill-bounded
    ((seq (list 1 2 3 4 5))
     (res  '(1 0 0 4 5)))

  (is (= res (fill seq 0 :start 1 :end 3))))


;;; Test REPLACE

(test-seq-fn replace
    ((seq1 (list 1 2 3 4 5))
     (seq2 '(x y))
     (res '(x y 3 4 5)))

  (is (= res (replace seq1 seq2))))

(test-seq-fn replace-start
    ((seq1 (list 1 2 3 4 5))
     (seq2 '(w x y))
     (res '(1 2 x y 5)))

  (is (= res (replace seq1 seq2 :start1 2 :start2 1))))

(test-seq-fn replace-bounded-1
    ((seq1 (list 1 2 3 4 5))
     (seq2 '(w x y z))
     (res '(1 x y 4 5)))

  (is (= res (replace seq1 seq2 :start1 1 :end1 4 :start2 1 :end2 3))))

(test-seq-fn replace-bounded-2
    ((seq1 (list 1 2 3 4 5))
     (seq2 '(w x y z))
     (res '(1 x 3 4 5)))

  (is (= res (replace seq1 seq2 :start1 1 :end1 2 :start2 1 :end2 3))))


;;; Test REDUCE

(test-seq-fn reduce-left
    ((seq '(1 2 3 4)))

  (is-every =
    ('(((1 2) 3) 4) (reduce #'list seq))
    ('((((0 1) 2) 3) 4) (reduce #'list seq :initial-value 0))
    ('((((0 2) 3) 4) 5) (reduce #'list seq :initial-value 0 :key #'1+))
    ('(3 4) (reduce #'list seq :start 2))
    ('(2 3) (reduce #'list seq :start 1 :end 3))))

(test-seq-fn reduce-left-empty
    ((seq nil))

  (is-every =
    (0 (reduce #'cl:+ seq))
    (1 (reduce #'list seq :initial-value 1))
    (1 (reduce #'list seq :initial-value 1 :key #'1+))))

(test-seq-fn reduce-left-single-element
    ((seq '(1)))

  (is-every =
    (1 (reduce #'cl:+ seq))
    (3 (reduce #'cl:+ seq :initial-value 2))))

(test-seq-fn reduce-right
    ((seq '(1 2 3 4)))

  (is-every =
    ('(1 (2 (3 4))) (reduce #'list seq :from-end t))
    ('(1 (2 (3 (4 0)))) (reduce #'list seq :initial-value 0 :from-end t))
    ('(2 (3 (4 (5 0)))) (reduce #'list seq :initial-value 0 :key #'1+ :from-end t))
    ('(3 4) (reduce #'list seq :start 2 :from-end t))
    ('(2 3) (reduce #'list seq :start 1 :end 3 :from-end t))))

(test-seq-fn reduce-right-empty
    ((seq nil))

  (is-every =
    (0 (reduce #'cl:+ seq :from-end t))
    (1 (reduce #'list seq :initial-value 1 :from-end t))
    (1 (reduce #'list seq :initial-value 1 :key #'1+ :from-end t))))

(test-seq-fn reduce-right-single-element
    ((seq '(1)))

  (is-every =
    (1 (reduce #'cl:+ seq :from-end t))
    ('(1 2) (reduce #'list seq :initial-value 2 :from-end t))))


;;; Test COUNT functions

(test-seq-fn count
    ((seq '("a" "b" "c" "a" "d")))

  (is-every =
    (2 (count "a" seq))
    (2 (count "a" seq :from-end t))
    (1 (count "a" seq :start 1))
    (0 (count "a" seq :start 1 :end 3))
    (2 (count "A" seq :key #'string-upcase))))

(test-seq-fn count-integer
    ((seq '(0 1 2 0 3)))

  (is (= 2 (count 1 seq :key #'1+))))

(test-seq-fn count-empty
    ((seq nil))

  (is (= 0 (count 'x seq))))

(test-seq-fn count-if
    ((seq '(1 2 3 4 5)))

  (is-every =
    (2 (count-if #'evenp seq))
    (2 (count-if #'evenp seq :from-end t))
    (1 (count-if #'evenp seq :start 2))
    (0 (count-if #'evenp seq :start 2 :end 3))))

(test-seq-fn count-if-integer
    ((seq '(0 1 2 0 3)))

  (is (= 2 (count-if (curry #'< 2) seq :key #'1+))))

(test-seq-fn count-if-empty
    ((seq nil))

  (is (= 0 (count-if #'evenp seq))))

(test-seq-fn count-if-not
    ((seq '(1 2 3 4 5)))

  (is-every =
    (3 (count-if-not #'evenp seq))
    (3 (count-if-not #'evenp seq :from-end t))
    (2 (count-if-not #'evenp seq :start 2))
    (1 (count-if-not #'evenp seq :start 2 :end 3))))

(test-seq-fn count-if-not-integer
    ((seq '(0 1 2 0 3)))

  (is (= 3 (count-if-not (curry #'< 2) seq :key #'1+))))

(test-seq-fn count-if-not-empty
    ((seq nil))

  (is (= 0 (count-if-not #'evenp seq))))


;;; Test FIND functions

(test-seq-fn find
    ((seq '("b" "a" "c" "d" "a")))

  (is-every =
    ("a" (find "a" seq))
    ("a" (find "a" seq :from-end t))
    ("a" (find "a" seq :start 2))
    (nil (find "a" seq :start 2 :end 3))))

(test-seq-fn find-integer
    ((seq '(1 3 5 6)))

  (is (= 1 (find 2 seq :key #'1+))))

(test-seq-fn find-empty
    ((seq nil))

  (is (= nil (find 'x seq))))

(test-seq-fn find-if
    ((seq '(1 3 4 5 6 7))
     (empty nil))

  (is-every =
    (4 (find-if #'evenp seq))
    (6 (find-if #'evenp seq :from-end t))
    (6 (find-if #'evenp seq :start 3))
    (nil (find-if #'evenp seq :start 3 :end 4))
    (1 (find-if #'evenp seq :key #'1+))
    (nil (find-if #'evenp empty))))

(test-seq-fn find-if-not
    ((seq '(1 3 4 5 6 7))
     (empty nil))

  (is-every =
    (1 (find-if-not #'evenp seq))
    (7 (find-if-not #'evenp seq :from-end t))
    (5 (find-if-not #'evenp seq :start 3))
    (nil (find-if-not #'evenp seq :start 2 :end 3))
    (4 (find-if-not #'evenp seq :key #'1+))

    (nil (find-if-not #'evenp empty))))


;;; Test POSITION Functions

(test-seq-fn position
    ((seq '("b" "a" "c" "d" "a")))

  (is-every =
    (1 (position "a" seq))
    (4 (position "a" seq :from-end t))
    (4 (position "a" seq :start 2))
    (nil (position "a" seq :start 2 :end 3))))

(test-seq-fn position-integer
    ((seq '(1 3 5 6)))

  (is (= 0 (position 2 seq :key #'1+))))

(test-seq-fn position-empty
    ((seq nil))

  (is (= nil (position 'x seq))))

(test-seq-fn position-if
    ((seq '(1 3 4 5 6 7)))

  (is-every =
    (2 (position-if #'evenp seq))
    (4 (position-if #'evenp seq :from-end t))
    (4 (position-if #'evenp seq :start 3))
    (nil (position-if #'evenp seq :start 3 :end 4))
    (0 (position-if #'evenp seq :key #'1+))))

(test-seq-fn position-if-empty
    ((seq nil))

  (is (= nil (position-if #'evenp seq))))

(test-seq-fn position-if-not
    ((seq '(1 3 4 5 6 7)))

  (is-every =
    (0 (position-if-not #'evenp seq))
    (3 (position-if-not #'evenp seq :start 3))
    (nil (position-if-not #'evenp seq :start 2 :end 3))
    (2 (position-if-not #'evenp seq :key #'1+))
    (5 (position-if-not #'evenp '(1 3 4 5 6 7) :from-end t))))

(test-seq-fn position-if-not-empty
    ((seq nil))

  (is (= nil (position-if-not #'evenp seq))))


;;; Test SEARCH

(test-seq-fn search
    ((seq1 '("alex" "bob"))
     (seq2 '("pete" "alex" "bob" "john" "alex" "jack" "alex" "bob" "jack"))
     (seq3 nil))

  (is-every =
    (1 (search seq1 seq2))
    (6 (search seq1 seq2 :from-end t))
    (6 (search seq1 seq2 :start2 2))
    (2 (search seq1 seq2 :start1 1 :start2 2))
    (nil (search seq1 seq2 :end2 2))
    (1 (search seq1 seq2 :end1 1 :end2 2))
    (4 (search seq1 seq2 :start1 0 :start2 3 :end1 1 :end2 5))

    (1 (search '("Alex" "BOB") seq2 :key #'string-upcase))

    ;; Empty Sequences
    (2 (search seq1 seq2 :start1 1 :start2 2 :end1 1))
    (0 (search seq3 seq2))))

(test-seq-fn search-with-key
    ((seq1 '((x 2) (y 3)))
     (seq2 '((a 1) (b 2) (c 3) (d 4))))

  ;; Test :KEY argument
  (is (= 1 (search seq1 seq2 :key #'cadr))))


;;; Test MISMATCH

(test-seq-fn mismatch
    ((seq '("alex" "bob" "john" "jack")))

  (is-every =
    (nil (mismatch seq '("alex" "bob" "john" "jack")))
    (1 (mismatch seq '("alex" "Bob" "john" "Jack")))
    (2 (mismatch seq '("alex" "bob")))
    (2 (mismatch '("alex" "bob") seq))
    (nil (mismatch seq '("alex" "Bob" "john" "Jack") :key #'string-upcase))
    (4 (mismatch seq '("alex" "Bob" "john" "Jack") :from-end t))
    (nil (mismatch seq '("bob" "john" "jack") :start1 1))
    (nil (mismatch seq '("Alex" "Pete" "bob" "john" "jack") :start1 1 :start2 2))
    (2 (mismatch seq '("Alex" "Pete" "bob" "John" "jack") :start1 1 :start2 2))
    (nil (mismatch seq '("Alex" "alex" "bob" "john" "Pete" "jack") :start1 1 :end1 3 :start2 2 :end2 4))

    (0 (mismatch seq seq :start2 1 :end2 1))
    (1 (mismatch seq seq :start1 1 :end1 1))
    (0 (mismatch seq nil))
    (0 (mismatch nil seq))))

(test-seq-fn mismatch-none
    ((seq '("alex" "john" "jack" "bob")))

  (is-every =
    (nil (mismatch seq '("alex" "bob" "john" "jack" "pete") :from-end t :start1 1 :end1 3 :start2 2 :end2 4))
    (1 (mismatch seq '("alex" "bob" "john" "jack" "pete") :from-end t :end1 3 :end2 4))))


;;; Test REVERSE functions

(test-seq-fn reverse
    ((list '(1 2 3 4))
     (res '(4 3 2 1)))

  (test-not-modified
      ((seq list))

    (is (= res (reverse seq)))))

(test-seq-fn reverse-single-element
    ((seq '(1))
     (res '(1)))

  (is (= res (reverse seq))))

(test-seq-fn reverse-empty
    ((seq nil)
     (res nil))

  (is (= res (reverse seq))))

(test reverse-vector
  "Test REVERSE on a vector"

  (is (= #(d c b a) (reverse #(a b c d)))))

(test-seq-fn nreverse
    ((seq (list 1 2 3 4))
     (res (list 4 3 2 1)))

  (is (= res (nreverse seq))))

(test-seq-fn nreverse-single-element
    ((seq (list 1))
     (res (list 1)))

  (is (= res (nreverse seq))))

(test-seq-fn nreverse-empty
    ((seq nil)
     (res nil))

  (is (= res (nreverse seq))))

(test nreverse-vector
  "Test NREVERSE on vector"

  (is (= #(d c b a) (nreverse (vector 'a 'b 'c 'd)))))


;;; Test MERGE functions

(test-seq-fn merge-1
    ((list1 '(1 2 3 4))
     (list2 '(5 6 7 8))
     (res '(1 2 3 4 5 6 7 8)))

  (test-not-modified
      ((seq1 list1)
       (seq2 list2))

    (is (= res (merge seq1 seq2 #'lessp)))))

(test-seq-fn merge-2
    ((list1 '(1 3 5 9))
     (list2 '(2 4 6 7 8))
     (res '(1 2 3 4 5 6 7 8 9)))

  (test-not-modified
      ((seq1 list1)
       (seq2 list2))

    (is (= res (merge seq1 seq2 #'lessp)))))

(test-seq-fn merge-3
    ((seq1 '((a 1) (b 2) (c 5) (d 8)))
     (seq2 '((e 3) (f 4) (g 6) (h 7)))
     (res '((a 1) (b 2) (e 3) (f 4) (c 5) (g 6) (h 7) (d 8))))

  (is (= res (merge seq1 seq2 #'lessp :key #'cadr))))

;; Test Stability

(test-seq-fn merge-stability
    ((seq1 '((b 1) (d 1) (a 99)))
     (seq2 '((e 1) (h 1) (f 32) (c 74)))
     (res '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99))))

  (is (= res (merge seq1 seq2 #'lessp :key #'cadr))))

;; Empty Sequences

(test-seq-fn merge-empty
    ((seq '(1 2 3 4))
     (empty nil))

  (is-every =
    (seq (merge seq nil #'lessp))
    (seq (merge empty seq #'lessp))))

(test-seq-fn nmerge-1
    ((seq1 (list 1 2 3 4))
     (seq2 (list 5 6 7 8))
     (res '(1 2 3 4 5 6 7 8)))

  (is (= res (nmerge seq1 seq2 #'lessp))))

(test-seq-fn nmerge-2
    ((seq1 (list 1 3 5 9))
     (seq2 (list 2 4 6 7 8))
     (res '(1 2 3 4 5 6 7 8 9)))

  (is (= res (nmerge seq1 seq2 #'lessp))))

(test-seq-fn nmerge-3
    ((seq1 (list '(a 1) '(b 2) '(c 5) '(d 8)))
     (seq2 (list '(e 3) '(f 4) '(g 6) '(h 7)))
     (res '((a 1) (b 2) (e 3) (f 4) (c 5) (g 6) (h 7) (d 8))))

  (is (= res (nmerge seq1 seq2 #'lessp :key #'cadr))))

;; Test Stability

(test-seq-fn nmerge-4
    ((seq1 (list '(b 1) '(d 1) '(a 99)))
     (seq2 (list '(e 1) '(h 1) '(f 32) '(c 74)))
     (res '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99))))

  (is (= res (nmerge seq1 seq2 #'lessp :key #'cadr))))

;; Empty Sequences

(test-seq-fn nmerge-5
    ((seq (list 1 2 3 4))
     (empty nil))

  (is-every =
    (seq (nmerge seq nil #'lessp) :test #'equalp)
    (seq (nmerge empty seq #'lessp) :test #'equalp)))


;;; Test SORT functions

(test-seq-fn sort
    ((list '("aac" "zzz" "aaa" "aab" "bac" "baa"))
     (res '("aaa" "aab" "aac" "baa" "bac" "zzz")))

  (test-not-modified
      ((seq list))

    (is (= res (sort seq)))))

(test-seq-fn sort-test
    ((seq '(99 3 74 56 1 32 49))
     (res '(99 74 56 49 32 3 1)))

  (is (= res (sort seq :test #'greaterp))))

(test-seq-fn sort-key
    ((seq '((a 99) (b 3) (c 74) (d 56) (e 1) (f 32) (h 49)))
     (res '((e 1) (b 3) (f 32) (h 49) (d 56) (c 74) (a 99))))

  (is (= res (sort seq :key #'cadr))))

(test-seq-fn sort-empty
    ((seq nil))

  (is (= seq (sort seq))))

;; Other CL Sequences
(test sort-vector
  "Test SORT on a vector"

  (is (= #(99 74 56 49 32 3 1) (sort #(99 3 74 56 1 32 49) :test #'greaterp))))

(test-seq-fn nsort
    ((seq (list "aac" "zzz" "aaa" "aab" "bac" "baa"))
     (res '("aaa" "aab" "aac" "baa" "bac" "zzz")))

  (is (= res (nsort seq))))

(test-seq-fn nsort-test
    ((seq (list 99 3 74 56 1 32 49))
     (res '(99 74 56 49 32 3 1)))

  (is (= res (nsort seq :test #'greaterp))))

(test-seq-fn nsort-key
    ((seq (list '(a 99) '(b 3) '(c 74) '(d 56) '(e 1) '(f 32) '(h 49)))
     (res '((e 1) (b 3) (f 32) (h 49) (d 56) (c 74) (a 99))))

  (is (= res (nsort seq :key #'cadr))))

(test-seq-fn nsort-empty
    ((seq nil))

  (is (= seq (nsort seq))))

(test nsort-vector
  "Test NSORT on a vector"

  (is (= #(99 74 56 49 32 3 1) (nsort (vector 99 3 74 56 1 32 49) :test #'greaterp))))

(test-seq-fn stable-sort
    ((list '("aac" "zzz" "aaa" "aab" "bac" "baa"))
     (res '("aaa" "aab" "aac" "baa" "bac" "zzz")))

  (test-not-modified
      ((seq list))

    (is (= res (stable-sort seq)))))

(test-seq-fn stable-sort-test
    ((seq '(99 3 74 56 1 32 49))
     (res '(99 74 56 49 32 3 1)))

  (is (= res (stable-sort seq :test #'greaterp))))

(test-seq-fn stable-sort-key
    ((seq '((a 99) (b 1) (c 74) (d 1) (e 1) (f 32) (h 1)))
     (res '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99))))

  (is (= res (stable-sort seq :key #'cadr))))

(test-seq-fn stable-sort-empty
    ((seq nil))

  (is (= seq (stable-sort seq))))

(test stable-sort-vector
  "Test STABLE-SORT on a vector"

  (is (= #(99 74 56 49 32 3 1) (stable-sort #(99 3 74 56 1 32 49) :test #'greaterp))))

(test-seq-fn stable-nsort
    ((seq (list "aac" "zzz" "aaa" "aab" "bac" "baa"))
     (res (list "aaa" "aab" "aac" "baa" "bac" "zzz")))

  (is (= res (stable-nsort seq))))

(test-seq-fn stable-nsort-test
    ((seq (list 99 3 74 56 1 32 49))
     (res '(99 74 56 49 32 3 1)))

  (is (= res (stable-nsort seq :test #'greaterp))))

(test-seq-fn stable-nsort-key
    ((seq (list '(a 99) '(b 1) '(c 74) '(d 1) '(e 1) '(f 32) '(h 1)))
     (res '((b 1) (d 1) (e 1) (h 1) (f 32) (c 74) (a 99))))

  (is (= res (stable-nsort seq :key #'cadr))))

(test-seq-fn stable-nsort-empty
    ((seq nil))

  (is (= seq (stable-nsort seq))))

(test stable-nsort-vector
  "Test STABLE-NSORT on a vector"

  (is (= #(99 74 56 49 32 3 1) (stable-nsort (vector 99 3 74 56 1 32 49) :test #'greaterp))))


;;; Test SUBSTITUTE Functions

(test-seq-fn substitute-string
    ((list '("a" "b" "old" "c" "old" "d"))
     (res '("a" "b" "new" "c" "new" "d")))

  (test-not-modified
      ((seq list))

    (is (= res (substitute "new" "old" seq)))))

(test-seq-fn substitute-number
    ((seq '(1 2 3 1 4 5 1))
     (res '(x 2 3 x 4 5 x)))

  (is (= res (substitute 'x 1 seq))))

(test-seq-fn substitute-count
    ((seq '(1 2 3 1 4 5 1))
     (res '(x 2 3 x 4 5 1)))

  (is (= res (substitute 'x 1 seq :count 2))))

(test-seq-fn substitute-from-end
    ((seq '(1 2 3 1 4 5 1))
     (res '(1 2 3 x 4 5 x)))

  (is (= res (substitute 'x 1 seq :count 2 :from-end t))))

(test-seq-fn substitute-start
    ((seq '(1 2 3 1 4 5 1))
     (res '(1 2 3 x 4 5 x)))

  (is (= res (substitute 'x 1 seq :start 1))))

(test-seq-fn substitute-start-count
    ((seq '(1 2 3 1 4 5 1))
     (res '(1 2 3 x 4 5 1)))

  (is (= res (substitute 'x 1 seq :start 1 :count 1))))

(test-seq-fn substitute-start-end
    ((seq '(1 2 3 1 4 5 1))
     (res '(1 2 3 x 4 5 1)))

  (is (= res (substitute 'x 1 seq :start 1 :end 5))))

(test-seq-fn substitute-end
    ((seq '(1 2 3 1 4 5 1))
     (res '(x 2 3 x 4 5 1)))

  (is (= res (substitute 'x 1 seq :end 5))))

(test-seq-fn substitute-end-count
    ((seq '(1 2 3 1 4 5 1))
     (res '(x 2 3 1 4 5 1)))

  (is (= res (substitute 'x 1 seq :end 5 :count 1))))

(test-seq-fn substitute-key
    ((seq '((a 1) (b 2) (c 3) (d 1) (e 4) (f 5) (g 1)))
     (res '(x (b 2) (c 3) x (e 4) (f 5) x)))

  (is (= res (substitute 'x 1 seq :key #'cadr))))

(test-seq-fn substitute-empty
    ((seq nil))

  (is (= seq (substitute 'y 'x seq))))

;;;; NSUBSTITUTE

(test-seq-fn nsubstitute-string
    ((seq (list "a" "b" "old" "c" "old" "d"))
     (res (list "a" "b" "new" "c" "new" "d")))

  (is (= res (nsubstitute "new" "old" seq))))

(test-seq-fn nsubstitute-number
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 'x 2 3 'x 4 5 'x)))

  (is (= res (nsubstitute 'x 1 seq))))

(test-seq-fn nsubstitute-count
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 'x 2 3 'x 4 5 1)))

  (is (= res (nsubstitute 'x 1 seq :count 2))))

(test-seq-fn nsubstitute-from-end
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 1 2 3 'x 4 5 'x)))

  (is (= res (nsubstitute 'x 1 seq :count 2 :from-end t))))

(test-seq-fn nsubstitute-start
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 1 2 3 'x 4 5 'x)))

  (is (= res (nsubstitute 'x 1 seq :start 1))))

(test-seq-fn nsubstitute-start-count
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 1 2 3 'x 4 5 1)))

  (is (= res (nsubstitute 'x 1 seq :start 1 :count 1))))

(test-seq-fn nsubstitute-start-end
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 1 2 3 'x 4 5 1)))

  (is (= res (nsubstitute 'x 1 seq :start 1 :end 5))))

(test-seq-fn nsubstitute-end
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 'x 2 3 'x 4 5 1)))

  (is (= res (nsubstitute 'x 1 seq :end 5))))

(test-seq-fn nsubstitute-end-count
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 'x 2 3 1 4 5 1)))

  (is (= res (nsubstitute 'x 1 seq :end 5 :count 1))))

(test-seq-fn nsubstitute-key
    ((seq (list '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1)))
     (res (list 'x '(b 2) '(c 3) 'x '(e 4) '(f 5) 'x)))

  (is (= res (nsubstitute 'x 1 seq :key #'cadr))))

(test-seq-fn nsubstitute-empty
    ((seq nil))

  (is (= seq (nsubstitute 'y 'x seq))))

;;;; SUBSTITUTE-IF

(test-seq-fn substitute-if
    ((list '(1 2 3 4 5 6 7 8))
     (res '(1 x 3 x 5 x 7 x)))

    (test-not-modified
	((seq list))

      (is (= res (substitute-if 'x #'evenp seq)))))

(test-seq-fn substitute-if-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 x 3 x 5 6 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :count 2))))

(test-seq-fn substitute-if-from-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 4 5 x 7 x)))

  (is (= res (substitute-if 'x #'evenp seq :count 2 :from-end t))))

(test-seq-fn substitute-if-start
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 x 5 x 7 x)))

  (is (= res (substitute-if 'x #'evenp seq :start 2))))

(test-seq-fn substitute-if-start-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 x 5 6 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :start 2 :count 1))))

(test-seq-fn substitute-if-start-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 x 5 x 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :start 2 :end 6))))

(test-seq-fn substitute-if-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 x 3 x 5 6 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :end 5))))

(test-seq-fn substitute-if-end-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 x 3 4 5 6 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :end 5 :count 1))))

(test-seq-fn substitute-if-key
    ((seq '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
     (res '((a 1) x (c 3) x (e 5) x (g 7))))

  (is (= res (substitute-if 'x #'evenp seq :key #'cadr))))

(test-seq-fn substitute-if-empty
    ((seq nil))

  (is (= seq (substitute-if 'y #'evenp seq))))

;;;; NSUBSTITUTE-IF

(test-seq-fn nsubstitute-if
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 'x 3 'x 5 'x 7 'x)))

  (is (= res (substitute-if 'x #'evenp seq))))

(test-seq-fn nsubstitute-if-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 'x 3 'x 5 6 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :count 2))))

(test-seq-fn nsubstitute-if-from-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 4 5 'x 7 'x)))

  (is (= res (substitute-if 'x #'evenp seq :count 2 :from-end t))))

(test-seq-fn nsubstitute-if-start
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 'x 5 'x 7 'x)))

  (is (= res (substitute-if 'x #'evenp seq :start 2))))

(test-seq-fn nsubstitute-if-start-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 'x 5 6 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :start 2 :count 1))))

(test-seq-fn nsubstitute-if-start-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 'x 5 'x 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :start 2 :end 6))))

(test-seq-fn nsubstitute-if-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 'x 3 'x 5 6 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :end 5))))

(test-seq-fn nsubstitute-if-end-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 'x 3 4 5 6 7 8)))

  (is (= res (substitute-if 'x #'evenp seq :end 5 :count 1))))

(test-seq-fn nsubstitute-if-key
    ((seq (list '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7)))
     (res (list '(a 1) 'x '(c 3) 'x '(e 5) 'x '(g 7))))

  (is (= res (substitute-if 'x #'evenp seq :key #'cadr))))

(test-seq-fn nsubstitute-if-empty
    ((seq nil))

  (is (= seq (nsubstitute-if 'y #'evenp seq))))

;;;; SUBSTITUTE-IF-NOT

(test-seq-fn substitute-if-not
    ((list '(1 2 3 4 5 6 7 8))
     (res '(x 2 x 4 x 6 x 8)))

  (test-not-modified
      ((seq list))

    (is (= res (substitute-if-not 'x #'evenp seq)))))

(test-seq-fn substitute-if-not-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(x 2 x 4 5 6 7 8)))

  (is (= res (substitute-if-not 'x #'evenp seq :count 2))))

(test-seq-fn substitute-if-not-from-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 4 x 6 x 8)))

  (is (= res (substitute-if-not 'x #'evenp seq :count 2 :from-end t))))

(test-seq-fn substitute-if-not-start
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 x 4 x 6 x 8)))

  (is (= res (substitute-if-not 'x #'evenp seq :start 2))))

(test-seq-fn substitute-if-not-start-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 x 4 5 6 7 8)))

  (is (= res (substitute-if-not 'x #'evenp seq :start 2 :count 1))))

(test-seq-fn substitute-if-not-start-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 x 4 x 6 7 8)))

  (is (= res (substitute-if-not 'x #'evenp seq :start 2 :end 6))))

(test-seq-fn substitute-if-not-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(x 2 x 4 x 6 7 8)))

  (is (= res (substitute-if-not 'x #'evenp seq :end 5))))

(test-seq-fn substitute-if-not-end-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(x 2 3 4 5 6 7 8)))

  (is (= res (substitute-if-not 'x #'evenp seq :end 5 :count 1))))

(test-seq-fn substitute-if-not-key
    ((seq '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
     (res '(x (b 2) x (d 4) x (f 6) x)))

  (is (= res (substitute-if-not 'x #'evenp seq :key #'cadr))))

(test-seq-fn substitute-if-not-empty
    ((seq nil))

  (is (= seq (substitute-if-not 'y #'evenp seq))))

;;;; SUBSTITUTE-IF-NOT

(test-seq-fn nsubstitute-if-not
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 'x 2 'x 4 'x 6 'x 8)))

  (is (= res (nsubstitute-if-not 'x #'evenp seq))))

(test-seq-fn nsubstitute-if-not-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 'x 2 'x 4 5 6 7 8)))

  (is (= res (nsubstitute-if-not 'x #'evenp seq :count 2))))

(test-seq-fn nsubstitute-if-not-from-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 4 'x 6 'x 8)))

  (is (= res (substitute-if-not 'x #'evenp seq :count 2 :from-end t))))

(test-seq-fn nsubstitute-if-not-start
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 'x 4 'x 6 'x 8)))

  (is (= res (nsubstitute-if-not 'x #'evenp seq :start 2))))

(test-seq-fn nsubstitute-if-not-start-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 'x 4 5 6 7 8)))

  (is (= res (nsubstitute-if-not 'x #'evenp seq :start 2 :count 1))))

(test-seq-fn nsubstitute-if-not-start-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 'x 4 'x 6 7 8)))

  (is (= res (nsubstitute-if-not 'x #'evenp seq :start 2 :end 6))))

(test-seq-fn nsubstitute-if-not-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 'x 2 'x 4 'x 6 7 8)))

  (is (= res (nsubstitute-if-not 'x #'evenp seq :end 5))))

(test-seq-fn nsubstitute-if-not-end-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 'x 2 3 4 5 6 7 8)))

  (is (= res (nsubstitute-if-not 'x #'evenp seq :end 5 :count 1))))

(test-seq-fn nsubstitute-if-not-key
    ((seq (list '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7)))
     (res (list 'x '(b 2) 'x '(d 4) 'x '(f 6) 'x)))

  (is (= res (nsubstitute-if-not 'x #'evenp seq :key #'cadr))))

(test-seq-fn nsubstitute-if-not-empty
    ((seq nil))

  (is (= seq (nsubstitute-if-not 'y #'evenp seq))))


;;; Test REMOVE Functions

(test-seq-fn remove-string
    ((list '("a" "b" "old" "c" "old" "d"))
     (res '("a" "b" "c" "d")))

  (test-not-modified
      ((seq list))

    (is (= res (remove "old" seq)))))

(test-seq-fn remove-integer
    ((seq '(1 2 3 1 4 5 1))
     (res '(2 3 4 5)))

  (is (= res (remove 1 seq))))

(test-seq-fn remove-count
    ((seq '(1 2 3 1 4 5 1))
     (res '(2 3 4 5 1)))

  (is (= res (remove 1 seq :count 2))))

(test-seq-fn remove-from-end
    ((seq '(1 2 3 1 4 5 1))
     (res '(1 2 3 4 5)))

  (is (= res (remove 1 seq :count 2 :from-end t))))

(test-seq-fn remove-start
    ((seq '(1 2 3 1 4 5 1))
     (res '(1 2 3 4 5)))

  (is (= res (remove 1 seq :start 1))))

(test-seq-fn remove-start-count
    ((seq '(1 2 3 1 4 5 1))
     (res '(1 2 3 4 5 1)))

  (is (= res (remove 1 seq :start 1 :count 1))))

(test-seq-fn remove-start-end
    ((seq '(1 2 3 1 4 5 1))
     (res '(1 2 3 4 5 1)))

  (is (= res (remove 1 seq :start 1 :end 5))))

(test-seq-fn remove-end
    ((seq '(1 2 3 1 4 5 1))
     (res '(2 3 4 5 1)))
    (is (remove 1 seq :end 5) res :test #'equalp))

(test-seq-fn remove-end-count
    ((seq '(1 2 3 1 4 5 1))
     (res '(2 3 1 4 5 1)))

  (is (= res (remove 1 seq :end 5 :count 1))))

(test-seq-fn remove-key
    ((seq '((a 1) (b 2) (c 3) (d 1) (e 4) (f 5) (g 1)))
     (res '((b 2) (c 3) (e 4) (f 5))))

  (is (= res (remove 1 seq :key #'cadr))))

(test-seq-fn remove-empty
    ((seq nil))

  (is (= seq (remove 'x seq))))

;;;; DELETE

(test-seq-fn delete-string
    ((seq (list "a" "b" "old" "c" "old" "d"))
     (res (list "a" "b" "c" "d")))

  (is (= res (delete "old" seq))))

(test-seq-fn delete-integer
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 2 3 4 5)))

  (is (= res (delete 1 seq))))

(test-seq-fn delete-count
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 2 3 4 5 1)))

  (is (= res (delete 1 seq :count 2))))

(test-seq-fn delete-from-end
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 1 2 3 4 5)))

  (is (= res (delete 1 seq :count 2 :from-end t))))

(test-seq-fn delete-start
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 1 2 3 4 5)))

  (is (= res (delete 1 seq :start 1))))

(test-seq-fn delete-start-count
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 1 2 3 4 5 1)))

  (is (= res (delete 1 seq :start 1 :count 1))))

(test-seq-fn delete-start-end
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 1 2 3 4 5 1)))

  (is (= res (delete 1 seq :start 1 :end 5))))

(test-seq-fn delete-end
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 2 3 4 5 1)))

  (is (= res (delete 1 seq :end 5))))

(test-seq-fn delete-end-count
    ((seq (list 1 2 3 1 4 5 1))
     (res (list 2 3 1 4 5 1)))

  (is (= res (delete 1 seq :end 5 :count 1))))

(test-seq-fn delete-key
    ((seq (list '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1)))
     (res (list '(b 2) '(c 3) '(e 4) '(f 5))))

  (is (= res (delete 1 seq :key #'cadr))))

(test-seq-fn delete-empty
    ((seq nil))

  (is (= seq (delete 'x seq))))

;;;; REMOVE-IF

(test-seq-fn remove-if
    ((list '(1 2 3 4 5 6 7 8))
     (res '(1 3 5 7)))

  (test-not-modified
      ((seq list))

    (is (= res (remove-if #'evenp seq)))))

(test-seq-fn remove-if-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 3 5 6 7 8)))

  (is (= res (remove-if #'evenp seq :count 2))))

(test-seq-fn remove-if-from-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 4 5 7)))

  (is (= res (remove-if #'evenp seq :count 2 :from-end t))))

(test-seq-fn remove-if-start-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 5 7)))

  (is (= res (remove-if #'evenp seq :start 2))))

(test-seq-fn remove-if-start-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 5 6 7 8)))

  (is (= res (remove-if #'evenp seq :start 2 :count 1))))

(test-seq-fn remove-if-start-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 5 7 8)))

  (is (= res (remove-if #'evenp seq :start 2 :end 6))))

(test-seq-fn remove-if-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 3 5 6 7 8)))

  (is (= res (remove-if #'evenp seq :end 5))))

(test-seq-fn remove-if-end-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 3 4 5 6 7 8)))

  (is (= res (remove-if #'evenp seq :end 5 :count 1))))

(test-seq-fn remove-if-key
    ((seq '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
     (res '((a 1) (c 3) (e 5) (g 7))))

  (is (= res (remove-if #'evenp seq :key #'cadr))))

(test-seq-fn remove-if-empty
    ((seq nil))

  (is (= seq (remove-if #'evenp seq))))

;;;; DELETE-IF

(test-seq-fn delete-if
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 3 5 7)))


  (is (= res (delete-if #'evenp seq))))

(test-seq-fn delete-if-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 3 5 6 7 8)))

  (is (= res (delete-if #'evenp seq :count 2))))

(test-seq-fn delete-if-from-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 4 5 7)))

  (is (= res (delete-if #'evenp seq :count 2 :from-end t))))

(test-seq-fn delete-if-start
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 5 7)))

  (is (= res (delete-if #'evenp seq :start 2))))

(test-seq-fn delete-if-start-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 5 6 7 8)))

  (is (= res (delete-if #'evenp seq :start 2 :count 1))))

(test-seq-fn delete-if-start-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 5 7 8)))

  (is (= res (delete-if #'evenp seq :start 2 :end 6))))

(test-seq-fn delete-if-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 3 5 6 7 8)))

  (is (= res (delete-if #'evenp seq :end 5))))

(test-seq-fn delete-if-end-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 3 4 5 6 7 8)))

  (is (= res (delete-if #'evenp seq :end 5 :count 1))))

(test-seq-fn delete-if-key
    ((seq (list '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7)))
     (res (list '(a 1) '(c 3) '(e 5) '(g 7))))

  (is (= res (delete-if #'evenp seq :key #'cadr))))

(test-seq-fn delete-if-empty
    ((seq nil))

  (is (= seq (delete-if #'evenp seq))))

;;;; REMOVE-IF-NOT

(test-seq-fn remove-if-not
    ((list '(1 2 3 4 5 6 7 8))
     (res '(2 4 6 8)))

  (test-not-modified
      ((seq list))

    (is (= res (remove-if-not #'evenp seq)))))

(test-seq-fn remove-if-not-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(2 4 5 6 7 8)))

  (is (= res (remove-if-not #'evenp seq :count 2))))

(test-seq-fn remove-if-not-from-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 3 4 6 8)))

  (is (= res (remove-if-not #'evenp seq :count 2 :from-end t))))

(test-seq-fn remove-if-not-start
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 4 6 8)))

  (is (= res (remove-if-not #'evenp seq :start 2))))

(test-seq-fn remove-if-not-start-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 4 5 6 7 8)))

  (is (= res (remove-if-not #'evenp seq :start 2 :count 1))))

(test-seq-fn remove-if-not-start-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(1 2 4 6 7 8)))

  (is (= res (remove-if-not #'evenp seq :start 2 :end 6))))

(test-seq-fn remove-if-not-end
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(2 4 6 7 8)))

  (is (= res (remove-if-not #'evenp seq :end 5))))

(test-seq-fn remove-if-not-end-count
    ((seq '(1 2 3 4 5 6 7 8))
     (res '(2 3 4 5 6 7 8)))

  (is (= res (remove-if-not #'evenp seq :end 5 :count 1))))

(test-seq-fn remove-if-not-key
    ((seq '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
     (res '((b 2) (d 4) (f 6))))

  (is (= res (remove-if-not #'evenp seq :key #'cadr))))

(test-seq-fn remove-if-not-empty
    ((seq nil))

  (is (= seq (remove-if-not #'evenp seq))))

;;;; DELETE-IF-NOT

(test-seq-fn delete-if-not
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 2 4 6 8)))

  (is (= res (delete-if-not #'evenp seq))))

(test-seq-fn delete-if-not-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 2 4 5 6 7 8)))

  (is (= res (delete-if-not #'evenp seq :count 2))))

(test-seq-fn delete-if-not-from-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 3 4 6 8)))

  (is (= res (delete-if-not #'evenp seq :count 2 :from-end t))))

(test-seq-fn delete-if-not-start
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 4 6 8)))

  (is (= res (delete-if-not #'evenp seq :start 2))))

(test-seq-fn delete-if-not-start-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 4 5 6 7 8)))

  (is (= res (delete-if-not #'evenp seq :start 2 :count 1))))

(test-seq-fn delete-if-not-start-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 1 2 4 6 7 8)))

  (is (= res (delete-if-not #'evenp seq :start 2 :end 6))))

(test-seq-fn delete-if-not-end
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 2 4 6 7 8)))

  (is (= res (delete-if-not #'evenp seq :end 5))))

(test-seq-fn delete-if-not-end-count
    ((seq (list 1 2 3 4 5 6 7 8))
     (res (list 2 3 4 5 6 7 8)))

  (is (= res (delete-if-not #'evenp seq :end 5 :count 1))))

(test-seq-fn delete-if-not-key
    ((seq (list '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7)))
     (res (list '(b 2) '(d 4) '(f 6))))

  (is (= res (delete-if-not #'evenp seq :key #'cadr))))

(test-seq-fn delete-if-not-empty
    ((seq nil))

  (is (= seq (delete-if-not #'evenp seq))))


;;; Test REMOVE-DUPLICATES Functions

(test-seq-fn remove-duplicates
    ((list '("alex" "john" "bob" "alex" "jack" "john"))
     (res '("bob" "alex" "jack" "john")))

  (test-not-modified
      ((seq list))

    (is (= res (remove-duplicates seq)))))

(test-seq-fn remove-duplicates-from-end
    ((seq '("alex" "john" "bob" "alex" "jack" "john"))
     (res '("alex" "john" "bob" "jack")))

  (is (= res (remove-duplicates seq :from-end t))))

(test-seq-fn remove-duplicates-start
    ((seq '("alex" "john" "bob" "alex" "jack" "john"))
     (res '("alex" "bob" "alex" "jack" "john")))

  (is (= res (remove-duplicates seq :start 1))))

(test-seq-fn remove-duplicates-start-end
    ((seq '("alex" "john" "bob" "alex" "jack" "john"))
     (res '("alex" "john" "bob" "alex" "jack" "john")))

  (is (= res (remove-duplicates seq :start 1 :end 4))))

(test-seq-fn remove-duplicates-key
    ((seq '((a 1) (b 2) (c 1) (d 3) (e 2) (f 4)))
     (res '((c 1) (d 3) (e 2) (f 4))))

  (is (= res (remove-duplicates seq :key #'cadr))))

(test-seq-fn remove-duplicates-test
    ((seq '(#\a #\B #\c #\D #\A #\b #\C #\d))
     (res '(#\a #\B #\c #\D)))

  (is (= res (remove-duplicates seq :from-end t :test #'char-equal))))

(test-seq-fn remove-duplicates-empty
    ((seq nil))

  (is (= seq (remove-duplicates seq))))

;;;; DELETE-DUPLICATES

(test-seq-fn delete-duplicates
    ((seq (list "alex" "john" "bob" "alex" "jack" "john"))
     (res (list "bob" "alex" "jack" "john")))

  (is (= res (delete-duplicates seq))))

(test-seq-fn delete-duplicates-from-end
    ((seq (list "alex" "john" "bob" "alex" "jack" "john"))
     (res (list "alex" "john" "bob" "jack")))

  (is (= res (delete-duplicates seq :from-end t))))

(test-seq-fn delete-duplicates-start
    ((seq (list "alex" "john" "bob" "alex" "jack" "john"))
     (res (list "alex" "bob" "alex" "jack" "john")))

  (is (= res (delete-duplicates seq :start 1))))

(test-seq-fn delete-duplicates-start-end
    ((seq (list "alex" "john" "bob" "alex" "jack" "john"))
     (res (list "alex" "john" "bob" "alex" "jack" "john")))

  (is (= res (delete-duplicates seq :start 1 :end 4))))

(test-seq-fn delete-duplicates-key
    ((seq (list '(a 1) '(b 2) '(c 1) '(d 3) '(e 2) '(f 4)))
     (res (list '(c 1) '(d 3) '(e 2) '(f 4))))

  (is (= res (delete-duplicates seq :key #'cadr))))

(test-seq-fn delete-duplicates-test
    ((seq (list #\a #\B #\c #\D #\A #\b #\C #\d))
     (res (list #\a #\B #\c #\D)))

  (is (= res (delete-duplicates seq :from-end t :test #'char-equal))))

(test-seq-fn delete-duplicates-empty
    ((seq nil))

  (is (= seq (delete-duplicates seq))))


;;; Test EVERY, SOME, NOTEVERY, NOTANY Functions

(test-seq-fn every
    ((seq1 '(1 2 4 5))
     (seq2 '(a b c d))
     (empty nil))

  (is-true (every #'numberp seq1))
  (is-false (every #'evenp seq1))
  (is-true (every #'equalp seq1 seq1))
  (is-false (every #'equalp seq1 seq2))

  ;; Empty Sequences
  (is-true (every #'evenp empty))

  ;; Deliberately use unary function EVENP as it should never
  ;; be called. If it is called an error should be raised and
  ;; the test should be aborted.
  (is-true (every #'evenp seq1 empty)))

(test-seq-fn some
    ((seq1 '(1 2 4 5))
     (seq2 '(a b c d))
     (empty nil))

  (is-false (some #'numberp seq2))
  (is-true (some #'evenp seq1))
  (is-true (some #'equalp seq1 seq1))
  (is-false (some #'equalp seq1 seq2))

  ;; Test Return Value
  (is (= 2 (some (lambda (x) (and (evenp x) x)) seq1)))

  ;; Empty Sequences
  (is-false (some #'evenp empty))

  ;; Deliberately use unary function EVENP as it should never
  ;; be called. If it is called an error should be raised and
  ;; the test should be aborted.
  (is-false (some #'evenp seq1 empty)))

(test-seq-fn notany
    ((seq1 '(1 2 4 5))
     (seq2 '(a b c d))
     (empty nil))

  (is-true (notany #'numberp seq2))
  (is-false (notany #'evenp seq1))
  (is-false (notany #'equalp seq1 seq1))
  (is-true (notany #'equalp seq1 seq2))

  ;; Empty Sequences
  (is-true (notany #'evenp empty))

  ;; Deliberately use unary function EVENP as it should never
  ;; be called. If it is called an error should be raised and
  ;; the test should be aborted.
  (is-true (notany #'evenp seq1 empty)))

(test-seq-fn notevery
    ((seq1 '(1 2 4 5))
     (seq2 '(a 5 c d))
     (empty nil))

  (is-true (notevery #'evenp seq1))
  (is-false (notevery #'numberp seq1))
  (is-false (notevery #'equalp seq1 seq1))
  (is-true (notevery #'equalp seq1 seq2))

  ;; Empty Sequences
  (is-false (notevery #'evenp empty))

  ;; Deliberately use unary function EVENP as it should never
  ;; be called. If it is called an error should be raised and
  ;; the test should be aborted.
  (is-false (notevery #'evenp seq1 empty)))


;;; Test CONCATENATE Functions

(test-seq-fn concatenate
    ((list1 '(1 2 3))
     (list2 '(4 5 6))
     (list3 '(7 8 9))
     (list4 nil)
     (res '(1 2 3 4 5 6 7 8 9)))

  (test-not-modified
      ((seq1 list1)
       (seq2 list2)
       (seq3 list3)
       (empty list4))

    (is (= res (concatenate seq1 empty seq2 seq3 empty)))))

(test concatenate-string
  "Test CONCATENATE on strings"

  (is (= "all together now" (concatenate "all" " " "together" " " "now"))))

(test-seq-fn nconcatenate
    ((seq1 (list 1 2 3))
     (seq2 (list 4 5 6))
     (seq3 (list 7 8 9))
     (empty nil)
     (res '(1 2 3 4 5 6 7 8 9)))

  (is (= res (nconcatenate seq1 empty seq2 seq3 empty))))

(test nconcatenate-string
  "Test NCONCATENATE on strings"

  (is (= "all together now"
	 (nconcatenate (make-array 3 :adjustable t :fill-pointer t :initial-contents '(#\a #\l #\l)) " " "together" " " "now"))))

(test-seq-fn concatenate-to
    ((seq1 (list 1 2 3))
     (seq2 (list 4 5 6))
     (seq3 (list 7 8 9))
     (hello (list #\h #\e #\l #\l #\o #\Space))
     (world (list #\w #\o #\r #\l #\d))
     (empty nil))

  (is (= '(1 2 3 4 5 6 7 8 9) (concatenate-to 'list seq1 empty seq2 seq3 empty)))

  (is (= (list-wrap 1 2 3 4 5 6 7 8 9)
	 (concatenate-to 'list-wrapper seq1 empty seq2 seq3 empty)))

  (is (= #(1 2 3 4 5 6 7 8 9) (concatenate-to 'vector seq1 empty seq2 seq3 empty)))

  (is (= "hello world" (concatenate-to 'string hello world))))

(test concatenate-to-string
  "Test CONCATENATE-TO with string type"

  (is (= "all together now" (concatenate-to 'string "all" " " "together" " " "now"))))


;;; Test Mapping Functions

(test-seq-fn map
    ((list (list 1 2 3 4))
     (empty-list nil)
     (res '(2 3 4 5)))

  (test-not-modified
      ((seq list))

    (is (= res (map #'1+ seq))))

  ;; Test Empty Sequences
  (test-not-modified
      ((seq list)
       (empty empty-list))

    ;; Always use unary 1+ in order to signal a condition (and
    ;; thus fail the tests) if it is called.
    (is (= empty (map #'1+ empty)))
    (is (= empty (map #'1+ seq empty)))
    (is (= empty (map #'1+ empty seq)))))

(test-seq-fn map-multiple
    ((list1 (list 1 2 3 4))
     (list2 (list 2 3 4 5))
     (res '(3 5 7 9)))

  (test-not-modified
      ((seq1 list1)
       (seq2 list2))

    (is (= res (map #'+ seq1 seq2)))))

(test-seq-fn nmap
    ((seq (list 1 2 3 4))
     (empty nil)
     (res '(2 3 4 5)))

  (is (= res (nmap seq #'1+)))

  ;; Test Empty Sequences

  ;; Always use unary 1+ in order to signal a condition (and
  ;; thus fail the tests) if it is called.
  (is (= empty (nmap empty #'1+)))
  (is (= seq (nmap seq #'1+ empty)))
  (is (= empty (nmap empty #'1+ seq))))

(test-seq-fn nmap-multiple
    ((seq1 (list 1 2 3 4))
     (seq2 (list 2 3 4 5))
     (res '(3 5 7 9)))

  (is (= res (nmap seq1 #'+ seq2))))

(test-seq-fn map-into
    ((out (list 1 2))
     (seq1 '(2 3))
     (seq2 '(1 1))
     (empty nil)
     (res '(1 2 3 4)))

  (is (= res (map-into out #'1+ seq1)))
  (is (= res (map-into out #'+ seq1 seq2)))

  ;; Empty Sequences
  (is (= seq1 (map-into empty #'1+ out)))
  (is (= out (map-into out #'1+ empty)))
  (is (= out (map-into out #'+ empty seq1))))

(test-seq-fn map-to
    ((chars  '(#\h #\e #\l #\l #\o))
     (seq '(1 2 3 4))
     (empty nil))

  (let ((str (map-to 'string #'char-upcase chars)))
    (is (typep str 'string))
    (is (= "HELLO" str)))

  (is (= '(2 3 4 5) (map-to 'list #'1+ seq)))
  (is (= #(2 4 6 8) (map-to 'vector #'+ seq seq)))
  (is (= (list-wrap 2 3 4 5) (map-to 'list-wrapper #'1+ seq)))

  ;; Empty Sequences

  ;; Always use unary 1+ in order to signal a condition (and
  ;; thus fail the tests) if it is called.
  (is (= #() (map-to 'vector #'1+ empty)))
  (is (= nil (map-to 'list #'1+ seq empty)))

  (signals error (map-to 'not-a-sequence-type #'1+ seq)))

(def-fixture sequence-functions ()
  (flet ((f (x)
	   (list x (cl:* x 2) (cl:* x 3)))

	 (fv (x)
	   (vector x (cl:* x 2) (cl:* x 3)))

	 (g (x)
	   (when (evenp x)
	     (list x (cl:* x 2) (cl:* x 3))))

	 (gv (x)
	   (when (evenp x)
	     (vector x (cl:* x 2) (cl:* x 3)))))

    (&body)))

(test-seq-fn (map-extend :fixture sequence-functions)
    ((list (list 1 2 3 4))
     (empty-list nil)
     (res '(1 2 3 2 4 6 3 6 9 4 8 12)))

  (test-not-modified
      ((seq list))

    (is (= res (map-extend #'f seq))))

  (test-not-modified
      ((seq list))

    (is (= res (map-extend #'fv seq))))

  (test-not-modified
      ((seq list)
       (empty empty-list))

    (is (= empty (map-extend #'f empty)))))

(test-seq-fn (map-extend-empty :fixture sequence-functions)
    ((list (list 1 2 3 4))
     (empty-list nil)
     (res '(2 4 6 4 8 12)))

  (test-not-modified
      ((seq list))

    (is (= res (map-extend #'g seq))))

  (test-not-modified
      ((seq list))

    (is (= res (map-extend #'gv seq)))))

(test-seq-fn (map-extend-multiple-arguments :fixture sequence-functions)
    ((list1 (list 1 2 3 4))
     (list2 (list 'a 'b 'c 'd))
     (empty-list nil)
     (res '(1 a 2 b 3 c 4 d)))

  (test-not-modified
      ((seq1 list1)
       (seq2 list2)
       (empty empty-list))

    (is (= res (map-extend #'list seq1 seq2)))
    (is (= res (map-extend #'vector seq1 seq2)))
    (is (= empty (map-extend #'list empty seq1)))))

(def-fixture sequence-functions-2 ()
  (flet ((f (x)
	   (list x (cl:* x 2)))

	 (fv (x)
	   (vector x (cl:* x 2)))

	 (g (x)
	   (when (evenp x)
	     (list x (cl:* x 2))))

	 (gv (x)
	   (when (evenp x)
	     (vector x (cl:* x 2)))))

    (&body)))

(test-seq-fn (map-extend-into :fixture sequence-functions-2)
    ((out (list 'a 'b))
     (seq1 '(2 3))
     (seq2 '(1 1))
     (empty nil)
     (res1 '(a b 2 4 3 6))
     (res2 '(a b 2 1 3 1))
     (res3 '(2 4 3 6)))

  (is (= res1 (map-extend-into out #'f seq1)))
  (is (= res1 (map-extend-into out #'fv seq1)))

  (is (= res2 (map-extend-into out #'list seq1 seq2)))
  (is (= res2 (map-extend-into out #'vector seq1 seq2)))

  (is (= res3 (map-extend-into empty #'f seq1)))
  (is (= out (map-extend-into out #'f empty)))
  (is (= out (map-extend-into out #'list empty seq1))))

;; (flet ((f (x)
;; 	 (list x (cl:* x 2) (cl:* x 3)))

;;        (fv (x)
;; 	 (vector x (cl:* x 2) (cl:* x 3)))

;;        (g (x)
;; 	 (when (evenp x)
;; 	   (list x (cl:* x 2) (cl:* x 3))))

;;        (gv (x)
;; 	 (when (evenp x)
;; 	   (vector x (cl:* x 2) (cl:* x 3))))))

(test-seq-fn (map-extend-to :fixture sequence-functions)
    ((list (list 1 2 3 4))
     (empty-list nil))

  (test-not-modified
      ((seq list))

    (is (= '(1 2 3 2 4 6 3 6 9 4 8 12) (map-extend-to 'list #'f seq)))
    (is (= #(1 2 3 2 4 6 3 6 9 4 8 12) (map-extend-to 'vector #'f seq))))

  (test-not-modified
      ((seq list))

    (is (= '(1 2 3 2 4 6 3 6 9 4 8 12) (map-extend-to 'list #'fv seq)))
    (is (= #(1 2 3 2 4 6 3 6 9 4 8 12) (map-extend-to 'vector #'fv seq))))

  (test-not-modified
      ((seq list)
       (empty empty-list))

    (is (= nil (map-extend-to 'list #'f empty)))
    (is (= #() (map-extend-to 'vector #'f empty)))))

(test-seq-fn (map-extend-to :fixture sequence-functions)
    ((list (list 1 2 3 4))
     (empty-list nil))

  (test-not-modified
      ((seq list))

    (is (= '(2 4 6 4 8 12) (map-extend-to 'list #'g seq)))
    (is (= #(2 4 6 4 8 12) (map-extend-to 'vector #'g seq))))

  (test-not-modified
      ((seq list))

    (is (= '(2 4 6 4 8 12) (map-extend-to 'list #'gv seq)))
    (is (= #(2 4 6 4 8 12) (map-extend-to 'vector #'gv seq)))))

(test-seq-fn (map-extend-to :fixture sequence-functions)
    ((list1 (list 1 2 3 4))
     (list2 (list 'a 'b 'c 'd))
     (empty-list nil))

  (test-not-modified
      ((seq1 list1)
       (seq2 list2)
       (empty empty-list))

    (is (= '(1 a 2 b 3 c 4 d) (map-extend-to 'list #'list seq1 seq2)))
    (is (= #(1 a 2 b 3 c 4 d) (map-extend-to 'vector #'list seq1 seq2)))

    (is (= '(1 a 2 b 3 c 4 d) (map-extend-to 'list #'vector seq1 seq2)))
    (is (= #(1 a 2 b 3 c 4 d) (map-extend-to 'vector #'vector seq1 seq2)))

    (is (= nil (map-extend-to 'list #'list empty seq1)))
    (is (= #() (map-extend-to 'vector #'list empty seq1)))))


;;; Test FOREACH

(test-seq-fn foreach
    ((seq '(1 2 3 4))
     (empty nil))

  (let ((collector (make-collector (list-wrap))))
    (foreach
     (lambda (x)
       (accumulate collector x))
     seq)

    (is (= (list-wrap 1 2 3 4) (collector-sequence collector))))

  (let ((collector (make-collector (list-wrap))))
    (foreach
     (lambda (x y)
       (accumulate collector (+ x y)))
     seq seq)

    (is (= (list-wrap 2 4 6 8) (collector-sequence collector))))

  ;; Empty Sequences
  (block test-empty
    (flet ((fail-test (&rest args)
	     (fail "Unary Function Called by FOREACH with:~%~%~s" args)
	     (return-from test-empty nil)))

      (foreach #'fail-test empty)
      (foreach #'fail-test seq empty)

      (pass "FOREACH on empty sequences"))))
