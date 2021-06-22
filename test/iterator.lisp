;;;; iterator.lisp
;;;;
;;;; Copyright 2018-2021 Alexander Gutev
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

;;;; Unit tests for iterator interface

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite iterator
    :description "Test iterator interface"
    :in generic-cl)

(in-suite iterator)


;;; Test Utilities

(defmacro is-iter-elements ((iter &key step length) &body elements)
  "Test that an iterator produces the expected list of elements.

   ITER is the iterator creation form.

   STEP is an integer specifying the step value to pass to calls to
   ADVANCE-N. If NIL (the default) a step of 1 is assumed and ADVANCE
   is used instead.

   LENGTH is the expected return value of (LENGTH ITER). If NIL the
   length of ELEMENTS is used.

   ELEMENTS is the list of elements which the iterator is expected to
   produce in order. The elements are treated as quoted values (not
   evaluated)."

  (let ((length (or length (cl:length elements))))
    (with-gensyms (it got-length at)
      `(let* ((,it ,iter)
	      (,got-length (length ,it)))
	 (is (= ,length ,got-length)
	     "~%With step: ~a~%~%(LENGTH ~s)~%~%~Tevaluated to: ~a~%~%~Texpected: ~a"
	     ,step ',iter ,got-length ,length)

	 ,@(loop
	      for exp in elements
	      for i = 0 then (1+ i)
	      collect
		`(let ((,at (at ,it)))
		   (is (= ',exp ,at)
		      "~%With step: ~a~%~%~a: (AT ~s)~%~%~Tevaluated to:~%~%~a~%~%~Texpected:~%~%~a"
		      ,step ,i ',iter ,at ',exp))

	      collect (if step `(advance-n ,it ,step) `(advance ,it)))

	 (is-true (endp ,it)
		  "~%With step: ~a~%~%(ENDP ~s) not true"
		  ,step ',iter)))))

(defmacro is-hash-iter-elements ((iter &key step length) &body expected)
  "Test that a hash-table iterator produces elements from an expected set of elements.

   ITER is the iterator creation form.

   STEP is an integer specifying the step value to pass to calls to
   ADVANCE-N. If NIL (the default) a step of 1 is assumed and ADVANCE
   is used instead.

   LENGTH is the expected return value of (LENGTH ITER). If NIL the
   length of ELEMENTS is used.

   EXPECTED is the set of expected elements, treated as quoted
   values (not evaluated). The iterator is tested that at each step it
   produces an element from this set, however not in any particular
   order. It is also checked that the iterator does not return the
   same element more than once."

  (let ((length (or length (cl:length expected))))
    (with-gensyms (it got-length at exp-alist got-alist)
      `(let* ((,it ,iter)
	      (,got-length (length ,it)))
	 (is (= ,length ,got-length)
	     "~%With step: ~a~%~%(LENGTH ~s)~%~%~Tevaluated to: ~a~%~%~Texpected: ~a"
	     ,step ',iter ,got-length ,length)

	 (let ((,exp-alist ',expected)
	       (,got-alist nil))
	   (declare (ignorable ,exp-alist ,got-alist))

	   ,@(loop
		for i from 0 below (if step (ceiling length step) length)
		collect
		  `(let ((,at (at ,it)))
		     (is-true (member ,at ,exp-alist :test #'equalp)
			      "~%With step: ~a~%~%(AT ~s)~%~%~Tevaluated to: ~a~%~%~Twhich is not a member of:~%~%~a"
			      ,step ',iter ,at ,exp-alist)

		     (is-false (member ,at ,got-alist :test #'equalp)
			       "~%With step: ~a~%~%(AT ~s)~%~%~Treturned duplicated value: ~a"
			       ,step ',iter ,at)

		     (push ,at ,got-alist)

		     ,(if step `(advance-n ,it ,step) `(advance ,it)))))

	 (is-true (endp ,it)
		  "~%With step: ~a~%~%(ENDP ~s) not true"
		  ,step ',iter)))))

(defmacro test-set-element ((seq &rest args) (index value) &body expected)
  "Test modifying a sequence element by (SETF AT).

   SEQ is the sequence creation form. This must be a mutable
   sequence.

   ARGS is the list of remaining arguments to pass to the ITERATOR
   function.

   INDEX is the index of the element to modify. (SETF AT) is called
   after ADVANCE is called INDEX times.

   VALUE is the value to which to set the sequence element, at INDEX.

   EXPECTED is the expected sequence elements, after modifying the
   sequence using (SETF AT), which are tested with IS-ITER-ELEMENTS."

  (with-gensyms (s)
    `(let ((,s ,seq))
       (set-iter-element (iterator ,s ,@args) ,index ',value)
       (is-iter-elements ((iterator ,s)) ,@expected))))

(defun set-iter-element (it index value)
  "Set a sequence element by its iterator.

   IT is the sequence iterator.

   INDEX is the index of the element to set.

   VALUE is the value to which to set the element."

  (loop
     for i below index
     do
       (advance it)
     finally (setf (at it) value)))


;;; List Iterator Tests

(test list-unbounded-iterator
  "Test unbounded list iterator"

  (is-iter-elements ((iterator '(1 2 3 a b c)))
    1 2 3 a b c)

  (is-iter-elements ((iterator '(1 2 3 a b c t) :from-end t))
    t c b a 3 2 1)

  (is-iter-elements ((iterator '(1 2 3 a b c)) :step 2 :length 6)
    1 3 b)

  (is-iter-elements ((iterator '(1 2 3 a b c) :from-end t) :step 2 :length 6)
    c a 2))

(test list-bounded-iterator
  "Test bounded list iterator"

  (is-iter-elements ((iterator '(1 2 3 a b c) :start 2))
    3 a b c)

  (is-iter-elements ((iterator '(1 2 3 a b c) :start 2 :end 4))
    3 a)

  (is-iter-elements ((iterator '(1 2 3 a b c) :start 2 :from-end t))
    c b a 3)

  (is-iter-elements ((iterator '(1 2 3 a b c) :start 2 :end 4 :from-end t))
    a 3)

  (is-iter-elements ((iterator '(1 2 3 a b c) :start 2) :step 3 :length 4)
    3 c)

  (is-iter-elements ((iterator '(1 2 3 a b c) :start 2 :end 4) :step 2 :length 2)
    3)

  (is-iter-elements ((iterator '(1 2 3 a b c) :start 2 :from-end t) :step 2 :length 4)
    c a)

  (is-iter-elements ((iterator '(1 2 3 a b c) :start 2 :end 4 :from-end t) :step 3 :length 2)
    a))

(test list-subseq-iterator
  "Test SUBSEQ on list iterator"

  (is-iter-elements ((subseq (iterator '(1 2 3 a b c)) 2))
    3 a b c)

  (is-iter-elements ((subseq (iterator '(1 2 3 a b c)) 2 nil))
    3 a b c)

  (is-iter-elements ((subseq (iterator '(1 2 3 a b c)) 0 4))
    1 2 3 a)

  (is-iter-elements ((subseq (iterator '(1 2 3 a b c) :from-end t) 2))
    a 3 2 1)

  (is-iter-elements ((subseq (iterator '(1 2 3 a b c) :from-end t) 2 nil))
    a 3 2 1)

  (is-iter-elements ((subseq (iterator '(1 2 3 a b c) :from-end t) 2 4))
    a 3))

(test list-iterator-single-element
  "Test list iterator on single element list"

  (is-iter-elements ((iterator '(a)))
    a)

  (is-iter-elements ((iterator '(a) :from-end t))
    a)

  (is-iter-elements ((iterator '(a) :start 1)))

  (is-iter-elements ((iterator '(a) :start 1 :from-end t)))

  (is-iter-elements ((iterator '(a) :start 1 :end 1)))

  (is-iter-elements ((iterator '(a) :start 1 :end 1 :from-end t)))

  (is-iter-elements ((iterator '(a)))
    a)

  (is-iter-elements ((iterator '(a) :from-end t) :step 2)
    a)

  (is-iter-elements ((iterator '(a) :start 1) :step 3))

  (is-iter-elements ((iterator '(a) :start 1 :from-end t) :step 4))

  (is-iter-elements ((iterator '(a) :start 1 :end 1) :step 2))

  (is-iter-elements ((iterator '(a) :start 1 :end 1 :from-end t) :step 2))

  ;; Empty List

  (is-iter-elements ((iterator nil))))

(test list-iterator-mutate
  "Test modifying elements using list iterator"

  (test-set-element ((list 1 2 3 4)) (2 x)
    1 2 x 4)

  (test-set-element ((list 1 2 3 4) :from-end t) (2 x)
    1 x 3 4)

  (test-set-element ((list 1 2 3 4) :start 1) (1 y)
    1 2 y 4)

  (test-set-element ((list 1 2 3 4) :start 1 :from-end t) (2 y)
    1 y 3 4)

  (test-set-element ((list 1 2 3 4) :start 1 :end 3) (1 z)
    1 2 z 4)

  (test-set-element ((list 1 2 3 4) :start 1 :end 3 :from-end t) (1 z)
    1 z 3 4))


;;; Vector Iterator Tests

(test vector-iterator
  "Test vector iterator"

  ;; Single-Step

  (is-iter-elements ((iterator #(1 2 3 a b c)))
    1 2 3 a b c)

  (is-iter-elements ((iterator #(1 2 3 a b c) :from-end t))
    c b a 3 2 1)

  (is-iter-elements ((iterator #(1 2 3 a b c) :start 1 :end 3))
    2 3)

  (is-iter-elements ((iterator #(1 2 3 a b c) :from-end t :start 1 :end 3))
    3 2)

  ;; With Step

  (is-iter-elements ((iterator #(1 2 3 a b c)) :step 3 :length 6)
    1 a)

  (is-iter-elements ((iterator #(1 2 3 a b c) :from-end t) :step 3 :length 6)
    c 3)

  (is-iter-elements ((iterator #(1 2 3 a b c) :start 1 :end 3) :step 4 :length 2)
    2)

  (is-iter-elements ((iterator #(1 2 3 a b c) :from-end t :start 1 :end 3) :step 2 :length 2)
    3))

(test vector-subseq-iterator
  "Test SUBSEQ on vector iterator"

  (is-iter-elements ((subseq (iterator #(1 2 3 a b c)) 1 3))
    2 3)

  (is-iter-elements ((subseq (iterator #(1 2 3 a b c) :from-end t) 1 3))
    b a)

  (is-iter-elements ((subseq (iterator #(1 2 3 a b c)) 1))
    2 3 a b c)

  (is-iter-elements ((subseq (iterator #(1 2 3 a b c)) 1 nil))
    2 3 a b c)

  (is-iter-elements ((subseq (iterator #(1 2 3 a b c) :from-end t) 1))
    b a 3 2 1)

  (is-iter-elements ((subseq (iterator #(1 2 3 a b c) :from-end t) 1 nil))
    b a 3 2 1))

(test vector-iterator-single-element
  "Test vector iterator on single element vector"

  (is-iter-elements ((iterator #(1)))
    1)

  (is-iter-elements ((iterator #(1) :from-end t))
    1)

  (is-iter-elements ((iterator #(1) :start 1)))

  (is-iter-elements ((iterator #(1) :start 1 :end 1)))

  (is-iter-elements ((iterator #(1) :start 1 :end 1 :from-end t)))

  ;; With-Step

  (is-iter-elements ((iterator #(1)) :step 3)
    1)

  (is-iter-elements ((iterator #(1) :from-end t) :step 2)
    1)

  (is-iter-elements ((iterator #(1) :start 1) :step 4))

  (is-iter-elements ((iterator #(1) :start 1 :end 1) :step 5))

  (is-iter-elements ((iterator #(1) :start 1 :end 1 :from-end t) :step 2))

  ;; Empty Vector

  (is-iter-elements ((iterator #())))
  (is-iter-elements ((iterator #()) :step 3)))

(test other-vector-iterator
  "Test vector iterator on other vector types"

  (is-iter-elements ((iterator (vector 1 2 3 4 5 6)))
    1 2 3 4 5 6)

  (is-iter-elements ((iterator (make-array 4 :initial-contents '(a b c d))))
    a b c d)

  (is-iter-elements ((iterator (make-array 4 :adjustable t :fill-pointer t :initial-contents '(a b c d))))
    a b c d)

  (is-iter-elements ((iterator (make-array 4 :element-type 'integer :adjustable t :fill-pointer t :initial-contents '(1 2 3 4))))
    1 2 3 4)

  (is-iter-elements ((iterator "Hello World"))
    #\H #\e #\l #\l #\o #\Space #\W #\o #\r #\l #\d)

  (is-iter-elements ((iterator #*10111011))
    1 0 1 1 1 0 1 1))

(test vector-iterator-mutate
  "Test modifying elements using vector iterator"

  (test-set-element (#(1 2 3 4)) (2 x)
    1 2 x 4)

  (test-set-element (#(1 2 3 4) :from-end t) (2 x)
    1 x 3 4)

  (test-set-element (#(1 2 3 4) :start 1) (1 y)
    1 2 y 4)

  (test-set-element (#(1 2 3 4) :start 1 :from-end t) (2 y)
    1 y 3 4)

  (test-set-element (#(1 2 3 4) :start 1 :end 3) (1 z)
    1 2 z 4)

  (test-set-element (#(1 2 3 4) :start 1 :end 3 :from-end t) (1 z)
    1 z 3 4))


;;; Multi-Dimensional Array Iterator Tests

(test array-iterator
  "Test multi-dimensional array iterator"

  ;; Single-Step

  (is-iter-elements ((iterator #2A((1 2 3) (4 5 6))))
    1 2 3 4 5 6)

  (is-iter-elements ((iterator #2A((1 2 3) (4 5 6)) :from-end t))
    6 5 4 3 2 1)

  (is-iter-elements ((iterator #2A((1 2 3) (4 5 6)) :start 2 :end 5))
    3 4 5)

  (is-iter-elements ((iterator #2A((1 2 3) (4 5 6)) :start 2 :end 5 :from-end t))
    5 4 3)

   ;; With Step

  (is-iter-elements ((iterator #2A((1 2 3) (4 5 6))) :step 3 :length 6)
    1 4)

  (is-iter-elements ((iterator #2A((1 2 3) (4 5 6)) :from-end t) :step 4 :length 6)
    6 2)

  (is-iter-elements ((iterator #2A((1 2 3) (4 5 6)) :start 2 :end 5) :step 2 :length 3)
    3 5)

  (is-iter-elements ((iterator #2A((1 2 3) (4 5 6)) :start 2 :end 5 :from-end t) :step 3 :length 3)
    5))

(test array-subseq-iterator
  "Test SUBSEQ on multi-dimensional array iterator"

  ;; Subseq

  (is-iter-elements ((subseq (iterator #2A((1 2 3) (4 5 6))) 2 5))
    3 4 5)

  (is-iter-elements ((subseq (iterator #2A((1 2 3) (4 5 6)) :from-end t) 2 5))
    4 3 2)

  (is-iter-elements ((subseq (iterator #2A((1 2 3) (4 5 6))) 2))
    3 4 5 6)

  (is-iter-elements ((subseq (iterator #2A((1 2 3) (4 5 6)) :from-end t) 2))
    4 3 2 1)

  (is-iter-elements ((subseq (iterator #2A((1 2 3) (4 5 6))) 2 nil))
    3 4 5 6)

  (is-iter-elements ((subseq (iterator #2A((1 2 3) (4 5 6)) :from-end t) 2 nil))
    4 3 2 1))

(test array-iterator-mutate
  "Test modifying elements using multi-dimensional array iterator"

  (test-set-element (#2A((1 2) (3 4))) (1 x)
    1 x 3 4)

  (test-set-element (#2A((1 2) (3 4)) :from-end t) (1 x)
    1 2 x 4)

  (test-set-element (#2A((1 2) (3 4)) :start 1) (2 x)
    1 2 3 x)

  (test-set-element (#2A((1 2) (3 4)) :start 1 :from-end t) (2 x)
    1 x 3 4)

  (test-set-element (#2A((1 2) (3 4)) :start 1 :end 3) (1 x)
    1 2 x 4)

  (test-set-element (#2A((1 2) (3 4)) :start 1 :end 3 :from-end t) (1 x)
    1 x 3 4))


;;; Test Hash-Table Iterators

(test hash-table-iterator
  "Test hash-table iterator"

  ;; Single-Step

  (is-hash-iter-elements ((iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4)))))
    (a . 1) (b . 2) (c . 3) (d . 4))

  (is-hash-iter-elements ((iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :from-end t))
    (a . 1) (b . 2) (c . 3) (d . 4))

  (is-hash-iter-elements ((iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :start 1 :end 3) :length 2)
    (a . 1) (b . 2) (c . 3) (d . 4))

  (is-hash-iter-elements ((iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :start 1 :end 3 :from-end t) :length 2)
    (a . 1) (b . 2) (c . 3) (d . 4))

  ;; Multi-Step

  (is-hash-iter-elements ((iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4)))) :step 2)
    (a . 1) (b . 2) (c . 3) (d . 4))

  (is-hash-iter-elements ((iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :from-end t) :step 3)
    (a . 1) (b . 2) (c . 3) (d . 4))

  (is-hash-iter-elements ((iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :start 1 :end 3) :step 4 :length 2)
    (a . 1) (b . 2) (c . 3) (d . 4))

  (is-hash-iter-elements ((iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :start 1 :end 3 :from-end t) :step 2 :length 2)
    (a . 1) (b . 2) (c . 3) (d . 4))

  ;; Empty Hash-Table

  (is-hash-iter-elements ((iterator (make-hash-map))))
  (is-hash-iter-elements ((iterator (make-hash-map) :from-end t)))
  (is-hash-iter-elements ((iterator (make-hash-map)) :step 2)))

(test hash-table-subseq-iterator
  "Test SUBSEQ on hash-table iterator"

  (is-hash-iter-elements ((subseq (iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4)))) 1 3) :length 2)
    (a . 1) (b . 2) (c . 3) (d . 4))

  (is-hash-iter-elements ((subseq (iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4)))) 2) :length 2)
    (a . 1) (b . 2) (c . 3) (d . 4))

  (is-hash-iter-elements ((subseq (iterator (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4)))) 1 nil) :length 3)
    (a . 1) (b . 2) (c . 3) (d . 4)))

(test hash-table-iterator-mutate
  "Test modifying elements using hash-table iterator"

  (let* ((hash (alist-hash-map '((a . 1) (b . 2) (c . 3))))
	 (it (iterator hash)))
    (advance it)
    (setf (at it) 'x)
    (is (= 'x (get (car (at it)) hash))))

  (let* ((hash (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))))
	 (it (iterator hash :start 1 :end 3)))

    (advance it)
    (setf (at it) 'x)
    (is (= 'x (get (car (at it)) hash)))))


;;; DOSEQ Macro Tests

;;;; Lists

(test doseq-list-unbounded
  "Test DOSEQ macro on list (unbounded)"

  (let (result)
    (doseq (elem '(1 2 3 4))
      (push elem result))

    (is (= '(1 2 3 4) (cl:nreverse result)))))

(test doseq-list-destructure
  "Test DOSEQ with destructuring on list"

  (let (result)
    (doseq ((x y) '((1 2) (3 4) (5 6) (7 8)))
      (push (cl:+ x y) result))

    (is (= '(3 7 11 15) (cl:nreverse result)))))

(test doseq-list-reverse
  "Test DOSEQ macro on list with :FROM-END t"

  (let* ((list '(1 2 3 4))
         (result nil))

    (doseq (elem (the list list) :from-end t)
      (push elem result))

    (is (= '(4 3 2 1) (cl:nreverse result)))))

(test doseq-list-bounded
  "Test DOSEQ macro on list with :START 1 and :END 4"

  (let* (result)
    (doseq (elem '(1 2 3 4 5) :start 1 :end 4)
      (push elem result))

    (is (= '(2 3 4) (cl:nreverse result)))))

(test doseq-list-reverse-bounded
  "Test DOSEQ macro on list with :START 1, :END 4, :FROM-END T"

  (let* (result)
    (doseq (elem '(1 2 3 4 5) :start 1 :end 4 :from-end t)
      (push elem result))

    (is (= '(4 3 2) (cl:nreverse result)))))


;;;; Vectors

(test doseq-vector-unbounded
  "Test DOSEQ macro on vector (unbounded)"

  (let (result)
    (doseq (elem #(1 2 3 4))
      (push elem result))

    (is (= '(1 2 3 4) (cl:nreverse result)))))

(test doseq-vector-destructure
  "Test DOSEQ with destructuring on vector"

  (let (result)
    (doseq ((x y) #((1 2) (3 4) (5 6) (7 8)))
      (push (cl:+ x y) result))

    (is (= '(3 7 11 15) (cl:nreverse result)))))

(test doseq-vector-reverse
  "Test DOSEQ macro on vector with :FROM-END t"

  (let* ((vec #(1 2 3 4))
         (result nil))

    (doseq (elem (the vector vec) :from-end t)
      (push elem result))

    (is (= '(4 3 2 1) (cl:nreverse result)))))

(test doseq-vector-bounded
  "Test DOSEQ macro on vector with :START 1 and :END 4"

  (let* (result)
    (doseq (elem #(1 2 3 4 5) :start 1 :end 4)
      (push elem result))

    (is (= '(2 3 4) (cl:nreverse result)))))

(test doseq-vector-reverse-bounded
  "Test DOSEQ macro on vector with :START 1, :END 4, :FROM-END T"

  (let* (result)
    (doseq (elem #(1 2 3 4 5) :start 1 :end 4 :from-end t)
      (push elem result))

    (is (= '(4 3 2) (cl:nreverse result)))))


;;;; Hash Tables

(test doseq-hash-table
  "Test DOSEQ macro on hash-table"

  (let* ((map (alist-hash-map '((a . 1) (b . 2) (c . 3))))
	 (new-map (make-hash-map)))

    (doseq ((key . value) (the hash-map map))
      (setf (get key new-map) value))

    (is (= map (alist-hash-map '((a . 1) (b . 2) (c . 3)))))))


;;;; Iterator Based Implementation

(test doseq-generic
  "Test DOSEQ macro on untyped sequence"

  (let* ((seq '(1 2 3 4 5 6))
         (result nil))

    (doseq (elem seq)
      (push elem result))

    (is (= '(1 2 3 4 5 6) (cl:nreverse result)))))

(test doseq-generic-destructure
  "Test DOSEQ with destructuring on untyped sequence"

  (let ((seq '((1 2) (3 4) (5 6) (7 8)))
        (result nil))
    (doseq ((x y) seq)
      (push (cl:+ x y) result))

    (is (= '(3 7 11 15) (cl:nreverse result)))))

(test doseq-generic-reverse
  "Test DOSEQ macro on untyped sequence with :FROM-END t"

  (let* ((list '(1 2 3 4))
         (result nil))

    (doseq (elem list :from-end t)
      (push elem result))

    (is (= '(4 3 2 1) (cl:nreverse result)))))

(test doseq-generic-bounded
  "Test DOSEQ macro on untyped sequence with :START 1 and :END 4"

  (let* ((list '(1 2 3 4 5))
         result)

    (doseq (elem list :start 1 :end 4)
      (push elem result))

    (is (= '(2 3 4) (cl:nreverse result)))))

(test doseq-generic-reverse-bounded
  "Test DOSEQ macro on untyped sequence with :START 1, :END 4, :FROM-END T"

  (let* ((seq '(1 2 3 4 5))
         result)

    (doseq (elem seq :start 1 :end 4 :from-end t)
      (push elem result))

    (is (= '(4 3 2) (cl:nreverse result)))))


;;;; DO-SEQUENCES macro

(test do-sequences
  "Test DO-SEQUENCES on multiple sequences"

  (let ((res1 nil)
        (res2 nil)
        (res3 (make-hash-map)))

   (do-sequences ((x (list 1 2 3))
                  (y #(a b c d e) :start 1)
                  ((zkey . zval) (alist-hash-map '((k1 . 1) (k2 . 2) (k3 . 3)))))

     (push x res1)
     (push y res2)
     (setf (get zkey res3) zval))

   (is (= '(1 2 3) (cl:nreverse res1)))
   (is (= '(b c d) (cl:nreverse res2)))
   (is (= (alist-hash-map '((k1 . 1) (k2 . 2) (k3 . 3)))
          res3))))
