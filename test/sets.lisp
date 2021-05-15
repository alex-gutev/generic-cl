;;;; sets.lisp
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

;;;; Unit tests for generic set functions

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite sets
    :description "Test set data structure interface"
    :in generic-cl)

(in-suite sets)


;;; Test Utilities

(defmacro test-set-fn (name (&rest sets) &body tests)
  "Tests a function on a LIST and `HASH-SET' simultaneously.
   Each element of SETS is of the form (SYM LIST) where SYM is a
   symbol and list is a LIST (evaluated). The forms in TESTS are
   evaluated twice, first with each SYM bound to the corresponding
   LIST, then with each SYM bound to a `HASH-SET' with the contents of
   the corresponding LIST. Both bindings are established by
   SYMBOL-MACROLET."

  (destructuring-bind (name &rest args) (ensure-list name)
    `(progn
       (test ,(alet (symbolicate 'list- name)
		(if args `(,it ,@args) it))

	 ,(format nil "Test ~a on lists as sets" name)

	 (symbol-macrolet
	     ,(loop for (var . list) in sets
		 collect `(,var (list ,@list)))

	   (macrolet ((is-set (got expected &rest args)
			`(is (set-equal ,expected ,got :test #'equalp) ,@args)))
	     ,@tests)))

       (test ,(alet (symbolicate 'hash-set- name)
		(if args `(,it ,@args) it))

	 ,(format nil "Test ~a on hash-sets" name)

	 (symbol-macrolet
	     ,(loop for (var . list) in sets
		 collect `(,var (hash-set ,@list)))

	   (macrolet ((is-set (got expected &rest args)
			`(is (= ,expected ,got) ,@args)))
	     ,@tests))))))

(defmacro test-set-not-modified ((&rest sets) &body tests)
  "Tests that a set is not modified after the evaluation of
   TESTS. Each element of SETS is of the form (SYM SET) where SYM is
   the symbol to which the result of the evaluation of SET is
   bound. Tests are evaluated in the environment of the bindings to
   each SYM. After the evaluation of TESTS, further tests are
   performed that check whether each SYM is equal to the corresponding
   SET."

  `(progn
     (let ,sets
       ,@tests
       ,@(loop for (var set) in sets
	    collect `(is-set ,var ,set)))))


;;; Test HASH-SET Specific Functions

(test create-hash-set
  "Test HASH-SET Creation"

  (flet ((contains-elements? (set list)
	   (is-true
	    (loop for elem in list
	       always (nth-value 1 (get elem set)))

	    "~%~TSet:~%~%~s~%~%~Tdoes not contain expected elements:~%~%~s"
	    set list)))

    (contains-elements? (hash-set 'a 'b 'c 1 2 3 "hello" #\z) '(a b c 1 2 3 "hello" #\z))
    (contains-elements? (coerce #1='(a b c 1 2 3 "hello" #\z) 'hash-set) #1#)))

(test hash-set-equalp
  "Test EQUALP on HASH-SET's"

  (is-true (= (hash-set 1 2 3 4) (hash-set 1 4 2 3)))
  (is-true (= (hash-set "abc" #S(custom-key :slot1 a :slot2 b)) (hash-set #S(custom-key :slot1 a :slot2 b) "abc")))

  (is-false (= (hash-set 1 2 3 4) (hash-set 1 2 3 4 5)))
  (is-false (= (hash-set 1 2 3 4 5) (hash-set 1 2 3 4)))
  (is-false (= (hash-set) (hash-set 1 2 3)))
  (is-false (= (hash-set 1 2 3) (hash-set))))


;;; Test MEMBERP

(test-set-fn memberp
    ((set 1 3 "hello" #S(custom-key :slot1 a :slot2 c) 4 9 15)
     (empty))

  (is-true (memberp 1 set))
  (is-true (memberp 3 set))
  (is-true (memberp "hello" set))
  (is-true (memberp #S(custom-key :slot1 a :slot2 c) set))
  (is-true (memberp 4 set))
  (is-true (memberp 9 set))
  (is-true (memberp 15 set))

  (is-false (memberp 100 set))
  (is-false (memberp "HELLO" set))
  (is-false (memberp 'x empty)))

(test-set-fn subsetp
    ((super 1 3 "hello" #S(custom-key :slot1 a :slot2 c) 4 9 15)
     (sub1 1 9 "hello")
     (sub2 #S(custom-key :slot1 a :slot2 c) 15)
     (sub3 3)
     (not-sub 1 3 4 5)
     (empty))

  (is-true (subsetp sub1 super))
  (is-true (subsetp sub2 super))
  (is-true (subsetp sub3 super))
  (is-true (subsetp empty super))
  (is-true (subsetp empty sub3))

  (is-false (subsetp sub1 sub2))
  (is-false (subsetp not-sub super))
  (is-false (subsetp super sub1))
  (is-false (subsetp super sub2))
  (is-false (subsetp super empty)))


;;; Test ADJOIN Functions

(test-set-fn adjoin-1
    ((elems 1 2 3 "hello" #S(custom-key :slot1 a :slot2 b))
     (res 1 2 3 4 "hello" #S(custom-key :slot1 a :slot2 b)))

  (test-set-not-modified
      ((set elems))

    (is-set (adjoin 4 set) res)))

(test-set-fn adjoin-2
    ((set 1 2 3 "hello" #S(custom-key :slot1 a :slot2 b))
     (empty)
     (res 1))

  (is-set (adjoin #S(custom-key :slot1 a :slot2 b) set) set)
  (is-set (adjoin 1 empty) res))

(test-set-fn nadjoin
    ((set 1 2 3 "hello" #S(custom-key :slot1 a :slot2 b))
     (res1 1 2 3 4 "hello" #S(custom-key :slot1 a :slot2 b))

     (empty)
     (res2 1))

  (is-set (nadjoin 4 set) res1)
  (is-set (nadjoin #S(custom-key :slot1 a :slot2 b) set) set)
  (is-set (nadjoin 1 empty) res2))


;;; Test INTERSECTION functions

(test-set-fn intersection
    ((elems1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
     (elems2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
     (elems3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
     (empty)
     (res 3 #S(custom-key :slot1 1 :slot2 2) "hello"))

  ;; Test non-empty intersection

  (test-set-not-modified
   ((set1 elems1)
    (set2 elems2))

   (is-set (intersection set1 set2) res))

  ;; Test commutativity

  (test-set-not-modified
      ((set1 elems1)
       (set2 elems2))

    (is-set (intersection set2 set1) res))

  ;; Test self-intersection

  (test-set-not-modified
      ((set1 elems1))

    (is-set (intersection set1 set1) elems1))

  ;; Test empty intersection

  (test-set-not-modified
      ((set1 elems1)
       (set3 elems3))

    (is-set (intersection set1 set3) empty))

  ;; Test intersection with the empty set

  (test-set-not-modified
      ((set1 empty)
       (set2 elems2))

    (is-set (intersection set1 set2) empty))

  (test-set-not-modified
      ((set1 empty)
       (set2 elems2))

    (is-set (intersection set2 set1) empty))

  (test-set-not-modified
      ((set empty))

    (is-set (intersection set set) empty)))

(test-set-fn nintersection
    ((set1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
     (set2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
     (set3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
     (empty)
     (res 3 #S(custom-key :slot1 1 :slot2 2) "hello"))

  ;; Test non-empty intersection
  (is-set (nintersection set1 set2) res)
  ;; Test commutativity
  (is-set (nintersection set2 set1) res)
  ;; Test empty intersection
  (is-set (nintersection set1 set3) empty)
  ;; Test self-intersection
  (is-set (nintersection set1 set1) set1)
  ;; Test intersection with the empty set
  (is-set (nintersection empty set2) empty)
  (is-set (nintersection set2 empty) empty)
  (is-set (nintersection empty empty) empty))


;;; Test SET-DIFFERENCE Functions

(test-set-fn set-difference
    ((elems1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
     (elems2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
     (elems3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
     (empty)
     (res1 1 2 #S(custom-key :slot1 a :slot2 c))
     (res2 6 9 "world" #S(custom-key :slot1 a :slot2 b)))

  ;; Test set-difference with common elements

  (test-set-not-modified
   ((set1 elems1)
    (set2 elems2))

   (is-set (set-difference set1 set2) res1))

  (test-set-not-modified
   ((set1 elems1)
    (set2 elems2))

   (is-set (set-difference set2 set1) res2))

  ;; Test set-difference with no common elements

  (test-set-not-modified
      ((set1 elems1)
       (set3 elems3))

    (is-set (set-difference set1 set3) elems1))

  (test-set-not-modified
      ((set1 elems1)
       (set3 elems3))

    (is-set (set-difference set3 set1) elems3))

  ;; Test set-difference with self

  (test-set-not-modified
      ((set elems1))

    (is-set (set-difference set set) empty))

  ;; Test set-difference with the empty set

  (test-set-not-modified
      ((set1 empty)
       (set2 elems2))

    (is-set (set-difference set1 set2) empty))

  (test-set-not-modified
      ((set1 empty)
       (set2 elems2))

    (is-set (set-difference set2 set1) elems2))

  (test-set-not-modified
      ((set empty))

    (is-set (set-difference set set) empty)))

(test-set-fn nset-difference
    ((set1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
     (set2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
     (set3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
     (empty)
     (res1 1 2 #S(custom-key :slot1 a :slot2 c))
     (res2 6 9 "world" #S(custom-key :slot1 a :slot2 b)))

  ;; Test set-difference with common elements
  (is-set (nset-difference set1 set2) res1)
  (is-set (nset-difference set2 set1) res2)

  ;; Test set-difference with no common elements
  (is-set (nset-difference set1 set3) set1)
  (is-set (nset-difference set3 set1) set3)

  ;; Test set-difference with self
  (is-set (nset-difference set1 set1) empty)

  ;; Test set-difference with the empty set
  (is-set (nset-difference empty set2) empty)
  (is-set (nset-difference set2 empty) set2)
  (is-set (nset-difference empty empty) empty))


;;; Test SET-EXCLUSIVE-OR functions

(test-set-fn set-exclusive-or
    ((elems1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
     (elems2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
     (elems3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
     (empty)
     (res1 1 2 6 9 "world" #S(custom-key :slot1 a :slot2 c) #S(custom-key :slot1 a :slot2 b))
     (res2 1 2 3 4 5 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "Hello"))

  ;; Test set-exclusive-or with common elements

  (test-set-not-modified
      ((set1 elems1)
       (set2 elems2))

    (is-set (set-exclusive-or set1 set2) res1))

  (test-set-not-modified
      ((set1 elems1)
       (set2 elems2))

    (is-set (set-exclusive-or set2 set1) res1))

  ;; Test set-exclusive-or with no common elements

  (test-set-not-modified
      ((set1 elems1)
       (set3 elems3))

    (is-set (set-exclusive-or set1 set3) res2))

  (test-set-not-modified
      ((set1 elems1)
       (set3 elems3))

    (is-set (set-exclusive-or set3 set1) res2))

  ;; Test set-exclusive-or with self

  (test-set-not-modified
      ((set elems1))

    (is-set (set-exclusive-or set set) empty))

  ;; Test set-exclusive-or with the empty set

  (test-set-not-modified
      ((set1 empty)
       (set2 elems2))

    (is-set (set-exclusive-or set1 set2) elems2))

  (test-set-not-modified
      ((set1 empty)
       (set2 elems2))

    (is-set (set-exclusive-or set2 set1) elems2))

  (test-set-not-modified
      ((set empty))

    (is-set (set-exclusive-or set set) empty)))

(test-set-fn nset-exclusive-or
    ((set1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
     (set2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
     (set3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
     (empty)
     (res1 1 2 6 9 "world" #S(custom-key :slot1 a :slot2 c) #S(custom-key :slot1 a :slot2 b))
     (res2 1 2 3 4 5 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "Hello"))

  ;; Test set-exclusive-or with common elements
  (is-set (nset-exclusive-or set1 set2) res1)
  (is-set (nset-exclusive-or set2 set1) res1)

  ;; Test set-exclusive-or with no common elements
  (is-set (nset-exclusive-or set1 set3) res2)
  (is-set (nset-exclusive-or set3 set1) res2)

  ;; Test set-exclusive-or with self
  (is-set (nset-exclusive-or set1 set1) empty)

  ;; Test set-exclusive-or with the empty set
  (is-set (nset-exclusive-or empty set2) set2)
  (is-set (nset-exclusive-or set2 empty) set2)
  (is-set (nset-exclusive-or empty empty) empty))


;;; Test UNION functions

(test-set-fn union
    ((elems1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
     (elems2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
     (elems3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
     (empty)
     (res1 1 2 3 6 9 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "world")
     (res2 1 2 3 4 5 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "Hello"))

  ;; Test union with common elements

  (test-set-not-modified
      ((set1 elems1)
       (set2 elems2))

    (is-set (union set1 set2) res1))

  (test-set-not-modified
      ((set1 elems1)
       (set2 elems2))

    (is-set (union set2 set1) res1))

  ;; Test union with no common elements

  (test-set-not-modified
      ((set1 elems1)
       (set3 elems3))

    (is-set (union set1 set3) res2))

  (test-set-not-modified
      ((set1 elems1)
       (set3 elems3))

    (is-set (union set3 set1) res2))

  ;; Test union with self

  (test-set-not-modified
      ((set elems1))

    (is-set (union set set) elems1))

  ;; Test union with the empty set

  (test-set-not-modified
      ((set1 empty)
       (set2 elems2))

    (is-set (union set1 set2) elems2))

  (test-set-not-modified
      ((set1 empty)
       (set2 elems2))

    (is-set (union set2 set1) elems2))

  (test-set-not-modified
      ((set empty))

    (is-set (union set set) empty)))

(test-set-fn nunion
    ((set1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
     (set2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
     (set3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
     (empty)
     (res1 1 2 3 6 9 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "world")
     (res2 1 2 3 4 5 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "Hello"))

  ;; Test union with common elements
  (is-set (nunion set1 set2) res1)
  (is-set (nunion set2 set1) res1)

  ;; Test union with no common elements
  (is-set (nunion set1 set3) res2)
  (is-set (nunion set3 set1) res2)

  ;; Test union with self
  (is-set (nunion set1 set1) set1)

  ;; Test union with the empty set
  (is-set (nunion empty set2) set2)
  (is-set (nunion set2 empty) set2)
  (is-set (nunion empty empty) empty))


;;; Hash-Set Iterators

(test hash-set-iterators
  "Test hash set iterators"

  (labels ((test-set-iterator (set &rest args &key start end &allow-other-keys)
	     (let ((new-set (make-hash-set))
		   (iter (apply #'iterator set args)))
	       (loop
		  until (endp iter)
		  do
		    (setf (get (at iter) new-set) t)
		    (advance iter))

	       (cond
		 ((or start end)
		  (is (subsetp new-set set)
		      "~%~TIteration set:~%~%~s~%~%~Tnot a subset of:~%~%~s"
		      new-set set)

		  (let ((len (- (or end (length set))
				(or start 0))))
		    (is (= len (length new-set))
			"~%~TIterated over ~a elements.~%Expected to iterate over ~a elements."
			(length new-set) len)))

		 (t (is (= set new-set)))))))

    (test-set-iterator (hash-set 1 2 'a "hello" #\z))
    (test-set-iterator (hash-set 1 2 'a "hello" #\z) :from-end t)

    (test-set-iterator (hash-set 'a 'b 'c 1 2 3) :start 2 :end 4)
    (test-set-iterator (hash-set 'a 'b 'c 1 2 3) :start 2 :end 4 :from-end t)

    (test-set-iterator (hash-set 'a 'b 'c 1 2 3) :end 3)
    (test-set-iterator (hash-set 'a 'b 'c 1 2 3) :end 3 :from-end t)

    (test-set-iterator (hash-set 'a 'b 'c 1 2 3) :start 1)
    (test-set-iterator (hash-set 'a 'b 'c 1 2 3) :start 1 :from-end t)))
