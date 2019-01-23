;;;; sets.lisp
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

;;;; Unit tests for generic set functions

(in-package :generic-cl.test)

(plan nil)

(macrolet ((test-set-fn ((&rest sets) &body tests)
	     "Tests a function on a LIST and `HASH-SET' simultaneously.
              Each element of SETS is of the form (SYM LIST) where SYM
              is a symbol and list is a LIST (evaluated). The forms in
              TESTS are evaluated twice, first with each SYM bound to
              the corresponding LIST, then with each SYM bound to a
              `HASH-SET' with the contents of the corresponding
              LIST. Both bindings are established by SYMBOL-MACROLET."

	     `(progn
		(symbol-macrolet
		    ,(loop for (var . list) in sets
			collect `(,var (list ,@list)))

		  (diag "List")

		  (macrolet ((is-set (got expected &rest args)
			       `(is ,got ,expected ,@args
				   :test (rcurry #'set-equal :test #'equalp))))
		    ,@tests))

		(symbol-macrolet
		    ,(loop for (var . list) in sets
			collect `(,var (hash-set ,@list)))

		  (diag "Hash Set")

		  (macrolet ((is-set (got expected &rest args)
			       `(is ,got ,expected ,@args :test #'equalp)))
		    ,@tests))))

	   (test-not-modified ((&rest sets) &body tests)
	     "Tests that a set is not modified after the evaluation of
              TESTS. Each element of SETS is of the form (SYM SET)
              where SYM is the symbol to which the result of the
              evaluation of SET is bound. Tests are evaluated in the
              environment of the bindings to each SYM. After the
              evaluation of TESTS, further tests are performed that
              check whether each SYM is equal to the corresponding
              SET."

	     `(progn
		(let ,sets
		  ,@tests
		  ,@(loop for (var set) in sets
		       collect `(is-set ,var ,set "Not Modified"))))))

  (subtest "Test Hash Set Functions"
    (subtest "Test EQUALP"
      (is (hash-set 1 2 3 4) (hash-set 1 4 2 3) :test #'equalp)
      (is (hash-set "abc" #S(custom-key :slot1 a :slot2 b)) (hash-set #S(custom-key :slot1 a :slot2 b) "abc") :test #'equalp)

      (isnt (hash-set 1 2 3 4) (hash-set 1 2 3 4 5) :test #'equalp)
      (isnt (hash-set 1 2 3 4 5) (hash-set 1 2 3 4) :test #'equalp)
      (isnt (hash-set) (hash-set 1 2 3) :test #'equalp)
      (isnt (hash-set 1 2 3) (hash-set) :test #'equalp)))

  (subtest "Test Generic Set Functions"
    (subtest "Test MEMBERP"
      (test-set-fn
       ((set 1 3 "hello" #S(custom-key :slot1 a :slot2 c) 4 9 15)
	(empty))

       (ok (memberp 1 set))
       (ok (memberp 3 set))
       (ok (memberp "hello" set))
       (ok (memberp #S(custom-key :slot1 a :slot2 c) set))
       (ok (memberp 4 set))
       (ok (memberp 9 set))
       (ok (memberp 15 set))

       (ok (not (memberp 100 set)))
       (ok (not (memberp "HELLO" set)))
       (ok (not (memberp 'x empty)))))

    (subtest "Test SUBSETP"
      (test-set-fn
       ((super 1 3 "hello" #S(custom-key :slot1 a :slot2 c) 4 9 15)
	(sub1 1 9 "hello")
	(sub2 #S(custom-key :slot1 a :slot2 c) 15)
	(sub3 3)
	(not-sub 1 3 4 5)
	(empty))

       (ok (subsetp sub1 super))
       (ok (subsetp sub2 super))
       (ok (subsetp sub3 super))
       (ok (subsetp empty super))
       (ok (subsetp empty sub3))

       (ok (not (subsetp sub1 sub2)))
       (ok (not (subsetp not-sub super)))
       (ok (not (subsetp super sub1)))
       (ok (not (subsetp super sub2)))
       (ok (not (subsetp super empty)))))

    (subtest "Test ADJOIN Functions"
      (subtest "Test ADJOIN"
	(test-set-fn
	 ((elems 1 2 3 "hello" #S(custom-key :slot1 a :slot2 b))
	  (res 1 2 3 4 "hello" #S(custom-key :slot1 a :slot2 b)))

	 (test-not-modified
	  ((set elems))
	  (is-set (adjoin 4 set) res)))

	(test-set-fn
	 ((set 1 2 3 "hello" #S(custom-key :slot1 a :slot2 b))
	  (empty)
	  (res 1))

	 (is-set (adjoin #S(custom-key :slot1 a :slot2 b) set) set)
	 (is-set (adjoin 1 empty) res)))

      (subtest "Test NADJOIN"
	(test-set-fn
	 ((set 1 2 3 "hello" #S(custom-key :slot1 a :slot2 b))
	  (res1 1 2 3 4 "hello" #S(custom-key :slot1 a :slot2 b))

	  (empty)
	  (res2 1))

	 (is-set (nadjoin 4 set) res1)
	 (is-set (nadjoin #S(custom-key :slot1 a :slot2 b) set) set)
	 (is-set (nadjoin 1 empty) res2))))

    (subtest "Test INTERSECTION Functions"
      (subtest "Test INTERSECTION"
	(test-set-fn
	 ((elems1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
	  (elems2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
	  (elems3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
	  (empty)
	  (res 3 #S(custom-key :slot1 1 :slot2 2) "hello"))

	 ;; Test non-empty intersection

	 (test-not-modified
	  ((set1 elems1)
	   (set2 elems2))

	  (is-set (intersection set1 set2) res))

	 ;; Test commutativity

	 (test-not-modified
	  ((set1 elems1)
	   (set2 elems2))

	  (is-set (intersection set2 set1) res))

	 ;; Test self-intersection

	 (test-not-modified
	  ((set1 elems1))

	  (is-set (intersection set1 set1) elems1))

	 ;; Test empty intersection

	 (test-not-modified
	  ((set1 elems1)
	   (set3 elems3))

	  (is-set (intersection set1 set3) empty))

	 ;; Test intersection with the empty set

	 (test-not-modified
	  ((set1 empty)
	   (set2 elems2))

	  (is-set (intersection set1 set2) empty))

	 (test-not-modified
	  ((set1 empty)
	   (set2 elems2))

	  (is-set (intersection set2 set1) empty))

	 (test-not-modified
	  ((set empty))

	  (is-set (intersection set set) empty))))

      (subtest "Test NINTERSECTION"
	(test-set-fn
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
	 (is-set (nintersection empty empty) empty))))

    (subtest "Test SET-DIFFERENCE Functions"
      (subtest "Test SET-DIFFERENCE"
	(test-set-fn
	 ((elems1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
	  (elems2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
	  (elems3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
	  (empty)
	  (res1 1 2 #S(custom-key :slot1 a :slot2 c))
	  (res2 6 9 "world" #S(custom-key :slot1 a :slot2 b)))

	 ;; Test set-difference with common elements

	 (test-not-modified
	  ((set1 elems1)
	   (set2 elems2))

	  (is-set (set-difference set1 set2) res1))

	 (test-not-modified
	  ((set1 elems1)
	   (set2 elems2))

	  (is-set (set-difference set2 set1) res2))

	 ;; Test set-difference with no common elements

	 (test-not-modified
	  ((set1 elems1)
	   (set3 elems3))

	  (is-set (set-difference set1 set3) elems1))

	 (test-not-modified
	  ((set1 elems1)
	   (set3 elems3))

	  (is-set (set-difference set3 set1) elems3))

	 ;; Test set-difference with self

	 (test-not-modified
	  ((set elems1))

	  (is-set (set-difference set set) empty))

	 ;; Test set-difference with the empty set

	 (test-not-modified
	  ((set1 empty)
	   (set2 elems2))

	  (is-set (set-difference set1 set2) empty))

	 (test-not-modified
	  ((set1 empty)
	   (set2 elems2))

	  (is-set (set-difference set2 set1) elems2))

	 (test-not-modified
	  ((set empty))

	  (is-set (set-difference set set) empty))))

      (subtest "Test NSET-DIFFERENCE"
	(test-set-fn
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
	 (is-set (nset-difference empty empty) empty))))

    (subtest "Test SET-EXCLUSIVE-OR Functions"
      (subtest "Test SET-EXCLUSIVE-OR"
	(test-set-fn
	 ((elems1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
	  (elems2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
	  (elems3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
	  (empty)
	  (res1 1 2 6 9 "world" #S(custom-key :slot1 a :slot2 c) #S(custom-key :slot1 a :slot2 b))
	  (res2 1 2 3 4 5 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "Hello"))

	 ;; Test set-exclusive-or with common elements

	 (test-not-modified
	  ((set1 elems1)
	   (set2 elems2))

	  (is-set (set-exclusive-or set1 set2) res1))

	 (test-not-modified
	  ((set1 elems1)
	   (set2 elems2))

	  (is-set (set-exclusive-or set2 set1) res1))

	 ;; Test set-exclusive-or with no common elements

	 (test-not-modified
	  ((set1 elems1)
	   (set3 elems3))

	  (is-set (set-exclusive-or set1 set3) res2))

	 (test-not-modified
	  ((set1 elems1)
	   (set3 elems3))

	  (is-set (set-exclusive-or set3 set1) res2))

	 ;; Test set-exclusive-or with self

	 (test-not-modified
	  ((set elems1))

	  (is-set (set-exclusive-or set set) empty))

	 ;; Test set-exclusive-or with the empty set

	 (test-not-modified
	  ((set1 empty)
	   (set2 elems2))

	  (is-set (set-exclusive-or set1 set2) elems2))

	 (test-not-modified
	  ((set1 empty)
	   (set2 elems2))

	  (is-set (set-exclusive-or set2 set1) elems2))

	 (test-not-modified
	  ((set empty))

	  (is-set (set-exclusive-or set set) empty))))

      (subtest "Test NSET-EXCLUSIVE-OR"
	(test-set-fn
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
	 (is-set (nset-exclusive-or empty empty) empty))))

    (subtest "Test UNION Functions"
      (subtest "Test UNION"
	(test-set-fn
	 ((elems1 1 2 3 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 c) "hello")
	  (elems2 3 6 9 "hello" "world" #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b))
	  (elems3 4 5 #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) "Hello")
	  (empty)
	  (res1 1 2 3 6 9 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "world")
	  (res2 1 2 3 4 5 #S(custom-key :slot1 1 :slot2 2) #S(custom-key :slot1 2 :slot2 3) #S(custom-key :slot1 a :slot2 b) #S(custom-key :slot1 a :slot2 c) "hello" "Hello"))

	 ;; Test union with common elements

	 (test-not-modified
	  ((set1 elems1)
	   (set2 elems2))

	  (is-set (union set1 set2) res1))

	 (test-not-modified
	  ((set1 elems1)
	   (set2 elems2))

	  (is-set (union set2 set1) res1))

	 ;; Test union with no common elements

	 (test-not-modified
	  ((set1 elems1)
	   (set3 elems3))

	  (is-set (union set1 set3) res2))

	 (test-not-modified
	  ((set1 elems1)
	   (set3 elems3))

	  (is-set (union set3 set1) res2))

	 ;; Test union with self

	 (test-not-modified
	  ((set elems1))

	  (is-set (union set set) elems1))

	 ;; Test union with the empty set

	 (test-not-modified
	  ((set1 empty)
	   (set2 elems2))

	  (is-set (union set1 set2) elems2))

	 (test-not-modified
	  ((set1 empty)
	   (set2 elems2))

	  (is-set (union set2 set1) elems2))

	 (test-not-modified
	  ((set empty))

	  (is-set (union set set) empty))))

      (subtest "Test NUNION"
	(test-set-fn
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
	 (is-set (nunion empty empty) empty))))))

(finalize)
