;;;; equality.lisp
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

;;;; Unit tests for equality predicates

(in-package :generic-cl/test)


;;; Test Utilities

(defmacro test-nary (name fn &body tests)
  "Evaluates the forms TESTS once in a LOCALLY block with the n-ary
   function FN declared INLINE (hinting that compiler-macros should be
   expanded) and once in a LOCALLY block with FN declared NOTINLINE,
   thus preventing compiler-macros from being expanded."

  `(progn
     (test ,(symbolicate name '- fn '-inline)
       (locally (declare (inline ,fn))
	 ,@tests))

     (test ,(symbolicate name '- fn '-notinline)
       (locally (declare (notinline ,fn))
	 ,@tests))))


;;; Test Suite Definition

(def-suite equality
    :description "Test equality comparison functions"
    :in generic-cl)

(in-suite equality)


;;; Test Equality (EQUALP)

(test number-equalp
  "Test EQUALP on numbers"

  (is-true (equalp 1 1))
  (is-true (equalp 2.0 2))
  (is-true (equalp 6/3 2))

  (is-false (equalp 1 0))
  (is-false (equalp 1 'x))
  (is-false (equalp 1 #\1)))

(test-nary number =
  (is-true (= 1))
  (is-true (= 1 1.0 2/2))
  (is-false (= 1 "1" #\1)))

(test-nary number /=
  (is-true "(/= 1)")
  (is-true (/= 1 "1" #\1))
  (is-false (/= 1 1.0 2/2)))

(test character-equalp
  "Test EQUALP on characters"

  (is-true (equalp #\a #\a))
  (is-true (equalp #\0 #\0))

  (is-false (equalp #\a #\A))
  (is-false (equalp #\a 'a))
  (is-false (equalp #\a "a")))

(test-nary character =
  (is-true (= #\a #\a #\a))
  (is-false (= #\a #\A 'a)))

(test-nary character /=
  (is-true (/= #\a 'a "a"))
  (is-false (/= #\a #\a #\a)))

(test list-equalp
  "Test EQUALP on Lists/CONS cells"

  (is-true (equalp '(1 2 3) (list 1.0 2 3.0)))
  (is-true (equalp '(1 a #\x) (list 2/2 'a #\x)))
  (is-true (equalp '(((1 2) x y) #\z) (list (list (list 1 2) 'x 'y) #\z)))
  (is-true (equalp '(a b . c) (list* 'a 'b 'c)))

  (is-false (equalp '(1 2 3) '(1 2 1)))
  (is-false (equalp '(1 2 3) '(1 2)))
  (is-false (equalp '(1 2 3) '(1 2 . 3))))

(test vector-equalp
  "Test EQUALP on Vectors"

  (is-true (equalp #(1 2 3) (vector 1 2 3)))
  (is-true (equalp #(1 2 3) (make-array 3 :element-type 'number
					:adjustable t
					:fill-pointer t
					:initial-contents '(1 2 3))))

  (is-true (equalp #(1 2 x) (vector 1.0 2 'x)))
  (is-true (equalp #(#(1 2) 3) (vector (vector 1.0 2.0) 3)))
  (is-true (equalp #((1 2) 3) (vector '(1.0 2.0) 3)))

  (is-false (equalp #(1 2 3) #(1 1 1)))
  (is-false (equalp #(1 2 3) #(1 2 3 4)))
  (is-false (equalp #(1 2 3) (make-array 0)))
  (is-false (equalp #(1 2 3) (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
  (is-false (equalp #(#(1 2)) #(#(2 1)))))

(test array-equalp
  "Test EQUALP on multi-dimensional arrays"

  (is-true (equalp #2A((1 2 3) (4 5 6))
		   (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))

  (is-true (equalp #2A((1 (3 4)) (5 #\c))
		   (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c)))))

  (is-false (equalp #2A((1 2) (3 4)) #2A((1 1) (3 4))))
  (is-false (equalp #2A((1 2) (3 4)) #(1 2 3 4))))

(test string-equalp
  "Test EQUALP on strings"

  (is-true (equalp "Hello" "Hello"))
  (is-true (equalp "World" (string '|World|)))
  (is-true (equalp "AAA" (make-string 3 :initial-element #\A)))
  (is-true (equalp "hello" (vector #\h #\e #\l #\l #\o)))

  (is-false (equalp "hello" "Hello"))
  (is-false (equalp "hello" '|hello|))
  (is-false (equalp "world" "worlds")))

(test pathname-equalp
  "Test EQUALP on pathnames"

  ;; This is quite complicated to test properly as there are a lot
  ;; of possible cases

  (is-true (equalp (pathname "/usr/local/bin") #p"/usr/local/bin"))

  (is-false (equalp #p"/usr/local/bin" "/usr/local/bin"))
  (is-false (equalp #p"/usr/local/bin" #p"/USR/local/bin")))

(test hash-table-equalp
  "Test EQUALP on hash-tables"

  (let ((table (make-hash-map)))
    (setf (get 'x table) 1)
    (setf (get 'y table) 'z)
    (setf (get "hello" table) "world")
    (setf (get '(1 2 3) table) #\z)

    (is-true
     (equalp table
	     (alist-hash-map
	      '((x . 1) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-true
     (equalp table
	     (alist-hash-map
	      '((x . 1) (y . z) ("HELLO" . "world") ((1 2 3) . #\z)) :test #'cl:equalp)))

    (is-false
     (equalp table
	     (alist-hash-map
	      '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-false
     (equalp table
	     (alist-hash-map
	      '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-false
     (equalp table
	     (alist-hash-map
	      '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z) ("x" . "z")))))))

(test object-equalp
  "Test default EQUALP method"

  (is-true (equalp 'a 'a))
  (is-true (equalp *standard-output* *standard-output*))

  (is-false (equalp 'a 'b)))


;;; Test Similarity (LIKEP)

(test number-likep
  "Test LIKEP on numbers"

  (is-true (likep 1 1))
  (is-true (likep 2.0 2))
  (is-true (likep 6/3 2))

  (is-false (likep 1 0))
  (is-false (likep 1 'x))
  (is-false (likep 1 #\1)))

(test character-likep
  "Test LIKEP on characters"

  (is-true (likep #\a #\a))
  (is-true (likep #\0 #\0))
  (is-true (likep #\a #\A))

  (is-false (likep #\a #\b))
  (is-false (likep #\a 'a))
  (is-false (likep #\a "a")))

(test list-likep
  "Test LIKEP on lists / CONS cells"

  (is-true (likep '(1 2 3) (list 1.0 2 3.0)))
  (is-true (likep '(1 a #\x) (list 2/2 'a #\x)))
  (is-true (likep '(((1 2) x y) #\Z) (list (list (list 1 2) 'x 'y) #\z)))
  (is-true (likep '(a b . c) (list* 'a 'b 'c)))

  (is-false (likep '(1 2 3) '(1 2 1)))
  (is-false (likep '(1 2 3) '(1 2)))
  (is-false (likep '(1 2 3) '(1 2 . 3)))
  (is-false (likep '(#\a #\b) '(#\x #\y))))

(test vector-likep
  "Test LIKEP on vectors"

  (is-true (likep #(1 2 3) (vector 1 2 3)))
  (is-true (likep #(1 2 3) (make-array 3 :element-type 'number
				       :adjustable t
				       :fill-pointer t
				       :initial-contents '(1 2 3))))

  (is-true (likep #(1 2 #\x) (vector 1.0 2 #\X)))
  (is-true (likep #(#(1 #\a) 3) (vector (vector 1.0 #\A) 3)))
  (is-true (likep #((1 2) 3) (vector '(1.0 2.0) 3)))

  (is-false (likep #(1 2 3) #(1 1 1)))
  (is-false (likep #(1 2 3) #(1 2 3 4)))
  (is-false (likep #(1 2 3) (make-array 0)))
  (is-false (likep #(1 2 3) (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
  (is-false (likep #(#(1 2)) #(#(2 1))))
  (is-false (likep #(#\a #\b) #(#\x #\y))))

(test array-likep
  "Test LIKEP on multi-dimensional arrays"

  (is-true (likep #2A((1 2 3) (4 5 6)) (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
  (is-true (likep #2A((1 (3 4)) (5 #\C)) (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c)))))

  (is-false (likep #2A((1 2) (3 4)) #2A((1 1) (3 4))))
  (is-false (likep #2A((1 2) (3 4)) #(1 2 3 4))))

(test string-likep
  "Test LIKEP on strings"

  (is-true (likep "Hello" "Hello"))
  (is-true (likep "World" (string '|World|)))
  (is-true (likep "AAA" (make-string 3 :initial-element #\A)))
  (is-true (likep "hello" (vector #\H #\e #\l #\l #\o)))
  (is-true (likep "hello" "Hello"))

  (is-false (likep "hello" "hell"))
  (is-false (likep "hello" '|hello|))
  (is-false (likep "world" "worlds")))

(test pathname-likep
  "Test LIKEP on pathnames"

  ;; This is quite complicated to test properly as there are a lot
  ;; of possible cases

  (is-true (likep (pathname "/usr/local/bin") #p"/usr/local/bin"))

  (is-false (likep #p"/usr/local/bin" "/usr/local/bin"))
  (is-false (likep #p"/usr/local/bin" #p"/USR/local/bin")))

(test hash-table-likep
  "Test LIKEP on hash-tables"

  (let ((table (make-hash-map)))
    (setf (get 'x table) 1)
    (setf (get 'y table) 'z)
    (setf (get "hello" table) "world")
    (setf (get '(1 2 3) table) #\z)

    (is-true
     (likep table
	    (alist-hash-map
	     '((x . 1) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-true
     (likep table
	    (alist-hash-map
	     '((x . 1) (y . z) ("HELLO" . "world") ((1 2 3) . #\Z)))))

    (is-false
     (likep table
	    (alist-hash-map
	     '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-false
     (likep table
	    (alist-hash-map
	     '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))))

    (is-false
     (likep table
	    (alist-hash-map
	     '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z) ("x" . "z")))))))

(test object-likep
  "Test default LIKEP method"

  (is-true (likep 'a 'a))
  (is-true (likep *standard-output* *standard-output*))

  (is-false (likep 'a 'b)))
