;;;; equality.lisp
;;;;
;;;; Copyright 2018 Alexander Gutev
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

(in-package :generic-cl.test)

(plan nil)

(defmacro test-nary (fn &body tests)
  "Evaluates the forms TESTS once in a LOCALLY block with the n-ary
   function FN declared INLINE (hinting that compiler-macros should be
   expanded) and once in a LOCALLY block with FN declared NOTINLINE,
   thus preventing compiler-macros from being expanded."

  `(progn
     (subtest "With Compiler Macros (Declared Inline)"
       (locally (declare (inline ,fn))
	 ,@tests))

     (subtest "Without Compiler Macros (Declared Notinline)"
       (locally (declare (notinline ,fn))
	 ,@tests))))


(subtest "Equality Predicates (EQUALP)"
  (subtest "Numeric Equality"
    (is 1 1 :test #'equalp)
    (is 2.0 2 :test #'equalp)
    (is 6/3 2 :test #'equalp)

    (isnt 1 0 :test #'equalp)
    (isnt 1 'x :test #'equalp)
    (isnt 1 #\1 :test #'equalp)


    (test-nary =
      (ok (= 1) "(= 1)")
      (ok (= 1 1.0 2/2))
      (ok (not (= 1 "1" #\1))))

    (test-nary /=
      (ok (/= 1) "(/= 1)")
      (ok (/= 1 "1" #\1))
      (ok (not (/= 1 1.0 2/2)))))

  (subtest "Character Equality"
    (is #\a #\a :test #'equalp)
    (is #\0 #\0 :test #'equalp)

    (isnt #\a #\A :test #'equalp)
    (isnt #\a 'a :test #'equalp)
    (isnt #\a "a" :test #'equalp)


    (test-nary =
      (ok (= #\a #\a #\a))
      (ok (not (= #\a #\A 'a))))

    (test-nary /=
      (ok (/= #\a 'a "a"))
      (ok (not (/= #\a #\a #\a)))))

  (subtest "Cons/List Equality"
    (is '(1 2 3) (list 1.0 2 3.0) :test #'equalp)
    (is '(1 a #\x) (list 2/2 'a #\x) :test #'equalp)
    (is '(((1 2) x y) #\z) (list (list (list 1 2) 'x 'y) #\z) :test #'equalp)
    (is '(a b . c) (list* 'a 'b 'c) :test #'equalp)

    (isnt '(1 2 3) '(1 2 1) :test #'equalp)
    (isnt '(1 2 3) '(1 2) :test #'equalp)
    (isnt '(1 2 3) '(1 2 . 3) :test #'equalp))

  (subtest "Vector Equality"
    (is #(1 2 3) (vector 1 2 3) :test #'equalp)
    (is #(1 2 3) (make-array 3 :element-type 'number
                             :adjustable t
                             :fill-pointer t
                             :initial-contents '(1 2 3))
        :test #'equalp)
    (is #(1 2 x) (vector 1.0 2 'x) :test #'equalp)
    (is #(#(1 2) 3) (vector (vector 1.0 2.0) 3) :test #'equalp)
    (is #((1 2) 3) (vector '(1.0 2.0) 3) :test #'equalp)

    (isnt #(1 2 3) #(1 1 1) :test #'equalp)
    (isnt #(1 2 3) #(1 2 3 4) :test #'equalp)
    (isnt #(1 2 3) (make-array 0) :test #'equalp)
    (isnt #(1 2 3) (make-array '(2 2) :initial-contents '((1 2) (3 4))) :test #'equalp)
    (isnt #(#(1 2)) #(#(2 1)) :test #'equalp))

  (subtest "Multi-Dimensional Array Equality"
    (is #2A((1 2 3) (4 5 6)) (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))
	:test #'equalp)
    (is #2A((1 (3 4)) (5 #\c)) (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c)))
	:test #'equalp)
    (isnt #2A((1 2) (3 4)) #2A((1 1) (3 4)) :test #'equalp)
    (isnt #2A((1 2) (3 4)) #(1 2 3 4) :test #'equalp))

  (subtest "String Equality"
    (is "Hello" "Hello" :test #'equalp)
    (is "World" (string '|World|) :test #'equalp)
    (is "AAA" (make-string 3 :initial-element #\A))
    (is "hello" (vector #\h #\e #\l #\l #\o) :test #'equalp)

    (isnt "hello" "Hello" :test #'equalp)
    (isnt "hello" '|hello| :test #'equalp)
    (isnt "world" "worlds" :test #'equalp))

  (subtest "Pathname Equality"
    ;; This is quite complicated to test properly as there are a lot
    ;; of possible cases

    (is (pathname "/usr/local/bin") #p"/usr/local/bin" :test #'equalp)

    (isnt #p"/usr/local/bin" "/usr/local/bin" :test #'equalp)
    (isnt #p"/usr/local/bin" #p"/USR/local/bin" :test #'equalp))

  (subtest "Hash-Table Equality"
    (let ((table (make-hash-map)))
      (setf (get 'x table) 1)
      (setf (get 'y table) 'z)
      (setf (get "hello" table) "world")
      (setf (get '(1 2 3) table) #\z)

      (is table
          (alist-hash-map
           '((x . 1) (y . z) ("hello" . "world") ((1 2 3) . #\z)))
          :test #'equalp)
      (is table
          (alist-hash-map
           '((x . 1) (y . z) ("HELLO" . "world") ((1 2 3) . #\z)) :test #'cl:equalp)
          :test #'equalp)

      (isnt table
            (alist-hash-map
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))
            :test #'equalp)
      (isnt table
            (alist-hash-map
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))
            :test #'equalp)
      (isnt table
            (alist-hash-map
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z) ("x" . "z")))
            :test #'equalp)))

  (subtest "Generic Objects Equality"
    (is 'a 'a :test #'equalp)
    (is *standard-output* *standard-output* :test #'equalp)

    (isnt 'a 'b :test #'equalp)))

(subtest "Similarity Predicates (LIKEP)"
  (subtest "Numeric Equality"
    (is 1 1 :test #'likep)
    (is 2.0 2 :test #'likep)
    (is 6/3 2 :test #'likep)

    (isnt 1 0 :test #'likep)
    (isnt 1 'x :test #'likep)
    (isnt 1 #\1 :test #'likep))

  (subtest "Character Equality"
    (is #\a #\a :test #'likep)
    (is #\0 #\0 :test #'likep)
    (is #\a #\A :test #'likep)

    (isnt #\a #\b :test #'likep)
    (isnt #\a 'a :test #'likep)
    (isnt #\a "a" :test #'likep))

  (subtest "Cons/List Equality"
    (is '(1 2 3) (list 1.0 2 3.0) :test #'likep)
    (is '(1 a #\x) (list 2/2 'a #\x) :test #'likep)
    (is '(((1 2) x y) #\Z) (list (list (list 1 2) 'x 'y) #\z) :test #'likep)
    (is '(a b . c) (list* 'a 'b 'c) :test #'likep)

    (isnt '(1 2 3) '(1 2 1) :test #'likep)
    (isnt '(1 2 3) '(1 2) :test #'likep)
    (isnt '(1 2 3) '(1 2 . 3) :test #'likep)
    (isnt '(#\a #\b) '(#\x #\y) :test #'likep))

  (subtest "Vector Equality"
    (is #(1 2 3) (vector 1 2 3) :test #'likep)
    (is #(1 2 3) (make-array 3 :element-type 'number
                             :adjustable t
                             :fill-pointer t
                             :initial-contents '(1 2 3))
        :test #'likep)
    (is #(1 2 #\x) (vector 1.0 2 #\X) :test #'likep)
    (is #(#(1 #\a) 3) (vector (vector 1.0 #\A) 3) :test #'likep)
    (is #((1 2) 3) (vector '(1.0 2.0) 3) :test #'likep)

    (isnt #(1 2 3) #(1 1 1) :test #'likep)
    (isnt #(1 2 3) #(1 2 3 4) :test #'likep)
    (isnt #(1 2 3) (make-array 0) :test #'likep)
    (isnt #(1 2 3) (make-array '(2 2) :initial-contents '((1 2) (3 4))) :test #'likep)
    (isnt #(#(1 2)) #(#(2 1)) :test #'likep)
    (isnt #(#\a #\b) #(#\x #\y) :test #'likep))

  (subtest "Multi-Dimensional Array Equality"
    (is #2A((1 2 3) (4 5 6)) (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))
	:test #'likep)
    (is #2A((1 (3 4)) (5 #\C)) (make-array '(2 2) :initial-contents '((1 (3 4)) (5 #\c)))
	:test #'likep)

    (isnt #2A((1 2) (3 4)) #2A((1 1) (3 4)) :test #'likep)
    (isnt #2A((1 2) (3 4)) #(1 2 3 4) :test #'likep))

  (subtest "String Equality"
    (is "Hello" "Hello" :test #'likep)
    (is "World" (string '|World|) :test #'likep)
    (is "AAA" (make-string 3 :initial-element #\A))
    (is "hello" (vector #\H #\e #\l #\l #\o) :test #'likep)
    (is "hello" "Hello" :test #'likep)

    (isnt "hello" "hell" :test #'likep)
    (isnt "hello" '|hello| :test #'likep)
    (isnt "world" "worlds" :test #'likep))

  (subtest "Pathname Equality"
    ;; This is quite complicated to test properly as there are a lot
    ;; of possible cases

    (is (pathname "/usr/local/bin") #p"/usr/local/bin" :test #'likep)

    (isnt #p"/usr/local/bin" "/usr/local/bin" :test #'likep)
    (isnt #p"/usr/local/bin" #p"/USR/local/bin" :test #'likep))

  (subtest "Hash-Table Equality"
    (let ((table (make-hash-map)))
      (setf (get 'x table) 1)
      (setf (get 'y table) 'z)
      (setf (get "hello" table) "world")
      (setf (get '(1 2 3) table) #\z)

      (is table
          (alist-hash-map
           '((x . 1) (y . z) ("hello" . "world") ((1 2 3) . #\z)))
          :test #'likep)
      (is table
          (alist-hash-map
           '((x . 1) (y . z) ("HELLO" . "world") ((1 2 3) . #\Z)))
          :test #'likep)

      (isnt table
            (alist-hash-map
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))
            :test #'likep)
      (isnt table
            (alist-hash-map
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z)))
            :test #'likep)
      (isnt table
            (alist-hash-map
             '((x . 2) (y . z) ("hello" . "world") ((1 2 3) . #\z) ("x" . "z")))
            :test #'likep)))

  (subtest "Generic Objects Equality"
    (is 'a 'a :test #'likep)
    (is *standard-output* *standard-output* :test #'likep)

    (isnt 'a 'b :test #'likep)))

(finalize)
