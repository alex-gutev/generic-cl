;;;; comparison.lisp
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

;;;; Unit tests for arithmetic functions

(in-package :generic-cl.test)

(plan nil)


(subtest "Test Comparison Functions"
  (macrolet ((test (form)
	       "Tests that FORM evaluates to T, using OK, and passes
                the form itself as the test description."

	       `(ok ,form ,(format nil "~s" form)))

	     (is-lessp (a b)
	       `(test (lessp ,a ,b)))

	     (is-greaterp (a b)
	       `(test (greaterp ,a ,b)))

	     (is-less-equal-p (a b)
	       `(test (less-equal-p ,a ,b)))

	     (is-greater-equal-p (a b)
	       `(test (greater-equal-p ,a ,b)))

	     (isnt-lessp (a b)
	       `(test (not (lessp ,a ,b))))

	     (isnt-greaterp (a b)
	       `(test (not (greaterp ,a ,b))))

	     (isnt-less-equal-p (a b)
	       `(test (not (less-equal-p ,a ,b))))

	     (isnt-greater-equal-p (a b)
	       `(test (not (greater-equal-p ,a ,b)))))

    (subtest "Test Numbers"
      (subtest "Test LESSP Method"
	(is-lessp 1 2)
	(is-lessp 3.11 10)
	(is-lessp -1 0.5)

	(isnt-lessp 5 2)
	(isnt-lessp 10 2.4)
	(isnt-lessp 0.3 -3))

      (subtest "Test GREATERP Method"
	(is-greaterp 9 3)
	(is-greaterp 12 5.3)
	(is-greaterp 1/3 -4)

	(isnt-greaterp 3 7)
	(isnt-greaterp 2.6 10)
	(isnt-greaterp -8 1/6))

      (subtest "Test Or Equal Methods"
	(is-less-equal-p 2 7)
	(is-less-equal-p 7 7)
	(isnt-less-equal-p 9 8)

	(is-greater-equal-p 10 5)
	(is-greater-equal-p 10 10)
	(isnt-greater-equal-p 0 90))

      (subtest "Test COMPARE Method"
	(is (compare 1 2) :less)
	(is (compare 3 3) :equal)
	(is (compare 9 1) :greater)))

    (subtest "Test Characters"
      (subtest "Test LESSP Method"
	(is-lessp #\a #\b)
	(is-lessp #\1 #\7)

	(isnt-lessp #\Z #\T)
	(isnt-lessp #\6 #\5))

      (subtest "Test GREATERP Method"
	(is-greaterp #\x #\d)
	(is-greaterp #\4 #\1)

	(isnt-greaterp #\A #\F)
	(isnt-greaterp #\0 #\5))

      (subtest "Test Or Equal Methods"
	(is-less-equal-p #\a #\z)
	(is-less-equal-p #\c #\c)
	(isnt-less-equal-p #\x #\f)

	(is-greater-equal-p #\x #\f)
	(is-greater-equal-p #\r #\r)
	(isnt-greater-equal-p #\b #\f))

      (subtest "Test COMPARE Method"
	;; This also tests the generic COMPARE method since there is
	;; no COMPARE method specialized on characters.
	(is (compare #\a #\f) :less)
	(is (compare #\e #\e) :equal)
	(is (compare #\x #\t) :greater)))

    (subtest "Test Strings"
      (subtest "Test LESSP Method"
	(is-lessp "aaa" "aab")
	(is-lessp "hello" "hello world")
	(is-lessp "hello1" "hello2")

	(isnt-lessp "aax" "aaa")
	(isnt-lessp "hello world" "hello")
	(isnt-lessp "hello2" "hello1"))

      (subtest "Test GREATERP Method"
	(is-greaterp "aax" "aaa")
	(is-greaterp "hello world" "hello")
	(is-greaterp "hello3" "hello1")

	(isnt-greaterp "aaa" "aab")
	(isnt-greaterp "hello" "hello world")
	(isnt-greaterp "hello1" "hello2"))

      (subtest "Test Or Equal Methods"
	(is-less-equal-p "aaa" "aab")
	(is-less-equal-p "aaa" "aaa")
	(isnt-less-equal-p "aab" "aaa")

	(is-greater-equal-p "aab" "aaa")
	(is-greater-equal-p "aaa" "aaa")
	(isnt-greater-equal-p "aaa" "aab"))

      (subtest "Test COMPARE"
	(is (compare "hello1" "hello2") :less)
	(is (compare "hello" "hello") :equal)
	(is (compare "hello2" "hello1") :greater)))

    (subtest "Test N-Argument Functions"
      (subtest "Test <"
	(test-nary <
	  (test (< 1))
	  (test (< -1))
	  (test (< #\a #\b))
	  (test (< 3 4))
	  (test (< 3 4 5))
	  (test (< 2 3 4 5 6))
	  (test (not (< 2 3 5 4 6)))))

      (subtest "Test >"
	(test-nary >
	  (test (> 2))
	  (test (> -3))
	  (test (> #\3 #\1))
	  (test (> 5 3))
	  (test (> 5 4 3))
	  (test (> 5 4 3 2 1))
	  (test (not (> 5 4 1 2 3)))))

      (subtest "Test <="
	(test-nary <=
	  (test (<= 1))
	  (test (<= -1))
	  (test (<= #\a #\a))
	  (test (<= 1 1))
	  (test (<= 1 2))
	  (test (<= 1 2 3))
	  (test (<= 1 2 3 4))
	  (test (not (<= 1 2 4 3)))))

      (subtest "Test >="
	(test-nary >=
	  (test (>= 4))
	  (test (>= -4))
	  (test (>= #\1 #\1))
	  (test (>= 2 2))
	  (test (>= 3 2))
	  (test (>= 3 2 1))
	  (test (>= 3 3 2 1))
	  (test (not (>= 3 4 2 1)))))

      (subtest "Test MAX"
	(test-nary max
	  (is (max 1 3 5 0 2) 5)
	  (is (max #\a #\b #\z #\d) #\z)
	  (is (max "abc" "def" "xyz") "xyz")

	  (is (max 1) 1)
	  (is (max #\a) #\a)))

      (subtest "Test MIN"
	(test-nary min
	  (is (min 1 3 5 0 2) 0)
	  (is (min #\a #\b #\z #\d) #\a)
	  (is (min "def" "abc" "xyz") "abc")

	  (is (min 1) 1)
	  (is (min #\a) #\a))))))

(finalize)
