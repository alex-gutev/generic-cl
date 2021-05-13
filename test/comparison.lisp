;;;; comparison.lisp
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

;;;; Unit tests for arithmetic functions

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite comparison
    :description "Test comparison functions"
    :in generic-cl)

(in-suite comparison)


;;; Number Tests

(test number-lessp
  "Test LESSP on numbers"

  (is-true (lessp 1 2))
  (is-true (lessp 3.11 10))
  (is-true (lessp -1 0.5))

  (is-false (lessp 5 2))
  (is-false (lessp 10 2.4))
  (is-false (lessp 0.3 -3)))

(test number-greaterp
  "Test GREATERP on numbers"

  (is-true (greaterp 9 3))
  (is-true (greaterp 12 5.3))
  (is-true (greaterp 1/3 -4))

  (is-false (greaterp 3 7))
  (is-false (greaterp 2.6 10))
  (is-false (greaterp -8 1/6)))

(test number-less-equal-p
  "Test LESS-EQUAL-P on numbers"

  (is-true (less-equal-p 2 7))
  (is-true (less-equal-p 7 7))
  (is-false (less-equal-p 9 8)))

(test number-greater-equal-p
  "Test GREATER-EQUAL-P on numbers"

  (is-true (greater-equal-p 10 5))
  (is-true (greater-equal-p 10 10))
  (is-false (greater-equal-p 0 90)))

(test number-compare
  "Test COMPARE on numbers"

  (is (= :less (compare 1 2)))
  (is (= :equal (compare 3 3)))
  (is (= :greater (compare 9 1))))


;;; Character Tests

(test character-lessp
  "Test LESSP on characters"

  (is-true (lessp #\a #\b))
  (is-true (lessp #\1 #\7))

  (is-false (lessp #\Z #\T))
  (is-false (lessp #\6 #\5)))

(test character-greaterp
  "Test GREATERP on characters"

  (is-true (greaterp #\x #\d))
  (is-true (greaterp #\4 #\1))

  (is-false (greaterp #\A #\F))
  (is-false (greaterp #\0 #\5)))

(test character-less-equal-p
  "Test LESS-EQUAL-P on characters"

  (is-true (less-equal-p #\a #\z))
  (is-true (less-equal-p #\c #\c))
  (is-false (less-equal-p #\x #\f)))

(test character-greater-equal-p
  "Test GREATER-EQUAL-P on characters"

  (is-true (greater-equal-p #\x #\f))
  (is-true (greater-equal-p #\r #\r))
  (is-false (greater-equal-p #\b #\f)))

(test character-compare
  "Test COMPARE on characters"

  ;; This also tests the generic COMPARE method since there is
  ;; no COMPARE method specialized on characters.

  (is (= :less (compare #\a #\f)))
  (is (= :equal (compare #\e #\e)))
  (is (= :greater (compare #\x #\t))))


;;; String Tests

(test string-lessp
  "Test LESSP on strings"

  (is-true (lessp "aaa" "aab"))
  (is-true (lessp "hello" "hello world"))
  (is-true (lessp "hello1" "hello2"))

  (is-false (lessp "aax" "aaa"))
  (is-false (lessp "hello world" "hello"))
  (is-false (lessp "hello2" "hello1")))

(test string-greaterp
  "Test GREATERP on strings"

  (is-true (greaterp "aax" "aaa"))
  (is-true (greaterp "hello world" "hello"))
  (is-true (greaterp "hello3" "hello1"))

  (is-false (greaterp "aaa" "aab"))
  (is-false (greaterp "hello" "hello world"))
  (is-false (greaterp "hello1" "hello2")))

(test string-less-equal-p
  "Test LESS-EQUAL-P on strings"

  (is-true (less-equal-p "aaa" "aab"))
  (is-true (less-equal-p "aaa" "aaa"))
  (is-false (less-equal-p "aab" "aaa")))

(test string-greater-equal-p
  "Test GREATER-EQUAL-P on strings"

  (is-true (greater-equal-p "aab" "aaa"))
  (is-true (greater-equal-p "aaa" "aaa"))
  (is-false (greater-equal-p "aaa" "aab")))

(test string-compare
  "Test COMPARE on strings"

  (is (= :less (compare "hello1" "hello2")))
  (is (= :equal (compare "hello" "hello")))
  (is (= :greater (compare "hello2" "hello1"))))


;;; N-Argument Functions

(test-nary compare <
  (is-true (< 1))
  (is-true (< -1))
  (is-true (< #\a #\b))
  (is-true (< 3 4))
  (is-true (< 3 4 5))
  (is-true (< 2 3 4 5 6))
  (is-false (< 2 3 5 4 6)))

(test-nary compare >
  (is-true (> 2))
  (is-true (> -3))
  (is-true (> #\3 #\1))
  (is-true (> 5 3))
  (is-true (> 5 4 3))
  (is-true (> 5 4 3 2 1))
  (is-false (> 5 4 1 2 3)))

(test-nary compare <=
  (is-true (<= 1))
  (is-true (<= -1))
  (is-true (<= #\a #\a))
  (is-true (<= 1 1))
  (is-true (<= 1 2))
  (is-true (<= 1 2 3))
  (is-true (<= 1 2 3 4))
  (is-false (<= 1 2 4 3)))

(test-nary compare >=
  (is-true (>= 4))
  (is-true (>= -4))
  (is-true (>= #\1 #\1))
  (is-true (>= 2 2))
  (is-true (>= 3 2))
  (is-true (>= 3 2 1))
  (is-true (>= 3 3 2 1))
  (is-false (>= 3 4 2 1)))

(test-nary compare max
  (is (= 5 (max 1 3 5 0 2)))
  (is (= #\z (max #\a #\b #\z #\d)))
  (is (= "xyz" (max "abc" "def" "xyz")))

  (is (= 1 (max 1)))
  (is (= #\a (max #\a))))

(test-nary compare min
  (is (= 0 (min 1 3 5 0 2)))
  (is (= #\a (min #\a #\b #\z #\d)))
  (is (= "abc" (min "def" "abc" "xyz")))

  (is (= 1 (min 1)))
  (is (= #\a (min #\a))))
