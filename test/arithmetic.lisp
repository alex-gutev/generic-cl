;;;; arithmetic.lisp
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

(def-suite arithmetic
    :description "Test arithmetic functions"
    :in generic-cl)

(in-suite arithmetic)


;;; Tests

(test number-add
  "Test ADD on numbers"

  (is (= 3 (add 1 2)))
  (signals error (add 1 'x)))

(test-nary number +
  (is (= 0 (+)))
  (is (= 2 (+ 2)))
  (is (= 10 (+ 1 2 3 4))))

(test number-subtract
  "Test SUBTRACT on numbers"

  (is (= -1 (subtract 3 4)))
  (signals error (subtract 3 "z")))

(test-nary number -
  (is (= -3 (- 3)))
  (is (= -2 (- 5 4 3))))

(test number-multiply
  "Test MULTIPLY on numbers"

  (is (= 8 (multiply 2 4)))
  (signals error (multiply 4 #\a)))

(test-nary number *
  (is (= 1 (*)))
  (is (= 3 (* 3)))
  (is (= 120 (* 2 3 4 5))))

(test number-divide
  "Test DIVIDE on numbers"

  (is (= 2 (divide 6 3)))
  (signals error (divide 'a 'b)))

(test-nary number /
  (is (= 1/5 (/ 5)))
  (is (= 1 (/ 6 3 2))))

(test number-negate
  "Test NEGATE on numbers"

  (is (= -4 (negate 4)))
  (signals error (negate 'x)))

(test number-increments
  "Test 1+, 1-, INCF and DECF on numbers"

  (is (= 5 (1+ 4)))
  (is (= 7 (1- 8)))

  (let ((x 2))
    (is (= 3 (incf x)))
    (is (= 3 x)))

  (let ((x 6))
    (is (= 5 (decf x)))
    (is (= 5 x))))

(test number-minusp
  "Test MINUSP on numbers"

  (is-true (minusp -5))
  (is-true (minusp -1))

  (is-false (minusp 0))
  (is-false (minusp 3)))

(test number-plusp
  "Test PLUSP on numbers"

  (is-true (plusp 5))
  (is-true (plusp 1))

  (is-false (plusp 0))
  (is-false (plusp -3)))

(test number-zerop
  "Test ZEROP on numbers"

  (is-true (zerop 0))

  (is-false (zerop -1))
  (is-false (zerop 5)))

(test number-signum
  "Test SIGNUM on numbers"

  (is (= 1 (signum 3)))
  (is (= -1 (signum -1)))
  (is (= 0 (signum 0))))

(test number-abs
  "Test ABS on numbers"

  (is (= 10 (abs 10)))
  (is (= 0 (abs 0)))
  (is (= 3 (abs -3))))

(test number-evenp
  "Test EVENP on numbers"

  (is-true (evenp 2))
  (is-true (evenp 10))
  (is-true (evenp 0))
  (is-true (evenp -4))
  (is-false (evenp 5))
  (is-false (evenp -3)))

(test number-oddp
  "Test ODDP on numbers"

  (is-true (oddp 3))
  (is-true (oddp 7))
  (is-true (oddp -5))
  (is-false (oddp 0))
  (is-false (oddp 4))
  (is-false (oddp -6)))

(test number-floor
  "Test FLOOR on numbers"

  (is (= 3 (floor 3.0222)))
  (is (= -4 (floor -3.0222)))

  (is (= '(3 1) (multiple-value-list (floor 10 3))))
  (is (= '(-4 2) (multiple-value-list (floor -10 3)))))

(test number-ceiling
  "Test CEILING on numbers"

  (is (= 4 (ceiling 3.0222)))
  (is (= -3 (ceiling -3.0222)))

  (is (= '(4 -2) (multiple-value-list (ceiling 10 3))))
  (is (= '(-3 -1) (multiple-value-list (ceiling -10 3)))))

(test number-truncate
  "Test TRUNCATE on numbers"

  (is (= 3 (truncate 3.0222)))
  (is (= -3 (truncate -3.0222)))

  (is (= '(3 1) (multiple-value-list (truncate 10 3))))
  (is (= '(-3 -1) (multiple-value-list (truncate -10 3)))))

(test number-round
  "Test ROUND on numbers"

  (is (= 3 (round 3.0222)))
  (is (= -3 (round -3.0222)))

  (is (= 4 (round 3.7222)))
  (is (= -4 (round -3.7222)))

  (is (= 4 (round 4.5)))
  (is (= -4 (round -4.5)))

  (is (= 2 (round 1.5)))
  (is (= -2 (round -1.5)))

  (is (= '(3 1) (multiple-value-list (round 10 3))))
  (is (= '(-3 -1) (multiple-value-list (round -10 3))))

  (is (= '(2 1) (multiple-value-list (round 5 2))))
  (is (= '(-2 -1) (multiple-value-list (round -5 2)))))

(test number-mod
  "Test MOD on numbers"

  (is (= 2 (mod 5 3)))
  (is (= 1 (mod -5 3))))

(test number-rem
  "Test REM on numbers"

  (is (= 2 (rem 5 3)))
  (is (= -2 (rem -5 3))))
