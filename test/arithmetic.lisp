;;;; arithmetic.lisp
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

(subtest "Test Arithmetic Functions"
  (subtest "Test ADD"
    (is (add 1 2) 3)
    (is-error (add 1 'x) 'error)

    (is (+) 0)
    (is (+ 2) 2)
    (is (+ 1 2 3 4) 10))

  (subtest "Test SUBTRACT"
    (is (subtract 3 4) -1)
    (is-error (subtract 3 "z") 'error)

    (is (- 3) -3)
    (is (- 5 4 3) -2))

  (subtest "Test MULTIPLY"
    (is (multiply 2 4) 8)
    (is-error (multiply 4 #\a) 'error)

    (is (*) 1)
    (is (* 3) 3)
    (is (* 2 3 4 5) 120))

  (subtest "Test DIVIDE"
    (is (divide 6 3) 2)
    (is-error (divide 'a 'b) 'error)

    (is (/ 5) 1/5)
    (is (/ 6 3 2) 1))

  (subtest "Test NEGATE"
    (is (negate 4) -4)
    (is-error (negate 'x) 'error))

  (subtest "Test 1+, 1-, INCF and DECF"
    (is (1+ 4) 5)
    (is (1- 8) 7)

    (let ((x 2))
      (is (incf x) 3)
      (is x 3))

    (let ((x 6))
      (is (decf x) 5)
      (is x 5)))

  (subtest "Test MINUSP"
    (ok (minusp -5))
    (ok (minusp -1))

    (ok (not (minusp 0)))
    (ok (not (minusp 3))))

  (subtest "Test PLUSP"
    (ok (plusp 5))
    (ok (plusp 1))

    (ok (not (plusp 0)))
    (ok (not (plusp -3))))

  (subtest "Test ZEROP"
    (ok (zerop 0))

    (ok (not (zerop -1)))
    (ok (not (zerop 5))))

  (subtest "Test SIGNUM"
    (is (signum 3) 1)
    (is (signum -1) -1)
    (is (signum 0) 0))

  (subtest "Test ABS"
    (is (abs 10) 10)
    (is (abs 0) 0)
    (is (abs -3) 3))

  (subtest "Test EVENP"
    (ok (evenp 2))
    (ok (evenp 10))
    (ok (evenp 0))
    (ok (evenp -4))
    (ok (not (evenp 5)))
    (ok (not (evenp -3))))

  (subtest "Test ODDP"
    (ok (oddp 3))
    (ok (oddp 7))
    (ok (oddp -5))
    (ok (not (oddp 0)))
    (ok (not (oddp 4)))
    (ok (not (oddp -6))))

  (subtest "Test Rounding"
    (subtest "Test FLOOR"
      (is (floor 3.0222) 3)
      (is (floor -3.0222) -4)

      (is-values (floor 10 3) '(3 1))
      (is-values (floor -10 3) '(-4 2)))

    (subtest "Test CEILING"
      (is (ceiling 3.0222) 4)
      (is (ceiling -3.0222) -3)

      (is-values (ceiling 10 3) '(4 -2))
      (is-values (ceiling -10 3) '(-3 -1)))

    (subtest "Test TRUNCATE"
      (is (truncate 3.0222) 3)
      (is (truncate -3.0222) -3)

      (is-values (truncate 10 3) '(3 1))
      (is-values (truncate -10 3) '(-3 -1)))

    (subtest "Test ROUND"
      (is (round 3.0222) 3)
      (is (round -3.0222) -3)

      (is (round 3.7222) 4)
      (is (round -3.7222) -4)

      (is (round 4.5) 4)
      (is (round -4.5) -4)

      (is (round 1.5) 2)
      (is (round -1.5) -2)

      (is-values (round 10 3) '(3 1))
      (is-values (round -10 3) '(-3 -1))

      (is-values (round 5 2) '(2 1))
      (is-values (round -5 2) '(-2 -1))))

  (subtest "Test Modulus Operations"
    (subtest "MOD"
      (is (mod 5 3) 2)
      (is (mod -5 3) 1))

    (subtest "REM"
      (is (rem 5 3) 2)
      (is (rem -5 3) -2))))

(finalize)
