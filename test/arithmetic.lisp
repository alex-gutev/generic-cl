;;;; arithmetic.asd
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
  (subtest "Test ADD Method"
    (is (add 1 2) 3)
    (is-error (add 1 'x) 'error)

    (is (+) 0)
    (is (+ 2) 2)
    (is (+ 1 2 3 4) 10))

  (subtest "Test SUBTRACT Method"
    (is (subtract 3 4) -1)
    (is-error (subtract 3 "z") 'error)

    (is (- 3) -3)
    (is (- 5 4 3) -2))

  (subtest "Test MULTIPLY Method"
    (is (multiply 2 4) 8)
    (is-error (multiply 4 #\a) 'error)

    (is (*) 1)
    (is (* 3) 3)
    (is (* 2 3 4 5) 120))

  (subtest "Test DIVIDE Method"
    (is (divide 6 3) 2)
    (is-error (divide 'a 'b) 'error)

    (is (/ 5) 1/5)
    (is (/ 6 3 2) 1))

  (subtest "Test NEGATE Method"
    (is (negate 4) -4)
    (is-error (negate 'x) 'error)))

(finalize)
