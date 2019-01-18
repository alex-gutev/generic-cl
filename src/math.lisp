;;;; math.lisp
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

(in-package :generic-cl.math)


;;;; Generic Mathematical Functions

(defgeneric sin (a)
  (:documentation "Returns the sine of A (radians)."))

(defgeneric cos (a)
  (:documentation "Returns the cosine of A (radians)."))

(defgeneric tan (a)
  (:documentation "Returns the tangent of A (radians)."))

(defgeneric asin (a)
  (:documentation "Returns the arc sine of A."))

(defgeneric acos (a)
  (:documentation "Returns the arc cosine of A."))

(defgeneric atan (a &optional b)
  (:documentation
   "Returns the arc tangent of A. If B is supplied, returns the arc
    tangent of A/B."))


(defgeneric sinh (a)
  (:documentation "Returns the hyperbolic sine of A."))

(defgeneric cosh (a)
  (:documentation "Returns the hyperbolic cosine of A."))

(defgeneric tanh (a)
  (:documentation "Returns the hyperbolic tangent of A."))

(defgeneric asinh (a)
  (:documentation "Returns the hyperbolic arc sine of A."))

(defgeneric acosh (a)
  (:documentation "Returns the hyperbolic arc cosine of A."))

(defgeneric atanh (a)
  (:documentation "Returns the hyperbolic arc tangent of A."))


(defgeneric exp (power)
  (:documentation "Returns the natural exponent of power (e^POWER)."))

(defgeneric expt (base power)
  (:documentation "Returns BASE raised to the power POWER."))

(defgeneric log (n &optional base)
  (:documentation
   "Returns the logarithm of N in base BASE. If BASE is not supplied,
    the natural logarithm of N is returned."))


(defgeneric sqrt (a)
  (:documentation
   "Returns the square root of A."))

(defgeneric isqrt (a)
  (:documentation
   "Returns the largest integer smaller than or equal to the positive
    square root of A."))


(defgeneric realpart (a)
  (:documentation "Returns the real part of A."))

(defgeneric imagpart (a)
  (:documentation "Returns the imaginary part of A."))

(defgeneric cis (a)
  (:documentation
   "Returns a complex number equal to cos(A) + i*sin(A) where A is in
    radians."))

(defgeneric conjugate (a)
  (:documentation
   "Returns the complex conjugate of A."))

(defgeneric phase (a)
  (:documentation
   "Returns the phase of A (the angle of the polar representation of
    A)."))


(defgeneric numerator (a)
  (:documentation "Returns the numerator of A."))

(defgeneric denominator (a)
  (:documentation "Returns the numerator of A."))


(defgeneric rational (a)
  (:documentation
   "Returns a rational equivalent to A, assuming the floating point
    representation is completely accurate."))

(defgeneric rationalize (a)
  (:documentation
   "Returns a rational equivalent to A, assuming the floating point
    representation is accurate only to the floating-point
    precision."))


;;; CL:NUMBER Methods

(defmethod sin ((a number))
  (cl:sin a))

(defmethod cos ((a number))
  (cl:cos a))

(defmethod tan ((a number))
  (cl:tan a))

(defmethod asin ((a number))
  (cl:asin a))

(defmethod acos ((a number))
  (cl:acos a))

(defmethod atan ((a number) &optional (b nil b-sp))
  (if b-sp
      (cl:atan a b)
      (cl:atan a)))

(defmethod sinh ((a number))
  (cl:sinh a))

(defmethod cosh ((a number))
  (cl:cosh a))

(defmethod tanh ((a number))
  (cl:tanh a))

(defmethod asinh ((a number))
  (cl:asinh a))

(defmethod acosh ((a number))
  (cl:acosh a))

(defmethod atanh ((a number))
  (cl:atanh a))


(defmethod exp ((a number))
  (cl:exp a))

(defmethod expt ((b number) (p number))
  (cl:expt b p))

(defmethod log ((n number) &optional (base nil base-sp))
  (if base-sp
      (cl:log n base)
      (cl:log n)))


(defmethod sqrt ((a number))
  (cl:sqrt a))

(defmethod isqrt ((a number))
  (cl:isqrt a))


(defmethod realpart ((a number))
  (cl:realpart a))

(defmethod imagpart ((a number))
  (cl:imagpart a))

(defmethod cis ((a number))
  (cl:cis a))

(defmethod conjugate ((a number))
  (cl:conjugate a))

(defmethod phase ((a number))
  (cl:phase a))


(defmethod numerator ((a number))
  (cl:numerator a))

(defmethod denominator ((a number))
  (cl:denominator a))


(defmethod rational ((a number))
  (cl:rational a))

(defmethod rationalize ((a number))
  (cl:rationalize a))


;;; Generic Methods

(defmethod cis (a)
  (complex (cos a) (sin a)))

(defmethod conjugate (a)
  (complex (realpart a) (negate (imagpart a))))

(defmethod phase (a)
  (atan (imagpart a) (realpart a)))
