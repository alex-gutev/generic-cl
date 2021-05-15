;;;; misc.lisp
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

;;;; Unit tests for the miscellaneous utilities

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite misc-util
    :description "Test miscellaneous utilities"
    :in generic-cl)

(in-suite misc-util)


;;; Tests

(test defconstant
  "Test DEFCONSTANT with EQUALP test function"

  (is
   (= '(alexandria:define-constant +some-constant+ 12 :test #'equalp)
      (macroexpand-1 '(defconstant +some-constant+ 12))))

  (is (= '(alexandria:define-constant
	   +other-constant+ 'x
	   :test #'equalp
	   :documentation
	   "A constant defined with the value 'X.")

	 (macroexpand-1
	  '(defconstant +other-constant+ 'x
	    "A constant defined with the value 'X.")))))
