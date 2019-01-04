;;;; package.lisp
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

(defpackage :generic-cl.impl
  (:use :static-dispatch-cl
	:agutil

	:alexandria
	:anaphora
	:cl-arrows
        :trivia
        :iterate)

  (:import-from :agutil
		:defmacro!)

  (:shadow
   ;; Equality
   :equalp := :/=

   ;; Arithmetic
   :+ :- :* :/

   ;; Comparison
   :< :<= :> :>=

   ;; Sequences

   :elt
   :length
   :endp

   :reduce

   :count
   :count-if
   :count-if-not

   :find
   :find-if
   :find-if-not

   :position
   :position-if
   :position-if-not

   :mismatch

   :every
   :some
   :notevery
   :notany)

  (:export
   ;; Equality
   :equalp := :/=

   ;; Arithmetic
   :add
   :subtract
   :multiply
   :divide
   :negate
   :+ :- :* :/

   ;; Comparison
   :compare
   :lessp
   :greaterp
   :less-equal-p
   :greater-equal-p
   :< :<= :> :>=

   ;; Sequences
   :iterator
   :make-iterator
   :make-reverse-iterator

   :current
   :start
   :advance
   :endp

   :doseq

   :elt
   :length

   :reduce

   :count
   :count-if
   :count-if-not

   :position
   :position-if
   :position-if-not

   :find
   :find-if
   :find-if-not

   :mismatch

   :every
   :some
   :notevery
   :notany))

(agutil:define-merged-package :generic-cl
    :static-dispatch-cl
  :generic-cl.impl)
