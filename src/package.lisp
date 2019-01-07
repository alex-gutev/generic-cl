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
   :subseq
   :endp

   :map :map-into
   :fill :replace
   :concatenate
   :merge

   :reduce

   :count :count-if :count-if-not
   :find :find-if :find-if-not
   :position :position-if :position-if-not

   :search
   :mismatch

   :every :some :notevery :notany

   :reverse :nreverse
   :sort :stable-sort

   :substitute :substitute-if :substitute-if-not
   :nsubstitute :nsubstitute-if :nsubstitute-if-not
   :remove :remove-if :remove-if-not
   :delete :delete-if :delete-if-not
   :remove-duplicates :delete-duplicates)

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

   ;; Iterator Interface
   :iterator
   :make-iterator
   :make-reverse-iterator

   :current
   :start
   :advance
   :endp

   :doseq

   ;; Collector Interface
   :make-collector
   :collect
   :extend
   :collector-sequence


   ;; Sequences
   :elt :length :subseq

   :fill :replace

   :concatenate
   :nconcatenate
   :concatenate-to
   :merge

   :reduce

   :count :count-if :count-if-not
   :position :position-if :position-if-not
   :find :find-if :find-if-not

   :search
   :mismatch

   :every :some :notevery :notany

   :reverse :nreverse

   :sort :nsort :stable-sort :stable-nsort

   :substitute :substitute-if :substitute-if-not
   :nsubstitute :nsubstitute-if :nsubstitute-if-not

   :remove :remove-if :remove-if-not
   :delete :delete-if :delete-if-not

   :remove-duplicates :delete-duplicates))

(agutil:define-merged-package :generic-cl
    :static-dispatch-cl
  :generic-cl.impl)
