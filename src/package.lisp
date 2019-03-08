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
	:cl-environments.tools
	:agutil

	:alexandria
	:anaphora
	:cl-arrows
        :trivia
	:cl-custom-hash-table)

  (:import-from :agutil
		:defmacro!
		:symb)

  (:shadow
   ;; Macros
   :defstruct
   :defconstant

   ;; Equality
   :equalp := :/=

   ;; Arithmetic
   :+ :- :* :/
   :1+ :1-
   :incf :decf

   :minusp :plusp :zerop :signum
   :abs
   :evenp :oddp

   :floor :ceiling :round :truncate
   :mod :rem

   ;; Comparison
   :< :<= :> :>=
   :min :max

   ;; Misc
   :coerce

   ;; Sequences
   :elt
   :length
   :emptyp
   :subseq
   :endp
   :first
   :last

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
   :remove-duplicates :delete-duplicates

   ;; Hash-Tables
   :get

   ;; Sets
   :subsetp
   :intersection :nintersection
   :adjoin
   :set-difference :nset-difference
   :set-exclusive-or :nset-exclusive-or
   :union :nunion)

  (:export
   ;; Objects
   :copy
   :defstruct

   ;; Types
   :coerce
   :alist
   :plist

   ;; Equality
   :equalp := :/=

   ;; Arithmetic
   :add
   :subtract
   :multiply
   :divide
   :negate

   :+ :- :* :/
   :1+ :1-
   :incf :decf

   :minusp :plusp :zerop :signum
   :abs
   :evenp :oddp

   :floor :ceiling :round :truncate
   :mod :rem

   ;; Comparison
   :compare
   :lessp
   :greaterp
   :less-equal-p
   :greater-equal-p
   :< :<= :> :>=
   :min :max

   ;; Iterator Interface
   :iterator
   :make-iterator
   :make-reverse-iterator

   :at
   :start
   :advance
   :advance-n
   :endp

   :doseq
   :doiter
   :doiters

   ;; Collector Interface
   :cleared
   :sequence-of-type
   :make-sequence-of-type

   :make-collector
   :accumulate
   :extend
   :collector-sequence


   ;; Sequences
   :elt :length :emptyp :subseq
   :first :last :lastcdr
   :erase :clear

   :map :nmap :map-into :map-to :foreach
   :fill :replace

   :concatenate
   :nconcatenate
   :concatenate-to
   :merge
   :nmerge

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

   :remove-duplicates :delete-duplicates

   ;; Hash-Tables
   :hash-map
   :hash-map-table
   :hash-map-p
   :make-hash-map
   :ensure-hash-map
   :hash-map-test

   :hash
   :get
   :ensure-get

   :hash-map-alist
   :alist-hash-map

   :map-keys
   :map-values

   :alist
   :plist

   ;; Sets
   :memberp :subsetp
   :intersection :nintersection
   :adjoin :nadjoin
   :set-difference :nset-difference
   :set-exclusive-or :nset-exclusive-or
   :union :nunion

   :hash-set
   :make-hash-set
   :hash-table-set
   :hash-set-table
   :hash-set-p

   ;; Miscellaneous
   :defconstant))

(agutil:define-merged-package :generic-cl
    :static-dispatch-cl
  :generic-cl.impl)

(defpackage :generic-cl.math
  (:use :generic-cl)

  (:shadow
   :sin :cos :tan :asin :acos :atan
   :sinh :cosh :tanh :asinh :acosh :atanh
   :exp :expt :log
   :sqrt :isqrt
   :cis :conjugate :phase :realpart :imagpart
   :numerator :denominator :rational :rationalize)

  (:export
   :sin :cos :tan :asin :acos :atan
   :sinh :cosh :tanh :asinh :acosh :atanh
   :exp :expt :log
   :sqrt :isqrt
   :cis :conjugate :phase :realpart :imagpart
   :numerator :denominator :rational :rationalize))

(agutil:define-merged-package :generic-math-cl
    :generic-cl
  :generic-cl.math)

(agutil:define-merged-package :generic-cl-user
    (:internal :cl-user)
  :generic-cl)
