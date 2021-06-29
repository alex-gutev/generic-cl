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

(uiop:define-package :generic-cl.iterator
    (:mix :generic-cl.object
          :generic-cl.container

          :static-dispatch-cl)

  (:use :anaphora
        :arrows
        :trivia
        :tcr.parse-declarations-1.0
        :cl-form-types

        :generic-cl.internal)

  (:import-from :agutil
                :let-if
                :nlet)

  (:import-from :alexandria
                :with-gensyms
                :make-gensym-list
                :mappend
                :ensure-car
                :ensure-list
                :if-let
                :parse-body)

  (:shadow
   :endp)

  (:export
   :iterator
   :make-iterator
   :make-reverse-iterator

   :at
   :start
   :advance
   :advance-n
   :endp

   :doseq
   :doseq!
   :do-sequences
   :do-sequences!
   :doiter
   :doiters

   :with-iterators
   :with-iter-value
   :with-iter-values
   :with-iter-place
   :do-iter-values)

  (:intern :make-doseq
           :subtype
           :with-destructure-pattern
           :iter-macro)

  (:documentation
   "Generic iterator interface"))

(defpackage :generic-cl.iterator.optimization
  (:use)

  (:import-from :generic-cl.iterator
                :make-doseq
                :subtype
                :with-destructure-pattern
                :iter-macro)

  (:export :make-doseq
           :subtype
           :with-destructure-pattern
           :split-declarations-forms
           :iter-macro)

  (:documentation
   "Optimization interface of iterator interface."))
