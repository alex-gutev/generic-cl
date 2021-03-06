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

(uiop:define-package :generic-cl.lazy-seq
    (:mix :generic-cl.comparison
          :generic-cl.object
          :generic-cl.container
          :generic-cl.iterator
          :generic-cl.collector
          :generic-cl.sequence
          :generic-cl.map
          :static-dispatch-cl)

  (:use :agutil
        :arrows
        :trivia
        :cl-custom-hash-table)

  (:import-from :agutil
                :defmacro!
                :symb)

  (:import-from :generic-cl.map
                :make-hash-map-table
                :make-generic-hash-table
                :copy-generic-hash-table
                :do-generic-map
                :hash-map-test-p
                :make-empty-hash-table)

  (:import-from :generic-cl.sequence
                :advance-all
                :some-endp
                :get-elements
                :make-iters)

  (:export
   :make-lazy-seq
   :lazy-seq
   :lazy-seq-p
   :lazy-seq-head
   :lazy-seq-tail

   :range)

  (:documentation
   "Lazy Sequences"))
