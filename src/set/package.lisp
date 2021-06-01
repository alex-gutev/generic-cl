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

(uiop:define-package :generic-cl.set
    (:mix :generic-cl.comparison
          :generic-cl.object
          :generic-cl.container
          :generic-cl.iterator
          :generic-cl.collector
          :generic-cl.sequence
          :generic-cl.map

          :alexandria
          :static-dispatch-cl)

  (:use :cl-environments.tools
        :agutil

        :anaphora
        :arrows
        :trivia
	:cl-custom-hash-table)

  (:import-from :agutil
                :defmacro!
                :symb)

  (:import-from :generic-cl.iterator
                :list-iterator
                :make-list-iterator)

  (:import-from :generic-cl.map
                :make-hash-map-table
                :make-generic-hash-table
                :copy-generic-hash-table
                :do-generic-map
                :hash-map-test-p
                :make-empty-hash-table)

  (:shadow
   :subsetp
   :intersection :nintersection
   :adjoin
   :set-difference :nset-difference
   :set-exclusive-or :nset-exclusive-or
   :union :nunion)

  (:export
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
   :hash-set-p)

  (:documentation
   "Generic set interface"))
