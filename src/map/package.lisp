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

(uiop:define-package :generic-cl.map
    (:mix :generic-cl.comparison
          :generic-cl.object
          :generic-cl.container
          :generic-cl.iterator
          :generic-cl.collector
          :static-dispatch-cl)

  (:use :anaphora
        :trivia
        :cl-custom-hash-table)

  (:import-from :agutil
                :defmacro!
                :symb)

  (:import-from :generic-cl.iterator
                :list-iterator
                :make-list-iterator)

  (:shadow :get)

  (:export
   :hash-map
   :hash-map-table
   :hash-map-p
   :make-hash-map
   :ensure-hash-map
   :hash-map-test

   :hash
   :like-hash
   :get
   :ensure-get

   :hash-map-alist
   :alist-hash-map

   :map-keys
   :map-values

   :alist
   :plist)

  (:documentation
   "Generic map and hash-table interface"))
