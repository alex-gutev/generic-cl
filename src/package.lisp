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

(uiop:define-package :generic-cl
    (:use)

  (:mix :generic-cl.comparison
        :generic-cl.object
        :generic-cl.arithmetic
        :generic-cl.container
        :generic-cl.iterator
        :generic-cl.collector
        :generic-cl.sequence
        :generic-cl.map
        :generic-cl.set
        :generic-cl.lazy-seq
        :static-dispatch-cl)

  (:reexport :generic-cl.comparison
             :generic-cl.object
             :generic-cl.arithmetic
             :generic-cl.container
             :generic-cl.iterator
             :generic-cl.collector
             :generic-cl.sequence
             :generic-cl.map
             :generic-cl.set
             :generic-cl.lazy-seq
             :static-dispatch-cl)

  (:documentation "Full generic-cl interface"))

(uiop:define-package :generic-math-cl
    (:use)

  (:mix :generic-cl.math
        :generic-cl)

  (:documentation "generic-cl and generic-cl.math interface"))

(agutil:define-merged-package :generic-cl-user
    (:internal :cl-user)
  :generic-cl)
