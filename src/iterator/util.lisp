;;;; util.lisp
;;;;
;;;; Copyright 2021 Alexander Gutev
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

(in-package :generic-cl.iterator)

(define-method-combination subtype ()
  ((methods * :required t))
  (:arguments type)

  "Dispatch based on a type keyword given as a qualifier.

   With this method combination each method includes a single
   qualifier, which is interpreted as a type specifier symbol.

   The method of which the value given in the first argument,
   interpreted as a type specifier, is a subtype of the method's
   qualified type specifier, given in the first qualifier, is called.

   The methods are ordered such that the methods qualified with the
   most derived type, i.e. a type which is a subtype of the others,
   are called first."

  (labels ((type-qualifier (method)
             (first (method-qualifiers method)))

           (type< (t1 t2)
             (subtypep t1 t2))

           (make-if (method else)
             `(if (subtypep ,type ',(type-qualifier method))
                  (call-method ,method)
                  ,else)))

    (-<> (copy-list methods)
         (stable-sort #'type< :key #'type-qualifier)
         (reduce #'make-if <>
                 :from-end t
                 :initial-value
                 `(method-combination-error "No method for type ~a" ,type)))))
