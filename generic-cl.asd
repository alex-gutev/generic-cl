;;;; generic-cl.asd
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


(asdf:defsystem #:generic-cl
  :description "Standard Common Lisp functions implemented using generic functions."
  :author "Alexander Gutev"
  :license "MIT"
  :version "0.7.1"
  :serial t
  :depends-on (:agutil
               :alexandria
               :anaphora
               :arrows
               :cl-custom-hash-table
               :static-dispatch
               :trivia

               #:generic-cl.comparison
               #:generic-cl.object
               #:generic-cl.arithmetic
               #:generic-cl.container
               #:generic-cl.iterator)

  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "sequences")
     (:file "collector")
     (:file "hash-tables")
     (:file "cl-sequences" :depends-on ("hash-tables"))
     (:file "sets")
     (:file "generic-sequences")
     (:file "lazy-seq")
     (:file "math"))))

  :in-order-to ((asdf:test-op (asdf:test-op :generic-cl/test))))

(asdf:defsystem #:generic-cl/test
  :description "Tests for generic-cl."
  :author "Alexander Gutev"
  :license "MIT"
  :depends-on (:generic-cl :fiveam)
  :serial t
  :components
  ((:module "test"
	    :serial t
            :components
            ((:file "package")
	     (:file "test")
             (:file "equality")
             (:file "arithmetic")
             (:file "comparison")
             (:file "iterator")
             (:file "collector")
             (:file "sequences")
             (:file "object")
             (:file "hash-tables")
             (:file "sets")
             (:file "lazy-seq")
             (:file "misc"))))

  :perform (asdf:test-op :after (op c)
                         (uiop:symbol-call :generic-cl/test :test-generic-cl)))
