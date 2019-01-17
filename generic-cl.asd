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
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
	       :anaphora
	       :cl-arrows
               :trivia
               :iterate
               :cl-fad
	       :cl-custom-hash-table

               :agutil
               :static-dispatch)

  :components ((:module
                "src"

                :components
                ((:file "package")
		 (:file "util")
		 (:file "object")
                 (:file "equality")
                 (:file "arithmetic")
		 (:file "comparison")
		 (:file "sequences")
		 (:file "iterator")
		 (:file "collector")
		 (:file "cl-sequences")
		 (:file "hash-tables")
		 (:file "sets")
		 (:file "generic-sequences"))))

  :in-order-to ((asdf:test-op (asdf:test-op :generic-cl.test))))

(asdf:defsystem #:generic-cl.test
  :description "Tests for generic-cl."
  :author "Alexander Gutev"
  :license "MIT"
  :depends-on (:generic-cl :prove :prove-asdf)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module
		"test"

		:components
		((:test-file "package")
		 (:test-file "equality")
		 (:test-file "arithmetic")
		 (:test-file "comparison")
		 (:test-file "iterator")
		 (:test-file "collector")
		 (:test-file "sequences")
		 (:test-file "object")
		 (:test-file "hash-tables"))))

  :perform (asdf:test-op :after (op c)
			 (funcall (intern #.(string :run) :prove) c)))
