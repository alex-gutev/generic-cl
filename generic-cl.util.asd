;;;; generic-cl.util.asd
;;;;
;;;; Copyright 2020 Alexander Gutev
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

(asdf:defsystem #:generic-cl.util
  :description "Utilities implemented on top of GENERIC-CL"
  :author "Alexander Gutev"
  :license "MIT"
  :version "0.2"
  :serial t
  :depends-on (:generic-cl)

  :components ((:module
		"src/util"

		:components
		((:file "package")
		 (:file "lazy-seqs"))))

  :in-order-to ((asdf:test-op (asdf:test-op :generic-cl.util/test))))

(asdf:defsystem #:generic-cl.util/test
  :description "Tests for generic-cl utilities"
  :author "Alexander Gutev"
  :license "MIT"
  :depends-on (:generic-cl.util
	       :fiveam

	       :alexandria
	       :anaphora
	       :arrows)
  :serial t
  :components ((:module
		"test/util"

		:components
		((:file "package")
		 (:file "test")
		 (:file "lazy-seqs"))))

  :perform (asdf:test-op :after (op c)
			 (uiop:symbol-call :generic-cl.util/test :test-generic-cl.util)))
