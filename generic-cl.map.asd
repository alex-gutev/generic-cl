;;;; generic-cl.map.asd
;;;;
;;;; Copyright 2018-2021 Alexander Gutev
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


(asdf:defsystem #:generic-cl.map
  :description "Generic map and hash-table interface"
  :author "Alexander Gutev"
  :license "MIT"
  :version "1.0"
  :serial t
  :depends-on (#:agutil
               #:anaphora
               #:alexandria
               #:arrows
               #:trivia
               #:static-dispatch
               #:cl-custom-hash-table

               #:generic-cl.internal
               #:generic-cl.comparison
               #:generic-cl.object
               #:generic-cl.container
               #:generic-cl.iterator
               #:generic-cl.collector)

  :components
  ((:module "src/map"
    :serial t
    :components
    ((:file "package")
     (:file "hash-tables")
     (:file "expanders")))))
