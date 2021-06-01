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

(uiop:define-package :generic-cl.sequence
    (:mix :generic-cl.comparison
          :generic-cl.object
          :generic-cl.container
          :generic-cl.iterator
          :generic-cl.collector
          :generic-cl.sequence
          :generic-cl.map
          :static-dispatch-cl)

  (:use :anaphora
        :arrows
        :cl-custom-hash-table)

  (:import-from :generic-cl.map
                :make-hash-map-table
                :make-generic-hash-table
                :copy-generic-hash-table
                :do-generic-map
                :hash-map-test-p
                :make-empty-hash-table)

  (:shadow
   :map :map-into
   :fill :replace
   :concatenate
   :merge

   :reduce

   :count :count-if :count-if-not
   :find :find-if :find-if-not
   :position :position-if :position-if-not

   :search
   :mismatch

   :every :some :notevery :notany

   :reverse :nreverse
   :sort :stable-sort

   :substitute :substitute-if :substitute-if-not
   :nsubstitute :nsubstitute-if :nsubstitute-if-not
   :remove :remove-if :remove-if-not
   :delete :delete-if :delete-if-not
   :remove-duplicates :delete-duplicates)

  (:export
   :map :nmap :map-into :map-to :foreach
   :map-extend :map-extend-to :map-extend-into
   :fill :replace

   :concatenate
   :nconcatenate
   :concatenate-to
   :merge
   :nmerge

   :reduce

   :count :count-if :count-if-not
   :position :position-if :position-if-not
   :find :find-if :find-if-not
   :find-it :find-it-if :find-it-if-not

   :search
   :mismatch

   :every :some :notevery :notany

   :reverse :nreverse

   :sort :nsort :stable-sort :stable-nsort

   :substitute :substitute-if :substitute-if-not
   :nsubstitute :nsubstitute-if :nsubstitute-if-not

   :remove :remove-if :remove-if-not
   :delete :delete-if :delete-if-not

   :remove-duplicates :delete-duplicates)

  (:documentation
   "Generic sequence operations"))
