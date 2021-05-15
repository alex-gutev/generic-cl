;;;; object.lisp
;;;;
;;;; Copyright 2019-2021 Alexander Gutev
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

;;;; Unit tests for the miscellaneous generic object functions.

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite copy-coerce
    :description "Test COPY and COERCE functions"
    :in generic-cl)

(in-suite copy-coerce)


;;; Custom Object Type

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defstruct custom-object
   slot1
   slot2)

 (defmethod equalp ((a custom-object) (b custom-object))
   (and (equalp (custom-object-slot1 a) (custom-object-slot1 b))
	(equalp (custom-object-slot2 a) (custom-object-slot2 b))))

 (defmethod make-load-form ((object custom-object) &optional env)
   (make-load-form-saving-slots object :environment env)))


;;; Test COPY Function

(test list-copy
  "Test COPY on a list"

  (let* ((list '("a" "b" ("c" "d")))
	 (shallow-copy (copy list))
	 (deep-copy (copy list :deep t)))

    (is (not (eq list shallow-copy))
	"Shallow copy not a copy. EQ to original.")

    (is (not (eq list deep-copy))
	"Deep copy not a copy. EQ to original")

    (is (equal '("a" "b" ("c" "d")) shallow-copy))
    (is (equal '("a" "b" ("c" "d")) deep-copy))

    (is-true (cl:every #'eq list shallow-copy))
    (is-true (cl:notany #'eq list deep-copy))))

(test vector-copy
  "Test COPY on vectors"

  (let* ((vector #("a" "b" #("c" "d")))
	 (shallow-copy (copy vector))
	 (deep-copy (copy vector :deep t)))

    (is (not (eq shallow-copy vector))
	"Shallow copy not a copy. EQ to original.")

    (is (not (eq deep-copy vector))
	"Deep copy not a copy. EQ to original.")

    (is (= #("a" "b" #("c" "d")) shallow-copy))
    (is (= #("a" "b" #("c" "d")) deep-copy))

    (is-true (cl:every #'eq vector shallow-copy))
    (is-true (cl:notany #'eq vector deep-copy))))

(test string-copy
  "Test COPY on strings"

  (let* ((str "Hello World")
	 (shallow-copy (copy str))
	 (deep-copy (copy str :deep t)))

    (is (not (eq shallow-copy str))
	"Shallow copy not a copy. EQ to original.")

    (is (not (eq deep-copy str))
	"Deep copy not a copy. EQ to original.")

    (is (equal "Hello World" shallow-copy))
    (is (equal "Hello World" deep-copy))

    (is (eq 'character (array-element-type shallow-copy)))
    (is (eq 'character (array-element-type deep-copy)))

    (is-false (adjustable-array-p shallow-copy))
    (is-false (adjustable-array-p deep-copy))

    (is-false (array-has-fill-pointer-p shallow-copy))
    (is-false (array-has-fill-pointer-p deep-copy))))

(test array-copy
  "Test COPY on adjustable array with fill pointer"

  (let* ((array (make-array 3 :adjustable t :fill-pointer t :initial-contents '("1" "2" "3")))
	 (shallow (copy array))
	 (deep (copy array :deep t)))

    (is (not (eq shallow array))
	"Shallow copy not a copy. EQ to original.")

    (is (not (eq deep array))
	"Deep copy not a copy. EQ to original.")

    (is (= #("1" "2" "3") shallow))
    (is (= #("1" "2" "3") deep))

    (is-true (adjustable-array-p shallow))
    (is-true (adjustable-array-p deep))

    (is-true (array-has-fill-pointer-p shallow))
    (is-true (array-has-fill-pointer-p deep))

    (is-true (cl:every #'eq array shallow))
    (is-true (cl:notany #'eq array deep))))

(test nd-array-copy
  "Test COPY on multi-dimensional arrays"

  (let* ((array (make-array '(2 3) :adjustable t :initial-contents '(("a" "b" "c") ("d" "e" "f"))))
      	 (shallow (copy array))
      	 (deep (copy array :deep t)))

    (is (not (eq shallow array))
	"Shallow copy not a copy. EQ to original.")

    (is (not (eq deep array))
	"Deep copy not a copy. EQ to original.")

    (is (= #2A(("a" "b" "c") ("d" "e" "f")) shallow))
    (is (= #2A(("a" "b" "c") ("d" "e" "f")) deep))

    (is-true (adjustable-array-p shallow))
    (is-true (adjustable-array-p deep))

    (is-true (every #'eq array shallow))
    (is-true (notany #'eq array deep))))

(test hash-map-copy
  "Test COPY on hash-maps"

  (let* ((map (alist-hash-map '((a . "a") (b . "b") (c . "c"))))
	 (shallow (copy map))
	 (deep (copy map :deep t)))

    (is (not (eq shallow map))
	"Shallow copy not a copy. EQ to original.")

    (is (not (eq deep map))
	"Deep copy not a copy. EQ to original.")

    (is (= (alist-hash-map '((a . "a") (b . "b") (c . "c"))) shallow))
    (is (= (alist-hash-map '((a . "a") (b . "b") (c . "c"))) deep))

    (flet ((test-eq (pair)
	     (destructuring-bind (key . value) pair
	       (eq (get key map) value))))

      (is-true (every #'test-eq shallow))
      (is-true (notany #'test-eq deep)))))

(test object-copy
  "Test COPY on user-defined object"

  (let* ((orig (make-custom-object :slot1 1 :slot2 2))
	 (copy (copy orig)))

    (is (not (eq orig copy))
	"Copy not a copy. EQ to original.")

    (is (= #S(custom-object :slot1 1 :slot2 2) copy))))

(test atom-copy
  "Test COPY on atom objects"

  (is (= 1 (copy 1)))
  (is (= 'x (copy 'x)))
  (is (= #\a (copy #\a))))


;;; Test COERCE function

(test coerce-standard
  "Test COERCE on standard types."

  ;; For now simply test built-in type coercions to make sure that the
  ;; default method calls CL:COERCE

  (is (coerce '(1 2 3 4) 'vector) #(1 2 3 4) :test #'equalp)
  (is (coerce #(a b c d) 'list) '(a b c d)))
