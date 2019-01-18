;;;; object.lisp
;;;;
;;;; Copyright 2019 Alexander Gutev
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

(in-package :generic-cl.test)

(plan nil)

(defstruct custom-object
  slot1
  slot2)

(defmethod equalp ((a custom-object) (b custom-object))
  (and (equalp (custom-object-slot1 a) (custom-object-slot1 b))
       (equalp (custom-object-slot2 a) (custom-object-slot2 b))))


(subtest "Test COPY Function"
  (subtest "Test List Copying"
    (let* ((list '("a" "b" ("c" "d")))
	   (shallow-copy (copy list))
	   (deep-copy (copy list :deep t)))

      (isnt shallow-copy list "Shallow copied" :test #'eq)
      (isnt deep-copy list "Deep copied" :test #'eq)
      (is shallow-copy list :test #'equalp "Shallow copied correctly")
      (is deep-copy list :test #'equalp "Deep copied correctly")

      (ok (cl:every #'eq list shallow-copy) "Shallow copy is shallow")
      (ok (cl:notevery #'eq list deep-copy) "Deep copy is deep")))

  (flet ((test-attributes (orig)
	   (let ((copy (copy orig)))
	     (is copy orig :test #'equalp "Copied correctly")
	     (isnt copy orig :test #'eq "Copied")

	     (is (array-element-type copy) (array-element-type orig) "Element type")
	     (is (adjustable-array-p copy) (adjustable-array-p orig) "Adjustable")
	     (is (array-has-fill-pointer-p copy) (array-has-fill-pointer-p orig) "Fill pointer presence")
	     (when (array-has-fill-pointer-p orig)
	       (is (fill-pointer copy) (fill-pointer orig) "Fill pointers equal")))))

    (subtest "Test Vector Copying"
      (let* ((vector #("a" "b" #("c" "d")))
	     (shallow-copy (copy vector))
	     (deep-copy (copy vector :deep t)))

	(isnt shallow-copy vector "Shallow copied" :test #'eq)
	(isnt deep-copy vector "Deep copied" :test #'eq)
	(is shallow-copy vector :test #'equalp "Shallow copied correctly")
	(is deep-copy vector :test #'equalp "Deep copied correctly")

	(ok (cl:every #'eq vector shallow-copy) "Shallow copy is shallow")
	(ok (cl:notevery #'eq vector deep-copy) "Deep copy is deep"))

      (subtest "Test Copying of Attributes"
	(test-attributes "Hello World")
	(test-attributes (make-array 3 :initial-contents '(1 2 3)))
	(test-attributes (make-array 3 :element-type 'integer :initial-contents '(1 2 3)))
	(test-attributes (make-array 3 :element-type 'number :adjustable t :initial-contents '(1 2 3)))
	(test-attributes (make-array 3 :element-type 'integer :adjustable t :fill-pointer t :initial-contents '(1 2 3)))))

    (subtest "Test Multi-Dimensional Array Copying"
      (let* ((array #2A(("a" "b" "c") ("d" "e" "f")))
      	     (shallow-copy (copy array))
      	     (deep-copy (copy array :deep t)))

      	(isnt shallow-copy array "Shallow copied" :test #'eq)
      	(isnt deep-copy array "Deep copied" :test #'eq)

      	(is shallow-copy array :test #'equalp "Shallow copied correctly")
      	(is deep-copy array :test #'equalp "Deep copied correctly")

      	(ok (every #'eq array shallow-copy) "Shallow copy is shallow")
      	(ok (notevery #'eq array deep-copy) "Deep copy is deep"))

      (subtest "Test Copying of Attributes"
	(test-attributes (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))
	(test-attributes (make-array '(2 3) :element-type 'integer :initial-contents '((1 2 3) (4 5 6))))
	(test-attributes (make-array '(2 3) :element-type 'number :adjustable t :initial-contents '((1 2 3) (4 5 6)))))))

  (subtest "Test Hash-Table Copying"
    (let* ((map (alist-hash-map '((a . "a") (b . "b") (c . "c"))))
	   (shallow-copy (copy map))
	   (deep-copy (copy map :deep t)))

      (isnt shallow-copy map "Shallow copied" :test #'eq)
      (isnt deep-copy map "Deep copied" :test #'eq)
      (is shallow-copy map :test #'equalp "Shallow copied correctly")
      (is deep-copy map :test #'equalp "Deep copied correctly")

      (flet ((test-eq (pair)
	       (destructuring-bind (key . value) pair
		 (eq (get key map) value))))

	(ok (every #'test-eq shallow-copy) "Shallow copy is shallow")
	(ok (notevery #'test-eq deep-copy) "Deep copy is deep"))))

  (subtest "Test Custom Object Copying"
    (let* ((orig (make-custom-object :slot1 1 :slot2 2))
	   (copy (copy orig)))
      (isnt orig copy :test #'eq "Copied")
      (is orig copy :test #'equalp "Copied correctly")))

  (subtest "Test Copying Other Objects"
    (is (copy 1) 1)
    (is (copy 'x) 'x)
    (is (copy #\a) #\a))

  (subtest "Test DEFSTRUCT Macro COPY Method Generation"
    (is-expand
     (defstruct custom-object slot1 slot2)
     (progn
       (cl:defstruct custom-object slot1 slot2)
       (defmethod copy (($arg custom-object) &key)
	 (copy-custom-object $arg))
       'custom-object))

    (is-expand
     (defstruct (custom-object (:copier)) slot1 slot2)
     (progn
       (cl:defstruct (custom-object (:copier)) slot1 slot2)
       (defmethod copy (($arg custom-object) &key)
	 (copy-custom-object $arg))
       'custom-object))

    (is-expand
     (defstruct (custom-object (:copier custom-object-copy)) slot1 slot2)
     (progn
       (cl:defstruct (custom-object (:copier custom-object-copy)) slot1 slot2)
       (defmethod copy (($arg custom-object) &key)
	 (custom-object-copy))
       'custom-object))

    (is-expand
     (defstruct (custom-object (:copier nil)) slot1 slot2)
     (progn
       (cl:defstruct (custom-object (:copier nil)) slot1 slot2)
       nil
       'custom-object))))

(subtest "Test COERCE Function"
  ;; For now simply test built-in type coercions to make sure that the
  ;; default method calls CL:COERCE

  (is (coerce '(1 2 3 4) 'vector) #(1 2 3 4) :test #'equalp)
  (is (coerce #(a b c d) 'list) '(a b c d)))

(finalize)
