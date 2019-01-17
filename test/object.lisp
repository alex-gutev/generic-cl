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

(subtest "Test COPY Function"
  (subtest "Test List Copying"
    (let* ((list '("a" "b" ("c" "d")))
	   (shallow-copy (copy list))
	   (deep-copy (copy list :deep t)))

      (isnt shallow-copy list "Shallow Copied" :test #'eq)
      (isnt deep-copy list "Deep Copied" :test #'eq)
      (is shallow-copy list :test #'equalp "Shallow copied correctly")
      (is deep-copy list :test #'equalp "Deep copied correctly")

      (ok (cl:every #'eq list shallow-copy) "Shallow copy is shallow.")
      (ok (cl:notevery #'eq list deep-copy) "Deep copy is deep.")))

  (subtest "Test Vector Copying"
    (let* ((vector #("a" "b" #("c" "d")))
	   (shallow-copy (copy vector))
	   (deep-copy (copy vector :deep t)))

      (isnt shallow-copy vector "Shallow Copied" :test #'eq)
      (isnt deep-copy vector "Deep Copied" :test #'eq)
      (is shallow-copy vector :test #'equalp "Shallow copied correctly")
      (is deep-copy vector :test #'equalp "Deep copied correctly")

      (ok (cl:every #'eq vector shallow-copy) "Shallow copy is shallow.")
      (ok (cl:notevery #'eq vector deep-copy) "Deep copy is deep.")))

  (subtest "Test Hash-Table Copying"
    (let* ((map (alist-hash-map '((a . "a") (b . "b") (c . "c"))))
	   (shallow-copy (copy map))
	   (deep-copy (copy map :deep t)))

      (isnt shallow-copy map "Shallow Copied" :test #'eq)
      (isnt deep-copy map "Deep Copied" :test #'eq)
      (is shallow-copy map :test #'equalp "Shallow copied correctly")
      (is deep-copy map :test #'equalp "Deep copied correctly")

      (flet ((test-eq (pair)
	       (destructuring-bind (key . value) pair
		 (eq (get key map) value))))

	(ok (every #'test-eq shallow-copy) "Shallow copy is shallow")
	(ok (notevery #'test-eq deep-copy) "Deep copy is deep"))))

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

(finalize)
