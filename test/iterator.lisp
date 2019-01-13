;;;; iterator.lisp
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

;;;; Unit tests for iterator interface

(in-package :generic-cl.test)

(plan nil)

(subtest "Test Iterator Interface"
  (labels
      ((test-list-iter (list &key (start 0) end from-end &aux (test-list (test-sequence list start end from-end)))
	 (diag (format nil "Test List: ~s" list))
	 (diag (format nil "Start: ~s, End: ~s, From-end: ~s" start end from-end))

	 (let ((iter (iterator list :start start :end end :from-end from-end)))
	   ;; Test LENGTH
	   (is (length iter) (cl:length test-list) "(LENGTH ITER)")

	   (loop
	      for (expected . rest) on test-list
	      for got = (start iter) then (at iter)
	      until (endp iter)
	      do
		(is got expected)
		(advance iter)
	      finally
		(ok (endp iter) "(ENDP ITER)")
		(is rest nil))))

       (test-vec-iter (vec &key (start 0) end from-end &aux (test-vec (test-sequence vec start end from-end)))
	 (diag (format nil "Test Vector: ~s" vec))
	 (diag (format nil "Start: ~s, End: ~s, From-end: ~s" start end from-end))

	 (let ((iter (iterator vec :start start :end end :from-end from-end)))
	   ;; Test LENGTH
	   (is (length iter) (cl:length test-vec) "(LENGTH ITER)")

	   (loop
	      with iter = (iterator vec :start start :end end :from-end from-end)
	      with i = 0
	      for expected across test-vec
	      for got = (start iter) then (at iter)
	      until (endp iter)
	      do
		(is got expected)
		(advance iter)
		(incf i)
	      finally
		(ok (endp iter) "(ENDP ITER)")
		(is i (cl:length test-vec)))))

       (test-sequence (seq start end from-end)
	 (alet (cl:subseq seq start end)
	   (if from-end
	       (cl:reverse it)
	       it)))

       (test-array-iter (arr &key (start 0) end from-end &aux (test-arr (test-array arr start end from-end)))
	 (diag (format nil "Test array: ~s" arr))
	 (diag (format nil "Start: ~s, End: ~s, From-end: ~s" start end from-end))

	 (let ((iter (iterator arr :start start :end end :from-end from-end)))
	   ;; Test LENGTH
	   (is (length iter) (cl:length test-arr) "(LENGTH ITER)")

	   (loop
	      with i = 0
	      for expected across test-arr
	      for got = (start iter) then (at iter)
	      until (endp iter)
	      do
		(is got expected)
		(advance iter)
		(incf i)
	      finally
		(ok (endp iter) "(ENDP ITER)")
		(is i (cl:length test-arr)))))

       (test-array (array start end from-end)
	 (alet
	     (make-array (- (or end (array-total-size array)) start)
			 :displaced-to array
			 :displaced-index-offset start)
	   (if from-end
	       (cl:reverse it)
	       it)))

       (test-hash-iter (hash &key (start 0) end from-end)
	 (diag (format nil "Test Hash-Table: ~s" (hash-map-alist hash)))
	 (diag (format nil "Start: ~s, End: ~s, From-end: ~s" start end from-end))

	 (let ((iter (iterator hash :start start :end end :from-end from-end))
	       (count (- (or end (length hash)) start)))
	   ;; Test LENGTH
	   (is (length iter) count "(LENGTH ITER)")

	   (loop
	      with i = 0
	      for (key . value) = (start iter) then (at iter)
	      until (endp iter)
	      do
		(is value (get key hash))
		(advance iter)
		(incf i)
	      finally
		(ok (endp iter) "(ENDP ITER)")
		(is i count))))

       (test-set-element (seq index value expected &rest args)
	 (let ((seq (copy seq)))

	   (diag (format nil "Test Sequence: ~s" seq))
	   (diag (format nil "Iterator Arguments: ~a" args))
	   (diag (format nil "Set element ~a to ~s" index value))

	   (set-element (apply #'iterator seq args) index value)

	   (is seq expected :test #'equalp)))

       (copy (seq)
	 (typecase seq
	   (list (copy-list seq))
	   (array (copy-array seq))
	   (otherwise (copy-seq seq))))

       (set-element (it index value)
	 (loop
	    for i below index
	    do
	      (advance it)
	    finally (setf (at it) value))))

    (subtest "List iterator"
      (test-list-iter '(1 2 3 a b c))
      (test-list-iter '(1 2 3 a b c) :from-end t)

      (test-list-iter '(1 2 3 a b c) :start 2)
      (test-list-iter '(1 2 3 a b c) :start 2 :end 4)

      (test-list-iter '(1 2 3 a b c) :start 2 :from-end t)
      (test-list-iter '(1 2 3 a b c) :start 2 :end 4 :from-end t)

      (test-list-iter '(a))
      (test-list-iter '(a) :from-end t)
      (test-list-iter '(a) :start 1)
      (test-list-iter '(a) :start 1 :from-end t)
      (test-list-iter '(a) :start 1 :end 1)
      (test-list-iter '(a) :start 1 :end 1 :from-end t)

      (test-list-iter nil)

      (subtest "Modifying Elements"
	(test-set-element '(1 2 3 4) 2 'x '(1 2 x 4))
	(test-set-element '(1 2 3 4) 2 'x '(1 x 3 4) :from-end t)
	(test-set-element '(1 2 3 4) 1 'y '(1 2 y 4) :start 1)
	(test-set-element '(1 2 3 4) 2 'y '(1 y 3 4) :start 1 :from-end t)
	(test-set-element '(1 2 3 4) 1 'z '(1 2 z 4) :start 1 :end 3)
	(test-set-element '(1 2 3 4) 1 'z '(1 z 3 4) :start 1 :end 3 :from-end t)))

    (subtest "Vector Iterator"
      (test-vec-iter #(1 2 3 a b c))
      (test-vec-iter #(1 2 3 a b c) :from-end t)

      (test-vec-iter #(1 2 3 a b c) :start 1 :end 3)
      (test-vec-iter #(1 2 3 a b c) :from-end t :start 1 :end 3)

      (test-vec-iter #(1))
      (test-vec-iter #(1) :from-end t)
      (test-vec-iter #(1) :start 1)
      (test-vec-iter #(1) :start 1 :end 1)
      (test-vec-iter #(1) :start 1 :end 1 :from-end t)

      (test-vec-iter #())

      (diag "Other Vector Types:")

      (test-vec-iter (vector 1 2 3 4 5 6))
      (test-vec-iter (make-array 4 :initial-contents '(a b c d)))
      (test-vec-iter (make-array 4 :adjustable t :fill-pointer t :initial-contents '(a b c d)))
      (test-vec-iter (make-array 4 :element-type 'integer :adjustable t :fill-pointer t :initial-contents '(1 2 3 4)))
      (test-vec-iter "Hello World")
      (test-vec-iter #*10111011)

      (subtest "Modifying Elements"
	(test-set-element #(1 2 3 4) 2 'x #(1 2 x 4))
	(test-set-element #(1 2 3 4) 2 'x #(1 x 3 4) :from-end t)
	(test-set-element #(1 2 3 4) 1 'y #(1 2 y 4) :start 1)
	(test-set-element #(1 2 3 4) 2 'y #(1 y 3 4) :start 1 :from-end t)
	(test-set-element #(1 2 3 4) 1 'z #(1 2 z 4) :start 1 :end 3)
	(test-set-element #(1 2 3 4) 1 'z #(1 z 3 4) :start 1 :end 3 :from-end t)))

    (subtest "Multi-Dimensional Array Iterator"
      (test-array-iter #2A((1 2 3) (4 5 6)))
      (test-array-iter #2A((1 2 3) (4 5 6)) :from-end t)
      (test-array-iter #2A((1 2 3) (4 5 6)) :start 2 :end 5)
      (test-array-iter #2A((1 2 3) (4 5 6)) :start 2 :end 5 :from-end t)

      (subtest "Modifying Elements"
	(test-set-element #2A((1 2) (3 4)) 1 'x #2A((1 x) (3 4)))
	(test-set-element #2A((1 2) (3 4)) 1 'x #2A((1 2) (x 4)) :from-end t)
	(test-set-element #2A((1 2) (3 4)) 2 'x #2A((1 2) (3 x)) :start 1)
	(test-set-element #2A((1 2) (3 4)) 2 'x #2A((1 x) (3 4)) :start 1 :from-end t)
	(test-set-element #2A((1 2) (3 4)) 1 'x #2A((1 2) (x 4)) :start 1 :end 3)
	(test-set-element #2A((1 2) (3 4)) 1 'x #2A((1 x) (3 4)) :start 1 :end 3 :from-end t)))

    (subtest "Hash-Table Iterator"
      (test-hash-iter (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))))
      (test-hash-iter (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :from-end t)

      (test-hash-iter (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :start 1 :end 3)
      (test-hash-iter (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))) :start 1 :end 3 :from-end t)

      (test-hash-iter (make-hash-map))
      (test-hash-iter (make-hash-map) :from-end t)

      (subtest "Modifying Elements"
	(let* ((hash (alist-hash-map '((a . 1) (b . 2) (c . 3))))
	       (it (iterator hash)))
	  (advance it)
	  (setf (at it) 'x)
	  (is (get (car (at it)) hash) 'x))

	(let* ((hash (alist-hash-map '((a . 1) (b . 2) (c . 3) (d . 4))))
	       (it (iterator hash :start 1 :end 3)))
	  (advance it)
	  (setf (at it) 'x)
	  (is (get (car (at it)) hash) 'x))))

    (subtest "DOSEQ Macro"
      (let ((list '(1 2 3 4)))
	(doseq (elem list)
	  (is elem (car list))
	  (setf list (cdr list)))
	(is list nil))

      (let* ((list '(1 2 3 4))
	     (rlist (cl:reverse list)))
	(doseq (elem list :from-end t)
	  (is elem (car rlist))
	  (setf rlist (cdr rlist)))
	(is rlist nil))

      (let* ((list '(1 2 3 4 5))
	     (test-list (test-sequence list 1 4 nil)))
	(doseq (elem list :start 1 :end 4)
	  (is elem (car test-list))
	  (setf test-list (cdr test-list)))
	(is test-list nil)))))

(finalize)
