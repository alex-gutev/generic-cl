;;;; lazy-seq.lisp
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

;;;; Lazy Sequence Tests

(in-package :generic-cl.test)

(plan 3)

(defmacro lseq (&rest items)
  (cl:reduce
   (lambda (item seq)
     `(lazy-seq ,item ,seq))
   items
   :from-end t
   :initial-value nil))

(subtest "Test `LAZY-SEQ's"
  (subtest "Creation"
    (let ((seq (lazy-seq 1 (lazy-seq 2 (lazy-seq 3 nil)))))
      (is (lazy-seq-head seq) 1)

      (is (-> seq
	      lazy-seq-tail
	      funcall
	      lazy-seq-head)
	  2)

      (is (-> seq
	      lazy-seq-tail
	      funcall
	      lazy-seq-tail
	      funcall
	      lazy-seq-head)
	  3)

      (is (-> seq
	      lazy-seq-tail
	      funcall
	      lazy-seq-tail
	      funcall
	      lazy-seq-tail
	      funcall)
	  nil)))

  (subtest "Laziness"
    (let ((times 0))
      (flet ((get-elem (n)
	       (incf times)
	       n))
	(let ((seq (lazy-seq 1 (lazy-seq (get-elem 2) (lazy-seq (get-elem 3) nil)))))
	  (is times 0)

	  (funcall (lazy-seq-tail seq))
	  (is times 1)

	  (funcall (lazy-seq-tail (funcall (lazy-seq-tail seq))))
	  (is times 2))))))

(subtest "Test Implemented Interfaces"
  (labels ((seq (items)
	     (when items
	       (lazy-seq
		(car items)
		(seq (cdr items))))))

    (subtest "Iterator"
      (labels ((test-iter (items &key (start 0) end from-end step)
		 (diag (format nil "Test List: ~s" items))
		 (diag (format nil "Start: ~s, End: ~s, From-end: ~s, Step: ~s" start end from-end step))

		 (test-iter-elements
		  (iterator (seq items) :start start :end end :from-end from-end)
		  (test-sequence items start end from-end)
		  step))

	       (test-subseq (items &key (start 0) end from-end)
		 (diag (format nil "Subseq Test List: ~s" items))
		 (diag (format nil "Start: ~s, End: ~s, From-end: ~s" start end from-end))

		 (let ((test-list (subseq (if from-end (cl:reverse items) items) start end)))
		   (test-iter-elements
		    (subseq (iterator items :from-end from-end) start end)
		    test-list
		    nil)))

	       (test-iter-elements (iter test-list step)
		 ;; Test LENGTH
		 (is (length iter) (cl:length test-list) "(LENGTH ITER)")

		 (loop
		    for cell on test-list
		    by (or (nth (1- (or step 1))
				(list #'cdr #'cddr #'cdddr))
			   #'cdr)

		    for (expected) = cell
		    for got = (start iter) then (at iter)
		    until (endp iter)
		    do
		      (is got expected)
		      (if step
			  (advance-n iter step)
			  (advance iter))

		    finally
		      (ok (endp iter) "(ENDP ITER)")
		      (is cell nil)))

	       (test-sequence (seq start end from-end)
		 (alet (cl:subseq seq start end)
		   (if from-end
		       (cl:reverse it)
		       it))))

	(subtest "Unbounded"
	  (test-iter '(1 2 3 a b c))
	  (test-iter '(1 2 3 a b c) :from-end t)

	  (test-iter '(1 2 3 a b c) :step 2)
	  (test-iter '(1 2 3 a b c) :step 2 :from-end t))

	(subtest "Bounded"
	  (test-iter '(1 2 3 a b c) :start 2)
	  (test-iter '(1 2 3 a b c) :start 2 :end 4)

	  (test-iter '(1 2 3 a b c) :start 2 :from-end t)
	  (test-iter '(1 2 3 a b c) :start 2 :end 4 :from-end t)

	  (test-iter '(1 2 3 a b c) :start 2 :step 3)
	  (test-iter '(1 2 3 a b c) :start 2 :end 4 :step 2)

	  (test-iter '(1 2 3 a b c) :start 2 :from-end t :step 2)
	  (test-iter '(1 2 3 a b c) :start 2 :end 4 :from-end t :step 3))

	(subtest "SUBSEQ on iterator"
	  (test-subseq '(1 2 3 a b c) :start 2)
	  (test-subseq '(1 2 3 a b c) :start 2 :end nil)
	  (test-subseq '(1 2 3 a b c) :end 4)

	  (test-subseq '(1 2 3 a b c) :start 2 :from-end t)
	  (test-subseq '(1 2 3 a b c) :start 2 :end nil :from-end t)
	  (test-subseq '(1 2 3 a b c) :start 2 :end 4 :from-end t))

	(subtest "Single Element"
	  (test-iter '(a))
	  (test-iter '(a) :from-end t)
	  (test-iter '(a) :start 1)
	  (test-iter '(a) :start 1 :from-end t)
	  (test-iter '(a) :start 1 :end 1)
	  (test-iter '(a) :start 1 :end 1 :from-end t)

	  (test-iter '(a))
	  (test-iter '(a) :from-end t :step 2)
	  (test-iter '(a) :start 1 :step 3)
	  (test-iter '(a) :start 1 :from-end t :step 4)
	  (test-iter '(a) :start 1 :end 1 :step 2)
	  (test-iter '(a) :start 1 :end 1 :from-end t :step 2))

	(subtest "Empty List"
	  (test-iter nil))))

    (subtest "Equality"
      (ok (equalp (lseq 1 2 3) (lseq 1 2 3)))
      (ok (equalp (lseq 1 2 3)
		  (lseq 1 (cl:1+ 1) (cl:+ 2 1))))

      (ok (= (lseq (lseq 1 2 3) (lseq 'x) (lseq "Alex" "Bob"))
	     (lseq (lseq 1 (cl:1+ 1) (cl:+ 2 1)) (lseq 'x) (lseq "Alex" "Bob"))))

      (ok (/= (lseq 1 2 3) (lseq 4 5 6)))
      (ok (/= (lseq 1 2 3) (lseq 1 2 4)))
      (ok (/= (lseq 1 2 3) (lseq 1 2 3 4)))
      (ok (/= (lseq 1 2 3) (lseq 1 2))))

    (subtest "Sequence Methods"
      (subtest "CLEARED"
	(is (cleared (lazy-seq 1 (lazy-seq 2 (lazy-seq 3 nil)))) nil))

      (subtest "FIRST"
	(is (first (lazy-seq 1 (lazy-seq 2 (lazy-seq 3)))) 1))

      (subtest "EMPTYP"
	(is (emptyp (lazy-seq 1 nil)) nil)
	(is (emptyp (lazy-seq 1 (lazy-seq 2 nil))) nil)))

    (subtest "Sequence Operations"
      (macrolet ((test-seq-fn ((type expected) form)
		   (with-gensyms (result)
  		     `(let ((,result ,form))
			(is ,result
			    ,(case type
			       (lazy-seq
				`(lseq ,@(map (curry #'list 'quote) expected)))
			       (otherwise
				`',expected))
			    :test #'equalp)
			(is-type ,result ',type
				 ,(format nil "Result is of type: ~a" type))))))

	(subtest "SUBSEQ"
	  (test-seq-fn
	   (lazy-seq (2 3))
	   (subseq (lseq 1 2 3 4 5) 1 3))

	  (test-seq-fn
	   (lazy-seq (3 4 5))
	   (subseq (lseq 1 2 3 4 5) 2)))

	(subtest "SUBSTITUTE Functions"
	  (subtest "SUBSTITUTE"
	    (test-seq-fn
	     (lazy-seq ("a" "b" "new" "c" "new" "d"))
	     (substitute "new" "old" (lseq "a" "b" "old" "c" "old" "d")))

	    (subtest "No Keyword Arguments"
	      (test-seq-fn
	       (lazy-seq (x 2 3 x 4 5 x))
	       (substitute 'x 1 (lseq 1 2 3 1 4 5 1))))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (x 2 3 x 4 5 1))
	       (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 x 4 5 x))
	       (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :count 2 :from-end t)))

	    (subtest "START = 1, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 3 x 4 5 1))
	       (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :start 1 :count 1)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (x 2 3 x 4 5 1))
	       (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (x 2 3 1 4 5 1))
	       (substitute 'x 1 (lseq 1 2 3 1 4 5 1) :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq (x (b 2) (c 3) x (e 4) (f 5) x))
	       (substitute 'x 1 (lseq '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1))
			   :key #'cadr))))

	  (subtest "NSUBSTITUTE"
	    (test-seq-fn
	     (lazy-seq ("a" "b" "new" "c" "new" "d"))
	     (nsubstitute "new" "old" (lseq "a" "b" "old" "c" "old" "d")))

	    (subtest "No Keyword Arguments"
	      (test-seq-fn
	       (lazy-seq (x 2 3 x 4 5 x))
	       (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1))))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (x 2 3 x 4 5 1))
	       (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 x 4 5 x))
	       (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :count 2 :from-end t)))

	    (subtest "START = 1, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 3 x 4 5 1))
	       (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :start 1 :count 1)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (x 2 3 x 4 5 1))
	       (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (x 2 3 1 4 5 1))
	       (nsubstitute 'x 1 (lseq 1 2 3 1 4 5 1) :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq (x (b 2) (c 3) x (e 4) (f 5) x))
	       (nsubstitute 'x 1 (lseq '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1))
			    :key #'cadr))))

	  (subtest "Test SUBSTITUTE-IF"
	    (test-seq-fn
	     (lazy-seq (1 x 3 x 5 x 7 x))
	     (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (1 x 3 x 5 6 7 8))
	       (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8) :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 5 x 7 x))
	       (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :count 2 :from-end t)))

	    (subtest "START = 2"
	      (test-seq-fn
	       (lazy-seq (1 2 3 x 5 x 7 x))
	       (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8) :start 2)))

	    (subtest "START = 2, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 3 x 5 6 7 8))
	       (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :start 2 :count 1)))

	    (subtest "START = 2, END = 6"
	      (test-seq-fn
	       (lazy-seq (1 2 3 x 5 x 7 8))
	       (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :start 2 :end 6)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (1 x 3 x 5 6 7 8))
	       (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 x 3 4 5 6 7 8))
	       (substitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq ((a 1) x (c 3) x (e 5) x (g 7)))
	       (substitute-if 'x #'evenp (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
			      :key #'cadr))))

	  (subtest "Test NSUBSTITUTE-IF"
	    (test-seq-fn
	     (lazy-seq (1 x 3 x 5 x 7 x))
	     (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (1 x 3 x 5 6 7 8))
	       (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8) :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 5 x 7 x))
	       (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			       :count 2 :from-end t)))

	    (subtest "START = 2"
	      (test-seq-fn
	       (lazy-seq (1 2 3 x 5 x 7 x))
	       (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8) :start 2)))

	    (subtest "START = 2, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 3 x 5 6 7 8))
	       (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			       :start 2 :count 1)))

	    (subtest "START = 2, END = 6"
	      (test-seq-fn
	       (lazy-seq (1 2 3 x 5 x 7 8))
	       (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			       :start 2 :end 6)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (1 x 3 x 5 6 7 8))
	       (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			       :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 x 3 4 5 6 7 8))
	       (nsubstitute-if 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
			       :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq ((a 1) x (c 3) x (e 5) x (g 7)))
	       (nsubstitute-if 'x #'evenp (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
			       :key #'cadr))))

	  (subtest "Test SUBSTITUTE-IF-NOT"
	    (test-seq-fn
	     (lazy-seq (x 2 x 4 x 6 x 8))
	     (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (x 2 x 4 5 6 7 8))
	       (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				  :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 x 6 x 8))
	       (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				  :count 2 :from-end t)))

	    (subtest "START = 2"
	      (test-seq-fn
	       (lazy-seq (1 2 x 4 x 6 x 8))
	       (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				  :start 2)))

	    (subtest "START = 2, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 x 4 5 6 7 8))
	       (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				  :start 2 :count 1)))

	    (subtest "COUNT = 2, END = 6"
	      (test-seq-fn
	       (lazy-seq (1 2 x 4 x 6 7 8))
	       (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				  :start 2 :end 6)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (x 2 x 4 x 6 7 8))
	       (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				  :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (x 2 3 4 5 6 7 8))
	       (substitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				  :count 1 :end 5)))

	    (subtest "KEY #'CADR"
	      (test-seq-fn
	       (lazy-seq (x (b 2) x (d 4) x (f 6) x))
	       (substitute-if-not 'x #'evenp
				  (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
				  :key #'cadr))))

	  (subtest "Test NSUBSTITUTE-IF-NOT"
	    (test-seq-fn
	     (lazy-seq (x 2 x 4 x 6 x 8))
	     (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (x 2 x 4 5 6 7 8))
	       (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				   :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 x 6 x 8))
	       (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				   :count 2 :from-end t)))

	    (subtest "START = 2"
	      (test-seq-fn
	       (lazy-seq (1 2 x 4 x 6 x 8))
	       (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				   :start 2)))

	    (subtest "START = 2, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 x 4 5 6 7 8))
	       (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				   :start 2 :count 1)))

	    (subtest "COUNT = 2, END = 6"
	      (test-seq-fn
	       (lazy-seq (1 2 x 4 x 6 7 8))
	       (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				   :start 2 :end 6)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (x 2 x 4 x 6 7 8))
	       (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				   :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (x 2 3 4 5 6 7 8))
	       (nsubstitute-if-not 'x #'evenp (lseq 1 2 3 4 5 6 7 8)
				   :count 1 :end 5)))

	    (subtest "KEY #'CADR"
	      (test-seq-fn
	       (lazy-seq (x (b 2) x (d 4) x (f 6) x))
	       (nsubstitute-if-not 'x #'evenp
				   (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
				   :key #'cadr)))))

	(subtest "REMOVE Functions"
	  (subtest "REMOVE"
	    (subtest "String Elements"
	      (test-seq-fn
	       (lazy-seq ("a" "b" "c" "d"))
	       (remove "old" (lseq "a" "b" "old" "c" "old" "d"))))

	    (subtest "Number Elements"
	      (test-seq-fn
	       (lazy-seq (2 3 4 5))
	       (remove 1 (lseq 1 2 3 1 4 5 1))))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (2 3 4 5 1))
	       (remove 1 (lseq 1 2 3 1 4 5 1) :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 5))
	       (remove 1 (lseq 1 2 3 1 4 5 1)
		       :count 2 :from-end t)))

	    (subtest "START = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 3 4 5))
	       (remove 1 (lseq 1 2 3 1 4 5 1) :start 1)))

	    (subtest "START = 1, END = 5"
	      (test-seq-fn
	       (lazy-seq (1 2 3 4 5 1))
	       (remove 1 (lseq 1 2 3 1 4 5 1)
		       :start 1 :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (2 3 1 4 5 1))
	       (remove 1 (lseq 1 2 3 1 4 5 1)
		       :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq ((b 2) (c 3) (e 4) (f 5)))
	       (remove 1 (lseq '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1))
		       :key #'cadr))))

	  (subtest "DELETE"
	    (subtest "String Elements"
	      (test-seq-fn
	       (lazy-seq ("a" "b" "c" "d"))
	       (delete "old" (lseq "a" "b" "old" "c" "old" "d"))))

	    (subtest "Number Elements"
	      (test-seq-fn
	       (lazy-seq (2 3 4 5))
	       (delete 1 (lseq 1 2 3 1 4 5 1))))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (2 3 4 5 1))
	       (delete 1 (lseq 1 2 3 1 4 5 1) :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 5))
	       (delete 1 (lseq 1 2 3 1 4 5 1)
		       :count 2 :from-end t)))

	    (subtest "START = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 3 4 5))
	       (delete 1 (lseq 1 2 3 1 4 5 1) :start 1)))

	    (subtest "START = 1, END = 5"
	      (test-seq-fn
	       (lazy-seq (1 2 3 4 5 1))
	       (delete 1 (lseq 1 2 3 1 4 5 1)
		       :start 1 :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (2 3 1 4 5 1))
	       (delete 1 (lseq 1 2 3 1 4 5 1)
		       :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq ((b 2) (c 3) (e 4) (f 5)))
	       (delete 1 (lseq '(a 1) '(b 2) '(c 3) '(d 1) '(e 4) '(f 5) '(g 1))
		       :key #'cadr))))

	  (subtest "REMOVE-IF"
	    (test-seq-fn
	     (lazy-seq (1 3 5 7))
	     (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (1 3 5 6 7 8))
	       (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 5 7))
	       (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :count 2 :from-end t)))

	    (subtest "START = 2"
	      (test-seq-fn
	       (lazy-seq (1 2 3 5 7))
	       (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2)))

	    (subtest "START = 2, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 3 5 6 7 8))
	       (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2 :count 1)))

	    (subtest "START = 2, END = 6"
	      (test-seq-fn
	       (lazy-seq (1 2 3 5 7 8))
	       (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2 :end 6)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (1 3 5 6 7 8))
	       (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 3 4 5 6 7 8))
	       (remove-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq ((a 1) (c 3) (e 5) (g 7)))
	       (remove-if #'evenp (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
			  :key #'cadr))))

	  (subtest "DELETE-IF"
	    (test-seq-fn
	     (lazy-seq (1 3 5 7))
	     (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (1 3 5 6 7 8))
	       (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 5 7))
	       (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :count 2 :from-end t)))

	    (subtest "START = 2"
	      (test-seq-fn
	       (lazy-seq (1 2 3 5 7))
	       (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2)))

	    (subtest "START = 2, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 3 5 6 7 8))
	       (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2 :count 1)))

	    (subtest "START = 2, END = 6"
	      (test-seq-fn
	       (lazy-seq (1 2 3 5 7 8))
	       (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :start 2 :end 6)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (1 3 5 6 7 8))
	       (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 3 4 5 6 7 8))
	       (delete-if #'evenp (lseq 1 2 3 4 5 6 7 8)
			  :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq ((a 1) (c 3) (e 5) (g 7)))
	       (delete-if #'evenp (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
			  :key #'cadr))))

	  (subtest "REMOVE-IF-NOT"
	    (test-seq-fn
	     (lazy-seq (2 4 6 8))
	     (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (2 4 5 6 7 8))
	       (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 6 8))
	       (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :count 2 :from-end t)))

	    (subtest "START = 2"
	      (test-seq-fn
	       (lazy-seq (1 2 4 6 8))
	       (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :start 2)))

	    (subtest "START = 2, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 4 5 6 7 8))
	       (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :start 2 :count 1)))

	    (subtest "START = 2, END = 6"
	      (test-seq-fn
	       (lazy-seq (1 2 4 6 7 8))
	       (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :start 2 :end 6)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (2 4 6 7 8))
	       (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (2 3 4 5 6 7 8))
	       (remove-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq ((b 2) (d 4) (f 6)))
	       (remove-if-not #'evenp
			      (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
			      :key #'cadr))))

	  (subtest "DELETE-IF-NOT"
	    (test-seq-fn
	     (lazy-seq (2 4 6 8))
	     (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)))

	    (subtest "COUNT = 2"
	      (test-seq-fn
	       (lazy-seq (2 4 5 6 7 8))
	       (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :count 2)))

	    (subtest "COUNT = 2, FROM-END = T"
	      (test-seq-fn
	       (list (1 2 3 4 6 8))
	       (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :count 2 :from-end t)))

	    (subtest "START = 2"
	      (test-seq-fn
	       (lazy-seq (1 2 4 6 8))
	       (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :start 2)))

	    (subtest "START = 2, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (1 2 4 5 6 7 8))
	       (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :start 2 :count 1)))

	    (subtest "START = 2, END = 6"
	      (test-seq-fn
	       (lazy-seq (1 2 4 6 7 8))
	       (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :start 2 :end 6)))

	    (subtest "END = 5"
	      (test-seq-fn
	       (lazy-seq (2 4 6 7 8))
	       (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :end 5)))

	    (subtest "END = 5, COUNT = 1"
	      (test-seq-fn
	       (lazy-seq (2 3 4 5 6 7 8))
	       (delete-if-not #'evenp (lseq 1 2 3 4 5 6 7 8)
			      :end 5 :count 1)))

	    (subtest "KEY = #'CADR"
	      (test-seq-fn
	       (lazy-seq ((b 2) (d 4) (f 6)))
	       (delete-if-not #'evenp
			      (lseq '(a 1) '(b 2) '(c 3) '(d 4) '(e 5) '(f 6) '(g 7))
			      :key #'cadr)))))

	(subtest "REMOVE-DUPLICATES Functions"
	  (subtest "REMOVE-DUPLICATES"
	    (test-seq-fn
	     (list ("bob" "alex" "jack" "john"))
	     (remove-duplicates
	      (lseq "alex" "john" "bob" "alex" "jack" "john")))

	    (subtest "FROM-END = t"
	      (test-seq-fn
	       (lazy-seq ("alex" "john" "bob" "jack"))
	       (remove-duplicates
		(lseq "alex" "john" "bob" "alex" "jack" "john")
		:from-end t)))

	    (subtest "START = 1, END = 4, FROM-END = t"
	      (test-seq-fn
	       (lazy-seq ("alex" "john" "bob" "alex" "jack"))
	       (remove-duplicates
		(lseq "alex" "john" "bob" "alex" "jack" "john")
		:from-end t :start 1)))

	    (subtest "START = 1, END = 4, FROM-END = t"
	      (test-seq-fn
	       (lazy-seq ("alex" "john" "bob" "alex" "jack" "john"))
	       (remove-duplicates
		(lseq "alex" "john" "bob" "alex" "jack" "john")
		:from-end t :start 1 :end 4)))

	    (subtest "KEY = #'CADR, FROM-END = T"
	      (test-seq-fn
	       (lazy-seq ((a 1) (b 2) (d 3) (f 4)))
	       (remove-duplicates
		(lseq '(a 1) '(b 2) '(c 1) '(d 3) '(e 2) '(f 4))
		:from-end t :key #'cadr)))

	    (subtest "FROM-END = T, TEST = #'CHAR-EQUAL"
	      (test-seq-fn
	       (lazy-seq (#\a #\B #\c #\D))
	       (remove-duplicates
	    	(lseq #\a #\B #\c #\D #\A #\b #\C #\d)
	    	:from-end t :test #'char-equal))))

	  (subtest "DELETE-DUPLICATES"
	    (test-seq-fn
	     (list ("bob" "alex" "jack" "john"))
	     (remove-duplicates
	      (lseq "alex" "john" "bob" "alex" "jack" "john")))

	    (subtest "FROM-END = t"
	      (test-seq-fn
	       (lazy-seq ("alex" "john" "bob" "jack"))
	       (remove-duplicates
		(lseq "alex" "john" "bob" "alex" "jack" "john")
		:from-end t)))

	    (subtest "START = 1, END = 4, FROM-END = t"
	      (test-seq-fn
	       (lazy-seq ("alex" "john" "bob" "alex" "jack"))
	       (remove-duplicates
		(lseq "alex" "john" "bob" "alex" "jack" "john")
		:from-end t :start 1)))

	    (subtest "START = 1, END = 4, FROM-END = t"
	      (test-seq-fn
	       (lazy-seq ("alex" "john" "bob" "alex" "jack" "john"))
	       (remove-duplicates
		(lseq "alex" "john" "bob" "alex" "jack" "john")
		:from-end t :start 1 :end 4)))

	    (subtest "KEY = #'CADR, FROM-END = T"
	      (test-seq-fn
	       (lazy-seq ((a 1) (b 2) (d 3) (f 4)))
	       (remove-duplicates
		(lseq '(a 1) '(b 2) '(c 1) '(d 3) '(e 2) '(f 4))
		:from-end t :key #'cadr)))

	    (subtest "FROM-END = T, TEST = #'CHAR-EQUAL"
	      (test-seq-fn
	       (lazy-seq (#\a #\B #\c #\D))
	       (remove-duplicates
	    	(lseq #\a #\B #\c #\D #\A #\b #\C #\d)
	    	:from-end t :test #'char-equal)))))))

    (subtest "COERCE Methods"
      (subtest "Coerce to list"
	(is (coerce (lseq 1 2 3 4 5 6) 'list)
	    '(1 2 3 4 5 6))))

    (subtest "COPY Method"
      (subtest "Shallow Copy"
	(let* ((seq (lseq '(1) '(2) '(3)))
	       (copy (copy seq)))

	  (is-type copy 'lazy-seq "Result is a LAZY-SEQ")

	  (ok (every #'eq seq copy) "Elements Equal")
	  (is (length seq) (length copy) "Same Length")))

      (subtest "Deep Copy"
	(let* ((seq (lseq '(1) '(2) '(3)))
	       (copy (copy seq :deep t)))

	  (is-type copy 'lazy-seq "Result is a LAZY-SEQ")

	  (ok (every
	       (lambda (o c)
		 (and (= o c)
		      (not (eq o c))))
	       seq copy)
	      "Elements equal but not identical")

	  (is (length seq) (length copy) "Same Length"))))))

(subtest "Test Utilities"
  (subtest "RANGE"
    (subtest "Unbound Range"
      (ok (every #'= (range 0) '(0 1 2 3 4 5))))

    (subtest "Unbound Range with Step"
      (ok (every #'= (range 0 nil 2) '(0 2 4 6 8 10))))

    (subtest "Bound Range"
      (is (coerce (range 0 6) 'list) '(0 1 2 3 4 5) :test #'equalp))

    (subtest "Bound Range with Step"
      (is (coerce (range 0 11 2) 'list) '(0 2 4 6 8 10) :test #'equalp))))

(finalize)
