;;;; generic-sequences.lisp
;;;;
;;;; Copyright 2018-2019 Alexander Gutev
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

(in-package :generic-cl.impl)

;;; Generic methods based on iterators

(defmethod length (sequence)
  "Returns the number of elements in SEQUENCE, where SEQUENCE is of a
   type for which the iterator interface is implemented.

   The type is computed by iterating through the entire sequence, thus
   this is a linear O(n) operation, in the number of elements in the
   sequence."

  (loop
     with iter = (iterator sequence)
     for count = 0 then (1+ count)
     until (endp iter)
     do
       (advance iter)
     finally (return count)))

(defmethod reduce (fn sequence &key key from-end (start 0) end (initial-value nil init-sp))
  (let ((key (or key #'identity))
	(f (if from-end (lambda (x y) (funcall fn y x)) fn)) ; Flip function arguments if FROM-END is T
	(iter (iterator sequence :start start :end end :from-end from-end)))

    (flet ((reduce-seq (res)
	     (advance iter)
	     (loop
		with elem
		with res = res
		until (endp iter)
		do
		  (setf elem (current iter))
		  (setf res (funcall f res elem))
		  (advance iter)
		finally (return res))))

      (if (endp iter) ; If sequence is empty
	  ;; Return INITIAL-VALUE if supplied or call FN with no arguments
	  (if init-sp initial-value (funcall fn))

	  (let ((elem (funcall key (current iter))))
	    (reduce-seq (if init-sp (funcall f initial-value elem) elem)))))))


(defmethod count (item sequence &key from-end (start 0) end key (test #'equalp))
  (count-if (test-eq test item) sequence :from-end from-end :start start :end end :key key))

(defmethod count-if (test sequence &key from-end (start 0) end key)
  (let ((key (or key #'identity))
	(count 0))
    (doseq (elem sequence :from-end from-end :start start :end end)
      (when (funcall test (funcall key elem))
	(incf count)))
    count))

(defmethod count-if-not (test sequence &key from-end (start 0) end key)
  (count-if (test-not test) sequence
	    :from-end from-end
	    :start start
	    :end end
	    :key key))


(defmethod find (item sequence &key from-end (start 0) end (test #'equalp) key)
  (find-if (test-eq test item) sequence :from-end from-end :start start :end end :key key))

(defmethod find-if (test sequence &key from-end (start 0) end key)
  (let ((key (or key #'identity)))
   (doseq (elem sequence :from-end from-end :start start :end end)
     (when (funcall test (funcall key elem))
       (return elem)))))

(defmethod find-if-not (test sequence &key from-end (start 0) end key)
  (find-if (test-not test) sequence :from-end from-end :start start :end end :key key))


(defmethod position (item sequence &key from-end (start 0) end (test #'equalp) key)
  (position-if (test-eq test item) sequence :from-end from-end :start start :end end :key key))

(defmethod position-if (test sequence &key from-end (start 0) end key)
  (flet ((compute-pos (pos)
	   (+ start
	      (if from-end
		  (- (or end (length sequence)) pos)
		  pos))))
    (let ((key (or key #'identity))
	  (pos 0))
      (doseq (elem sequence :from-end from-end :start start :end end)
	(when (funcall test (funcall key elem))
	  (return (compute-pos pos)))
	(incf pos)))))

(defmethod position-if-not (test sequence &key from-end (start 0) end key)
  (position-if (test-not test) sequence :from-end from-end :start start :end end :key key))


(defmethod mismatch (seq1 seq2 &key from-end (test #'equalp) key (start1 0) (start2 0) end1 end2)
  (flet ((compute-pos (pos)
	   (if from-end
	       (- (or end1 (length seq1)) pos)
	       pos)))
    (let ((key (or key #'identity)))
      (loop
	 with it1 = (iterator seq1 :start start1 :end end1 :from-end from-end)
	 with it2 = (iterator seq2 :start start2 :end end2 :from-end from-end)
	 for pos = 0 then (1+ pos)
	 until (or (endp it1) (endp it2))
	 do
	   (unless (funcall test (funcall key (current it1)) (funcall key (current it2)))
	     (return (compute-pos pos)))

	   (advance it1)
	   (advance it2)

	 finally
	   (unless (and (endp it1) (endp it2))
	     (return (compute-pos pos)))))))
