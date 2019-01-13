;;;; collector.lisp
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

(in-package :generic-cl.impl)


;;;; Generic Collector Interface

(defgeneric empty-clone (sequence)
  (:documentation
   "Creates a new sequence of the same type and with the same
    properties as SEQUENCE however without any elements."))

(defgeneric sequence-of-type (type)
  (:documentation
   "Creates a sequence of the type TYPE."))


(defgeneric make-collector (sequence &key front)
  (:documentation
   "Returns a collector for adding items to SEQUENCE. If :FRONT is
    true the items will be added to the front of the sequence rather
    than the back."))

(defgeneric collect (collector item)
  (:documentation
   "Adds ITEM to the sequence with collector COLLECTOR."))

(defgeneric extend (collector sequence)
  (:documentation
   "Adds each item in SEQUENCE to the sequence with collector
    COLLECTOR.")

  (:method (collector seq)
    (doseq (item seq)
      (collect collector item)))

  (:method (collector (it iterator))
    (loop
       until (endp it)
       do
	 (collect collector (at it))
	 (advance it))))

(defgeneric collector-sequence (collector)
  (:documentation
   "Returns the sequence associated with the collector COLLECTOR.

    Calling this method is necessary, when no more items will be added
    to the sequence, as the original sequence passed to MAKE-COLLECTOR
    might not have been destructively modified."))


;;;; Lists

(defmethod empty-clone ((sequence list))
  "Returns NIL the empty list."
  nil)


(defstruct list-collector
  "Collector object for adding items to the back of a list."

  head
  tail)

(defstruct front-list-collector
  "Collector object for adding items to the front of a list."

  cons)


(defmethod make-collector ((list list) &key front)
  (if front
      (make-front-list-collector :cons list)
      (make-list-collector :head list :tail (cl:last list))))


;;; Back

(defmethod collect ((c list-collector) item)
  (slet (list-collector-tail c)
    (->> (if it
	     (setf (cdr it) (cons item nil))
	     (setf (list-collector-head c) (cons item nil)))
	 (setf it))))

(defmethod extend ((c list-collector) (list list))
  (slet (list-collector-tail c)
    (->> (setf (cdr it) (copy-list list))
	 (cl:last)
	 (setf it))))

(defmethod collector-sequence ((c list-collector))
  (list-collector-head c))


;;; Front

(defmethod collect ((c front-list-collector) item)
  (push item (front-list-collector-cons c)))

(defmethod collector-sequence ((c front-list-collector))
  (front-list-collector-cons c))


;;;; Vectors

(defmethod empty-clone ((vec vector))
  (make-array (cl:length vec) :adjustable t :fill-pointer 0))


(defstruct front-vector-collector
  "Collector object for adding items to the front of a vector"
  vector)

(defmethod make-collector ((vec vector) &key front)
  (if front
      (make-front-vector-collector
       :vector (copy-array (cl:reverse vec) :adjustable t :fill-pointer t))
      vec))


;;; Front

(defmethod collect ((vec vector) item)
  (vector-push-extend item vec))

(defmethod collector-sequence ((vec vector))
  vec)


;;; Back

(defmethod collect ((c front-vector-collector) item)
  (vector-push-extend item (front-vector-collector-vector c)))

(defmethod collector-sequence ((c front-vector-collector))
  (cl:nreverse (front-vector-collector-vector c)))
