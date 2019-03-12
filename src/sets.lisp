;;;; sets.lisp
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

;;;; Generic Set Functions


;;; Testing for membership and subsets

(defgeneric memberp (item set &key &allow-other-keys)
  (:documentation
   "Returns true if ITEM is an element of the set SET.

    Methods specialized on lists also accept TEST and KEY keyword
    parameters. These parameters are ignored by other methods."))

(defgeneric subsetp (set1 set2 &key &allow-other-keys)
  (:documentation
   "Returns true if the set SET1 is a subset of the set SET2.

    Methods specialized on lists also accept TEST and KEY keyword
    parameters. These parameters are ignored by other methods."))


;;; Adding Elements

(defgeneric adjoin (item set &key &allow-other-keys)
  (:documentation
   "Returns a new set which contains ITEM and all elements in set SET.

    Methods specialized on lists also accept TEST and KEY keyword
    parameters. These parameters are ignored by other methods."))

(defgeneric nadjoin (item set &key &allow-other-keys)
  (:documentation
   "Same as ADJOIN however is permitted to destructively modify
    SET."))


;;; Intersection

(defgeneric intersection (set1 set2 &key &allow-other-keys)
  (:documentation
   "Returns the set intersection of SET1 and SET2. The set returned
    contains all elements that occur in both SET1 and SET2.

    Methods specialized on lists also accept TEST, TEST-NOT and KEY
    keyword parameters. These parameters are ignored by other
    methods."))

(defgeneric nintersection (set1 set2 &key &allow-other-keys)
  (:documentation
   "Same as INTERSECTION however is permitted to destructively modify
    SET1 or SET2."))


;;; Set Difference

(defgeneric set-difference (set1 set2 &key &allow-other-keys)
  (:documentation
   "Returns the set difference of SET1 and SET2. The set returned
    contains all the elements of SET1 that do not appear in SET1.

    Methods specialized on lists also accept TEST, TEST-NOT and KEY
    keyword parameters. These parameters are ignored by other
    methods."))

(defgeneric nset-difference (set1 set2 &key &allow-other-keys)
  (:documentation
   "Same as SET-DIFFERENCE however is permitted to destructively
    modify SET1 or SET2."))


;;; Set Exclusive Or

(defgeneric set-exclusive-or (set1 set2 &key &allow-other-keys)
  (:documentation
   "Returns a set containing all elements that appear in exactly one
    of SET1 and SET2.

    Methods specialized on lists also accept TEST, TEST-NOT and KEY
    keyword parameters. These parameters are ignored by other
    methods."))

(defgeneric nset-exclusive-or (set1 set2 &key &allow-other-keys)
  (:documentation
   "Same as SET-EXCLUSIVE-OR however is permitted to destructively
    modify SET1 or SET2."))


;;; Union

(defgeneric union (set1 set2 &key &allow-other-keys)
  (:documentation
   "Returns the set union of SET1 and SET2. The set returned contains
    all elements of SET1 and SET2.

    Methods specialized on lists also accept TEST, TEST-NOT and KEY
    keyword parameters. These parameters are ignored by other
    methods."))

(defgeneric nunion (set1 set2 &key &allow-other-keys)
  (:documentation
   "Same as UNION however is permitted to destructively modify SET1 or
    SET2."))


;;;; Hash-Sets

(defstruct (hash-set (:include hash-map)
		     (:constructor hash-table-set (table))
		     (:copier nil))

  "Set data structure implemented using hash tables.

   This structure is equivalent to the `HASH-MAP' structure, in-fact
   all set methods also work on `HASH-MAP's as well as
   `HASH-SET's. However this structure should be used to indicate that
   the values stored in the hash table are unimportant. Some methods,
   specialized on `HASH-SET', will ignore the hash-table values.")

(defun make-hash-set (&rest args &key &allow-other-keys)
  "Creates a `HASH-SET'. The keyword arguments accepted by
   MAKE-HASH-MAP are accepted by MAKE-HASH-SET."

  (hash-table-set (apply #'make-hash-map-table args)))

(defun hash-set (&rest elements)
  "Returns a `HASH-SET' with elements ELEMENTS."

  (let ((table (make-generic-hash-table)))
    (dolist (element elements)
      (with-custom-hash-table
	(setf (gethash element table) t)))
    (hash-table-set table)))

(defmethod copy ((set hash-set) &key)
  (hash-table-set (copy-generic-hash-table (hash-set-table set) nil)))


;;; Iterators

(defmethod make-iterator ((set hash-set) start end)
  (make-list-iterator :cons (hash-set->list set start end)))

(defmethod make-reverse-iterator ((set hash-set) start end)
  (make-iterator set start end))


(defun hash-set->list (set &optional (start 0) end)
  "Returns a list containing the elements of the hash set SET,
   i.e. the keys of the underlying hash table. START and END determine
   the number of elements that will be returned. If START is zero and
   END is NIL all elements are returned."

  (let ((table (hash-set-table set)))
    (flet ((get-all ()
	     (let (list)
	       (do-generic-map (key nil table)
		 (push key list))
	       list))

	   (get-some (count)
	     (let (list (n 0))
	       (do-generic-map (key nil table)
		 (when (cl:= n count)
		   (return nil))
		 (push key list)
		 (cl:incf n))
	       list)))

      (with-custom-hash-table
	(if (or (cl:plusp start) end)
	    (get-some (cl:- (or end (hash-table-count table)) start))
	    (get-all))))))


;;; Checking for membership and subsets

(defmethod memberp (item (set hash-map) &key)
  (with-custom-hash-table
    (nth-value 1 (gethash item (hash-map-table set)))))

(defmethod subsetp ((set1 hash-map) (set2 hash-map) &key)
  (let ((set1 (hash-map-table set1))
	(set2 (hash-map-table set2)))
    (with-custom-hash-table
      (when (cl:<= (hash-table-count set1) (hash-table-count set2))
	(do-generic-map (key nil set1 t)
	  (unless (nth-value 1 (gethash key set2))
	    (return nil)))))))


;;; Adding Elements

(defmethod adjoin (item (set hash-map) &key)
  (aprog1 (copy set)
    (nadjoin item it)))

(defmethod nadjoin (item (set hash-map) &key)
  (let ((set (hash-map-table set)))
    (with-custom-hash-table
      (unless (nth-value 1 (gethash item set))
	(setf (gethash item set) t))))
  set)

(defmethod nadjoin (item (set hash-set) &key)
  (with-custom-hash-table
    (setf (gethash item (hash-set-table set)) t))
  set)


;;; Intersection

(defmethod intersection ((set1 hash-map) (set2 hash-map) &key &allow-other-keys)
  (let ((new (make-hash-set)))
    (let ((new (hash-set-table new))
	  (set1 (hash-map-table set1))
	  (set2 (hash-map-table set2)))
      (do-generic-map (key value set1)
	(when (nth-value 1 (gethash key set2))
	  (setf (gethash key new) value))))
    new))

(defmethod nintersection ((set1 hash-map) (set2 hash-map) &key &allow-other-keys)
  (let ((set1 (hash-map-table set1))
	(set2 (hash-map-table set2)))
    (do-generic-map (key nil set1)
      (unless (nth-value 1 (gethash key set2))
	(remhash key set1))))
  set1)


;;; Set Difference

(defmethod set-difference ((set1 hash-map) (set2 hash-map) &key &allow-other-keys)
  (let ((new (make-hash-set)))
    (let ((new (hash-set-table new))
	  (set1 (hash-map-table set1))
	  (set2 (hash-map-table set2)))
      (do-generic-map (key value set1)
	(unless (nth-value 1 (gethash key set2))
	  (setf (gethash key new) value))))
    new))

(defmethod nset-difference ((set1 hash-map) (set2 hash-map) &key &allow-other-keys)
  (let ((set1 (hash-map-table set1))
	(set2 (hash-map-table set2)))
    (do-generic-map (key nil set1)
      (when (nth-value 1 (gethash key set2))
	(remhash key set1))))
  set1)


;;; Union

(defmethod union ((set1 hash-map) (set2 hash-map) &key &allow-other-keys)
  (nunion (copy set1) set2))

(defmethod nunion ((set1 hash-map) (set2 hash-map) &key &allow-other-keys)
  (let ((set1 (hash-map-table set1))
	(set2 (hash-map-table set2)))
    (do-generic-map (key value set2)
      (setf (gethash key set1) value)))
  set1)


;;; Set Exclusive Or

(defmethod set-exclusive-or ((set1 hash-map) (set2 hash-map) &key &allow-other-keys)
  (nset-exclusive-or (copy set1) set2))

(defmethod nset-exclusive-or ((set1 hash-map) (set2 hash-map) &key &allow-other-keys)
  (let ((set1 (hash-map-table set1))
	(set2 (hash-map-table set2)))
    (do-generic-map (key value set2)
      (unless (remhash key set1)
	(setf (gethash key set1) value))))
  set1)


;;; Other Set Methods

(defmethod equalp ((a hash-set) (b hash-set))
  "Returns true if the `HASH-SETS' A and B have the same number of
   elements and each element of A is also an element of B."

  (let ((a (hash-set-table a))
	(b (hash-set-table b)))
    (with-custom-hash-table
      (when (cl:= (hash-table-count a) (hash-table-count b))
	(do-generic-map (key nil a t)
	  (unless (nth-value 1 (gethash key b))
	    (return nil)))))))

(defmethod subtract ((a hash-map) (b hash-map))
  "Returns the set difference of set A and set B."

  (set-difference a b))

(defmethod add ((a hash-map) (b hash-map))
  "Returns the union of set A and set B."

  (union a b))


;;;; Lists as Sets

(defmethod memberp (item (set list) &key (test #'equalp) key)
  (member item set :test test :key (or key #'identity)))

(defmethod subsetp ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:subsetp set1 set2 :test test :key key))


(defmethod adjoin (item (set list) &key (test #'equalp) key)
  (cl:adjoin item set :test test :key (or key #'identity)))

(defmethod nadjoin (item (set list) &key (test #'equalp) key)
  (cl:adjoin item set :test test :key (or key #'identity)))


(defmethod intersection ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:intersection set1 set2 :test test :key key))

(defmethod nintersection ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:nintersection set1 set2 :test test :key key))


(defmethod set-difference ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:set-difference set1 set2 :test test :key key))

(defmethod nset-difference ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:nset-difference set1 set2 :test test :key key))


(defmethod set-exclusive-or ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:set-exclusive-or set1 set2 :test test :key key))

(defmethod nset-exclusive-or ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:nset-exclusive-or set1 set2 :test test :key key))


(defmethod union ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:union set1 set2 :test test :key key))

(defmethod nunion ((set1 list) (set2 list) &key (test #'equalp) key)
  (cl:nunion set1 set2 :test test :key key))
