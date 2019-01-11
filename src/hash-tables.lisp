;;;; hash-tables.lisp
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

;;;; Generic hash-table interface, and implementation of collector and
;;;; interator interfaces for hash-tables.


;;; Generic Hash Function

(defgeneric hash (object)
  (:documentation
   "Should return a hash code for the object. For further details on
    the constraints of the hash code returned, see the documentation
    for CL:SXHASH.")

  (:method (obj)
    (sxhash obj)))

;; Constructor for hash-table's with generic HASH function.

(define-custom-hash-table-constructor make-generic-hash-table
    :test equalp :hash-function hash)


(defstruct (hash-map
	     (:constructor hash-map (table))
	     (:copier nil))

  "Hash-table wrapper for Common Lisp hash-tables and generic hash
   tables.

   The TABLE slot stores the actual hash-table which may either be a
   native hash table or a CL-CUSTOM-HASH-TABLE:CUSTOM-HASH-TABLE. On
   implementations which provide functionality for specifying a custom
   hash function, this slot always contains a CL:HASH-TABLE."

  table)

(defun make-hash-map (&rest args &key (test 'equalp) &allow-other-keys)
  "Creates a hash map. If TEST is the symbol GENERIC-CL:EQUALP (the
   default), a generic hash-table, with HASH as the hash function and
   GENERIC-CL:EQUALP as the comparison function. If another value for
   test (the available options are specified in the documentation for
   CL:MAKE-HASH-TABLE) is given, the native hash function is
   used.

   The remaining keyword arguments accepted by CL:MAKE-HASH-TABLE, are
   also accepted by this function.

   The return value is always a HASH-MAP which wraps either the
   generic or Common Lisp hash-table."

  (hash-map
   (case test
     (equalp
      (apply #'make-generic-hash-table (remove-from-plist args :test)))

     (otherwise
      (apply #'make-hash-table args)))))

(define-compiler-macro make-hash-map (&whole form &rest args)
  (match (getf args :test)
    ((list 'quote 'equalp)
     `(hash-map (make-generic-hash-table ,@(remove-from-plist args :test))))

    ((or (list 'quote (not 'equalp)) nil)
     `(hash-map (make-hash-table ,@args)))

    (_ form)))


;;;; Generic Lookup Functions

(defgeneric get (key map &optional default)
  (:documentation
   "Returns the value of the entry corresponding to the key KEY in the
    map MAP. If the map does not contain any entry with that key,
    DEFAULT is returned.

    Returns two values: the value and true if an entry with the key
    KEY was found in the map, false otherwise."))

(defgeneric (setf get) (value key map &optional default)
  (:documentation
   "Sets the value of the entry corresponding to the key KEY in the
    map MAP. DEFAULT is ignored."))


;;; Hash-Tables

(defmethod get (key (map hash-map) &optional default)
  (with-custom-hash-table
    (gethash key (hash-map-table map) default)))

(defmethod (setf get) (value key (map hash-map) &optional default)
  (declare (ignore default))

  (with-custom-hash-table
    (setf (gethash key (hash-map-table map)) value)))

(defmethod erase ((map hash-map) key)
  "Removes the entry corresponding to the key KEY from the hash-map
   MAP."

  (with-custom-hash-table
    (remhash key map)))


;;; Alists/Plists

(defmethod get (key (map list) &optional default)
  (typecase (first map)
    (cons
     (aif (assoc key map :test #'equalp)
	  (car it)
	  default))

    (otherwise
     (multiple-value-bind (value found?) (get-plist-value key map)
       (if found? value default)))))

(defun get-plist-value (lookup-key plist)
  (loop
     for (key value) on plist by #'cddr
     do
       (when (equalp lookup-key key)
	 (return (values value t)))))


;;;; Hash Table Iteration

(defmacro! do-generic-map ((key value o!hash-map &optional result) &body body)
  `(with-custom-hash-table
     (with-hash-table-iterator (,g!next ,g!hash-map)
       (block ,g!block
	 (loop
	    (multiple-value-bind (,g!more? ,key ,value) (,g!next)
	      (unless ,g!more?
		(return-from ,g!block ,result))
	      ,@body))))))


;;;; Hash Table Copying

(defmethod copy ((map hash-map) &key deep)
  (hash-map (copy-generic-hash-table (hash-map-table map) deep)))

(defun copy-generic-hash-table (map deep)
  (let ((new (make-empty-hash-table map)))
    (flet ((deep-copy ()
	     (do-generic-map (key value map)
	       (setf (gethash key new) (copy value :deep t))))

	   (shallow-copy ()
	     (do-generic-map (key value map)
	       (setf (gethash key new) (copy value)))))
      (if deep (deep-copy) (shallow-copy)))
    new))


;;;; Hash Table Equality

(defmethod equalp ((a hash-map) (b hash-map))
  "Hash-table comparison method. Returns true if both hash-tables have
   the same number of entries, and the value corresponding to each key
   in A is equal (by EQUALP) to the value corresponding to the same
   key in B.

   Issue: Hash-table equality is not necessarily symmetric if the test
   functions of the two hash-tables are different."

  (let ((a (hash-map-table a))
	(b (hash-map-table b)))

    (with-custom-hash-table
      (when (cl:= (hash-table-count a) (hash-table-count b))
	(do-generic-map (key a-value a t)
	  (multiple-value-bind (b-value in-hash?) (gethash key b)
	    (unless (and in-hash? (equalp a-value b-value))
	      (return-from equalp nil))))))))


;;;; Hash Table Collectors

(defmethod empty-clone ((map hash-map))
  (hash-map (make-empty-hash-table (hash-map-table map))))

(defun make-empty-hash-table (table)
  (with-custom-hash-table
    (let ((test (hash-table-test table)))
      (if (eq test 'equalp)
	  (make-generic-hash-table)
	  (make-hash-table :test test)))))

(defmethod make-collector ((map hash-map) &key front)
  (declare (ignore front))
  map)

(defmethod collect ((map hash-map) item)
  (destructuring-bind (key . value) item
    (with-custom-hash-table
      (setf (gethash key (hash-map-table map)) value))))

(defmethod collector-sequence ((map hash-map))
  map)


;;;; Hash-Table Iterators

(defstruct (hash-table-iterator (:include list-iterator))
  "Hash-table iterator. The actual hash-table is converted to an ALIST
   upon creation of the iterator, since closing over the iteration
   function provided by WITH-HASH-TABLE-ITERATOR is undefined, and
   assigned to the CONS slot. A reference to the hash-table is only
   kept to implement (SETF AT)."

  table)

(defmethod make-iterator ((map hash-map) start end)
  "Create an iterator for the elements of a `hash-map where each
   element is a CONS of the form (KEY . VALUE). The order in which the
   elements are iterated is unspecified, likewise there is no
   guarantee which elements will be iterated over if START is not 0
   and END is not NIL."

  (make-hash-table-iterator
   :table (hash-map-table map)
   :cons (hash-map->list map start end)))

(defmethod make-reverse-iterator ((hash hash-map) start end)
  "Create a reverse iterator for the elements of a `hash-table'. Since
   the order of iteration is unspecified this is identical to
   MAKE-ITERATOR."

  (make-iterator hash start end))

(defmethod (setf at) (value (iter hash-table-iterator))
  "Sets the value corresponding to the current key being
   iterator (CAR (AT ITER)) to VALUE."

  (with-custom-hash-table
    (setf (gethash (caar (hash-table-iterator-cons iter))
		   (hash-table-iterator-table iter))
	  value)))

(defun hash-map->list (map &optional (start 0) end)
  "Returns an ALIST containing the elements of the hash map MAP. START
   and END determine the number of elements that the hash map
   contains. If START is zero and END is NIL all elements are included
   in the ALIST."

  (let ((table (hash-map-table map)))
    (flet ((get-all ()
	     (let (list)
	       (do-generic-map (key value table)
		 (push (cons key value) list))
	       list))

	   (get-some (count)
	     (let (list (n 0))
	       (do-generic-map (key value table)
		 (when (cl:= n count)
		   (return nil))
		 (push (cons key value) list)
		 (cl:incf n))
	       list)))

      (with-custom-hash-table
	(if (or (plusp start) end)
	    (get-some (cl:- (or end (hash-table-count table)) start))
	    (get-all))))))


;;; Sequence Methods specialized on hash-tables

(defmethod length ((map hash-map))
  "Returns the number of entries in the hash map MAP."

  (with-custom-hash-table
    (hash-table-count (hash-map-table map))))


;;;; Hash-Table Utilities

(defun alist-hash-map (alist &rest args)
  "Returns an hash map containing all entries in the association list
   ALIST. ARGS are the additional arguments passed to MAKE-HASH-MAP."

  (let* ((map (apply #'make-hash-map args))
	 (table (hash-map-table map)))
    (with-custom-hash-table
      (loop
	 for (key . value) in alist
	 do
	   (setf (gethash key table) value)))
    map))

(defun hash-map-alist (map)
  "Returns an ALIST containing all the entries (key-value pairs) in
   the hash-map MAP."

  (let ((table (hash-map-table map)))
    (let (list)
      (do-generic-map (key value table)
	(push (cons key value) list))
      list)))
