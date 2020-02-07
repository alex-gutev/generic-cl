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

;;;; Unit tests for generic hash-tables

(in-package :generic-cl.test)

(plan nil)

(defparameter *hash-function-called-p* nil
  "Flag for whether the HASH function was called.")

(defparameter *equalp-function-called-p* nil
  "Flag for whether the EQUALP function was called.")

(defparameter *like-hash-function-called-p* nil
  "Flag for whether the LIKE-HASH function was called.")

(defparameter *likep-function-called-p* nil
  "Flag for whether the LIKEP function was called.")


(defstruct custom-key
  "Used as a custom key type in the generic hash-table tests."

  (slot1 0)
  (slot2 0))

(defmethod hash ((key custom-key))
  "Hash method for `CUSTOM-KEY'. Sets the *HASH-FUNCTION-CALLED-P* flag
   to true."

  (with-accessors ((slot1 custom-key-slot1)
		   (slot2 custom-key-slot2)) key
    (setf *hash-function-called-p* t)

    ;; Compute primitive hash
    (logxor (hash slot1) (hash slot2))))

(defmethod equalp ((a custom-key) (b custom-key))
  "EQUALP method for `CUSTOM-KEY'. Sets the *EQUALP-FUNCTION-CALLED-P*
   flag to true."

  (setf *equalp-function-called-p* t)

  (and (equalp (custom-key-slot1 a) (custom-key-slot1 b))
       (equalp (custom-key-slot2 a) (custom-key-slot2 b))))

(defmethod like-hash ((key custom-key))
  "Like-Hash method for `CUSTOM-KEY'. Sets the
  *LIKE-HASH-FUNCTION-CALLED-P* flag to true."

  (with-accessors ((slot1 custom-key-slot1)
		   (slot2 custom-key-slot2)) key
    (setf *like-hash-function-called-p* t)

    ;; Compute primitive hash
    (logxor (like-hash slot1) (like-hash slot2))))

(defmethod likep ((a custom-key) (b custom-key))
  "LIKEP method for `CUSTOM-KEY'. Sets the *LIKEP-FUNCTION-CALLED-P*
   flag to true."

  (setf *likep-function-called-p* t)

  (and (likep (custom-key-slot1 a) (custom-key-slot1 b))
       (likep (custom-key-slot2 a) (custom-key-slot2 b))))


(subtest "Test Generic Hash-Tables"
  (subtest "Test ENSURE-HASH-MAP"
    (let* ((table (make-hash-map))
	   (map (ensure-hash-map table)))
      (is-type map 'hash-map)
      (is map table :test #'eq)

      ;; Fails on CMUCL as CL-CUSTOM-HASH-TABLE uses a longer
      ;; identifier of the form:
      ;;
      ;; CL-CUSTOM-HASH-TABLE:|custom-hash-table EQUALP HASH|

      #-cmucl
      (is (hash-map-test map) 'equalp "Hash-table test is GENERIC-CL:EQUALP"))

    (let* ((table (make-hash-table))
	   (map (ensure-hash-map table)))
      (is-type map 'hash-map)
      (is (hash-map-table map) table))

    (let* ((table (generic-cl.impl::make-generic-hash-table))
	   (map (ensure-hash-map table)))
      (is-type map 'hash-map)
      (is (hash-map-table map) table))

    (is-error (ensure-hash-map 'x) 'type-error))

  (let ((*hash-function-called-p* nil)
	(*equalp-function-called-p* nil))

    (let ((key (make-custom-key :slot1 1 :slot2 2))
	  (map (make-hash-map)))

      (subtest "Test HASH, EQUALP and GET"
	(ok (emptyp map) "EMPTYP")

	(setf (get key map) 'a)
	(setf (get (make-custom-key) map) 'b)
	(setf (get (make-custom-key :slot1 100 :slot2 "a") map) 'c)

	(ok *hash-function-called-p* "HASH called")

	(is-values (get key map) '(a t))

	(is-values (get (make-custom-key :slot1 1 :slot2 2) map) '(a t))
	(is-values (get (make-custom-key :slot1 2 :slot2 1) map) '(nil nil))
	(is-values (get (make-custom-key :slot1 2 :slot2 1) map 'x) '(x nil))

	(is-values (get (make-custom-key) map) '(b t))
	(is-values (get (make-custom-key :slot1 100 :slot2 "a") map) '(c t))

	(is (length map) 3)

	(ok *equalp-function-called-p* "EQUALP called"))

      (subtest "Test ERASE and CLEAR"
	(is (erase map 'a) nil)
	(ok (erase map (make-custom-key :slot1 1 :slot2 2)))

	(is-values (get key map) '(nil nil))

	(is (length map) 2)

	(clear map)
	(is (length map) 0 "Cleared")))

    (subtest "Test ENSURE-GET"
      (let ((map (make-hash-map)))
	(is-values (ensure-get 'x map 3) '(3 nil))
	(is-values (ensure-get 'y map) '(nil nil))

	(is-values (ensure-get 'x map 5) '(3 t))
	(is-values (ensure-get 'x map) '(3 t))
	(is-values (ensure-get 'y map 4) '(nil t)))))

  (subtest "Test MAP-KEYS and MAP-VALUES"
    (let ((map (make-hash-map)))
      (setf (get 'a map) 1)
      (setf (get 'b map) 2)
      (setf (get "hello" map) #\z)

      (is (map-keys map) '(a b "hello") :test (rcurry #'set-equal :test #'equal))
      (is (map-values map) '(1 2 #\z) :test (rcurry #'set-equal :test #'equal))))

  (subtest "Test Hash-Map to list conversions and vice versa"
    (let ((map (alist-hash-map '((a . 1) (b . 2) (c . 3)))))
      (is (length map) 3)

      (is (get 'a map) 1)
      (is (get 'b map) 2)
      (is (get 'c map) 3)

      (setf (get (make-custom-key :slot1 5 :slot2 'x) map) "hello")
      (is (get #S(custom-key :slot1 5 :slot2 x) map) "hello")

      (is (length map) 4)

      (is (hash-map-alist map)
	  '((a . 1) (b . 2) (c . 3) (#S(custom-key :slot1 5 :slot2 x) . "hello"))
	  :test (rcurry #'set-equal :test #'equalp))

      (is (coerce map 'alist)
	  '((a . 1) (b . 2) (c . 3) (#S(custom-key :slot1 5 :slot2 x) . "hello"))
	  :test (rcurry #'set-equal :test #'equalp))

      (is (plist-alist (coerce map 'plist))
	  '((a . 1) (b . 2) (c . 3) (#S(custom-key :slot1 5 :slot2 x) . "hello"))
	  :test (rcurry #'set-equal :test #'equalp))))

  (subtest "Test LIKEP Hash-Map"
    (let ((*like-hash-function-called-p* nil)
	  (*likep-function-called-p* nil))

      (let ((key (make-custom-key :slot1 #\a :slot2 #\b))
	    (map (make-hash-map :test #'likep)))

	(subtest "Test LIKE-HASH, LIKEP and GET"
	  (setf (get key map) 'a)
	  (setf (get (make-custom-key) map) 'b)
	  (setf (get (make-custom-key :slot1 100 :slot2 "a") map) 'c)

	  (ok *like-hash-function-called-p* "LIKE-HASH called")

	  (is-values (get key map) '(a t))
	  (is-values (get (make-custom-key :slot1 #\A :slot2 #\b) map) '(a t))

	  (is-values (get (make-custom-key :slot1 2 :slot2 1) map) '(nil nil))
	  (is-values (get (make-custom-key :slot1 2 :slot2 1) map 'x) '(x nil))

	  (is-values (get (make-custom-key) map) '(b t))
	  (is-values (get (make-custom-key :slot1 100 :slot2 "A") map) '(c t))

	  (ok *likep-function-called-p* "LIKEP called"))

	(subtest "Test CL Key Types"
	  (subtest "Characters"
	    (setf (get #\X map) 1)
	    (is-values (get #\X map) '(1 t))
	    (is-values (get #\x map) '(1 t))
	    (is-values (get #\z map) '(nil nil)))

	  (subtest "Lists"
	    (setf (get '(1 'a #\b) map) 2)
	    (is-values (get '(1 'a #\b) map) '(2 t))
	    (is-values (get '(1 'a #\B) map) '(2 t))

	    (is-values (get '(100 'a #\b) map) '(nil nil))
	    (is-values (get '(1 'a #\x) map) '(nil nil))
	    (is-values (get '(1 'a #\b 3) map) '(nil nil)))

	  (subtest "Vectors"
	    (setf (get #(1 'a #\b) map) 2)
	    (is-values (get #(1 'a #\b) map) '(2 t))
	    (is-values (get #(1 'a #\B) map) '(2 t))

	    (is-values (get #(100 'a #\b) map) '(nil nil))
	    (is-values (get #(1 'a #\x) map) '(nil nil))
	    (is-values (get #(1 'a #\b 3) map) '(nil nil)))

	  (subtest "Arrays"
	    (setf (get #2A((1 'a) (#\b 'c)) map) 2)
	    (is-values (get #2A((1 'a) (#\b 'c)) map) '(2 t))
	    (is-values (get #2A((1 'a) (#\B 'c)) map) '(2 t))

	    (is-values (get #2A((100 'a) (#\b 'c)) map) '(nil nil))
	    (is-values (get #((1 'a) (#\x 'c)) map) '(nil nil))
	    (is-values (get #((1 'a 'b) (#\b 'c 'd)) map) '(nil nil)))

	  (subtest "Strings"
	    (setf (get "hello" map) 'hello)
	    (is-values (get "hello" map) '(hello t))
	    (is-values (get "HELLO" map) '(hello t))


	    (is-values (get "hello world" map) '(nil nil))
	    (is-values (get "hell" map) '(nil nil))))))))

(subtest "Test Non-Generic Hash-Tables"
  (subtest "Hash-Tables with EQUAL test function"
    (let ((map (make-hash-map :test #'equal))
	  (key (make-custom-key :slot1 1 :slot2 2)))

      ;; The following fails on CLISP as for some reason
      ;; EXT:FASTHASH-EQUAL is returned, if the test function is
      ;; EQUAL. Not sure whether this is against the Common Lisp Spec.

      #-clisp
      (is (hash-map-test map) 'equal "Hash-table test is CL:EQUAL")

      (setf (get key map) 'a)
      (setf (get "alex" map) 1)
      (setf (get "bob" map) 2)
      (setf (get "mary" map) 3)

      (is-values (get key map) '(a t))
      (is-values (get #S(custom-key :slot1 1 :slot2 2) map) '(nil nil))
      (is-values (get "ALEX" map) '(nil nil))
      (is-values (get "ALEX" map 5) '(5 nil))

      (is (get "alex" map) 1)
      (is (get "bob" map) 2)
      (is (erase map "mary") t)

      (is (length map) 3)))

  (subtest "Hash-Tables with CL:EQUALP test function"
    (let ((map (make-hash-map :test #'cl:equalp)))

      (is (hash-map-test map) 'cl:equalp "Hash-table test is CL:EQUALP")

      (setf (get "alex" map) 1)
      (setf (get "Alex" map) 2)
      (setf (get "bob" map) 3)

      (is (length map) 2)
      (is (get "ALEX" map) 2)
      (is (get "Bob" map) 3)))

  (subtest "Raw Hash-Tables"
    (let ((map (make-hash-table :test #'cl:equalp)))
      (ok (emptyp map) "EMPTYP")

      (setf (get "alex" map) 1)
      (setf (get "Alex" map) 2)
      (setf (get "bob" map) 3)

      (is (length map) 2)
      (is-values (get "ALEX" map) '(2 t))
      (is-values (get "Bob" map) '(3 t))

      (is-values (get "z" map) '(nil nil))
      (is-values (get "z" map 'some-value) '(some-value nil))

      (is (erase map "bob") t)

      (is (length map) 1)
      (clear map)
      (is (length map) 0 "Cleared"))))

(subtest "Test ALISTS and PLISTS"
  (subtest "Test ALISTS"
    (let ((alist '((#S(custom-key :slot1 5 :slot2 7) . 1) (a . 3) (#(b c) . "x"))))
      (is-values (get (make-custom-key :slot1 5 :slot2 7) alist) '(1 t))
      (is-values (get 'a alist) '(3 t))
      (is-values (get #(b c) alist) '("x" t))
      (is-values (get 'x alist) '(nil nil))
      (is-values (get 'x alist 4) '(4 nil))))

  (subtest "Test PLISTS"
    (let ((plist '(:a 1 :b 2 :c 3)))
      (is-values (get :a plist) '(1 t))
      (is-values (get :b plist) '(2 t))
      (is-values (get :c plist) '(3 t))
      (is-values (get :d plist) '(nil nil))
      (is-values (get :d plist 'x) '(x nil)))))

(finalize)
