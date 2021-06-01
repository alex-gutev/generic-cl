;;;; hash-tables.lisp
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

;;;; Unit tests for generic hash-tables

(in-package :generic-cl/test)


;;; Test Suite Definition

(def-suite hash-maps
    :description "Test generic hash-tables (hash-maps)"
    :in generic-cl)

(in-suite hash-maps)


;;; Custom Key Type

(defparameter *hash-function-called-p* nil
  "Flag for whether the HASH function was called.")

(defparameter *equalp-function-called-p* nil
  "Flag for whether the EQUALP function was called.")

(defparameter *like-hash-function-called-p* nil
  "Flag for whether the LIKE-HASH function was called.")

(defparameter *likep-function-called-p* nil
  "Flag for whether the LIKEP function was called.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct custom-key
    "Used as a custom key type in the generic hash-table tests."

    (slot1 0)
    (slot2 0))

  (defmethod make-load-form ((object custom-key) &optional env)
    (make-load-form-saving-slots object :environment env))

  (defmethod hash ((key custom-key))
    "Hash method for `CUSTOM-KEY'. Sets the *HASH-FUNCTION-CALLED-P* flag
   to true."

    (with-accessors ((slot1 custom-key-slot1)
		     (slot2 custom-key-slot2))
	key

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
		     (slot2 custom-key-slot2))
	key

      (setf *like-hash-function-called-p* t)

      ;; Compute primitive hash
      (logxor (like-hash slot1) (like-hash slot2))))

  (defmethod likep ((a custom-key) (b custom-key))
    "LIKEP method for `CUSTOM-KEY'. Sets the *LIKEP-FUNCTION-CALLED-P*
   flag to true."

    (setf *likep-function-called-p* t)

    (and (likep (custom-key-slot1 a) (custom-key-slot1 b))
	 (likep (custom-key-slot2 a) (custom-key-slot2 b)))))


;;; Test Generic Hash Tables

(test ensure-hash-map
  "Test ENSURE-HASH-MAP function"

  (let* ((table (make-hash-map))
	 (map (ensure-hash-map table)))

    (is (typep map 'hash-map))
    (is (eq table map))

    ;; Fails on CMUCL as CL-CUSTOM-HASH-TABLE uses a longer
    ;; identifier of the form:
    ;;
    ;; CL-CUSTOM-HASH-TABLE:|custom-hash-table EQUALP HASH|

    #-cmucl
    (is (= 'equalp (hash-map-test map))))

  (let* ((table (make-hash-table))
	 (map (ensure-hash-map table)))

    (is (typep map 'hash-map))
    (is (eq table (hash-map-table map))))

  (let* ((table (generic-cl.map::make-generic-hash-table))
	 (map (ensure-hash-map table)))

    (is (typep map 'hash-map))
    (is (eq table (hash-map-table map))))

  (signals type-error (ensure-hash-map 'x)))

(def-fixture test-hash-equalp-calls ()
  (let((*hash-function-called-p* nil)
       (*equalp-function-called-p* nil))
    (&body)))

(test (get-hash-equalp :fixture test-hash-equalp-calls)
  "Test GET function and that HASH, EQUALP are called"

  (let ((key (make-custom-key :slot1 1 :slot2 2))
	(map (make-hash-map)))

    (is-true (emptyp map))

    (setf (get key map) 'a)
    (setf (get (make-custom-key) map) 'b)
    (setf (get (make-custom-key :slot1 100 :slot2 "a") map) 'c)

    (is-true *hash-function-called-p*)

    (is (= '(a t) (multiple-value-list (get key map))))

    (is (= '(a t) (multiple-value-list (get (make-custom-key :slot1 1 :slot2 2) map))))
    (is (= '(nil nil) (multiple-value-list (get (make-custom-key :slot1 2 :slot2 1) map))))
    (is (= '(x nil) (multiple-value-list (get (make-custom-key :slot1 2 :slot2 1) map 'x))))

    (is (= '(b t) (multiple-value-list (get (make-custom-key) map))))
    (is (= '(c t) (multiple-value-list (get (make-custom-key :slot1 100 :slot2 "a") map))))

    (is (= 3 (length map)))

    (is-true *equalp-function-called-p*)))

(test erase-clear
  "Test ERASE and CLEAR functions"

  (let ((key (make-custom-key :slot1 1 :slot2 2))
	(map (make-hash-map)))

    (is-true (emptyp map))

    (setf (get key map) 'a)
    (setf (get (make-custom-key) map) 'b)
    (setf (get (make-custom-key :slot1 100 :slot2 "a") map) 'c)

    (is-false (erase map 'a))
    (is-true (erase map (make-custom-key :slot1 1 :slot2 2)))

    (is (= '(nil nil) (multiple-value-list (get key map))))

    (is (= 2 (length map)))

    (clear map)
    (is (= 0 (length map)))))

(test ensure-get
  "Test ENSURE-GET macro"

  (let ((map (make-hash-map)))
    (is (= '(3 nil) (multiple-value-list (ensure-get 'x map 3))))
    (is (= '(nil nil) (multiple-value-list (ensure-get 'y map))))

    (is (= '(3 t) (multiple-value-list (ensure-get 'x map 5))))
    (is (= '(3 t) (multiple-value-list (ensure-get 'x map))))
    (is (= '(nil t) (multiple-value-list (ensure-get 'y map 4))))))

(test map-keys-and-map-values
  "Test MAP-KEYS and MAP-VALUES functions"

  (let ((map (make-hash-map)))
    (setf (get 'a map) 1)
    (setf (get 'b map) 2)
    (setf (get "hello" map) #\z)

    (is (set-equal '(a b "hello") (map-keys map) :test #'equal))
    (is (set-equal '(1 2 #\z) (map-values map) :test #'equal))))

(test hash-map-coerce-list
  "Test Hash-Map to list conversions and vice versa"

  (let ((map (alist-hash-map '((a . 1) (b . 2) (c . 3)))))
    (is (= 3 (length map)))

    (is (= 1 (get 'a map)))
    (is (= 2 (get 'b map)))
    (is (= 3 (get 'c map)))

    (setf (get (make-custom-key :slot1 5 :slot2 'x) map) "hello")
    (is (= "hello" (get #S(custom-key :slot1 5 :slot2 x) map)))

    (is (= 4 (length map)))

    (is (set-equal
	 '((a . 1) (b . 2) (c . 3) (#S(custom-key :slot1 5 :slot2 x) . "hello"))
	 (hash-map-alist map)

	 :test #'equalp))

    (is (set-equal
	 '((a . 1) (b . 2) (c . 3) (#S(custom-key :slot1 5 :slot2 x) . "hello"))
	 (coerce map 'alist)

	 :test #'equalp))

    (is (set-equal
	 '((a . 1) (b . 2) (c . 3) (#S(custom-key :slot1 5 :slot2 x) . "hello"))
	 (plist-alist (coerce map 'plist))

	 :test #'equalp))))

(test (hash-map-likep-test :fixture test-hash-equalp-calls)
  "Test hash-maps with LIKEP test function"

  (let ((key (make-custom-key :slot1 #\a :slot2 #\b))
	(map (make-hash-map :test #'likep)))

    (setf (get key map) 'a)
    (setf (get (make-custom-key) map) 'b)
    (setf (get (make-custom-key :slot1 100 :slot2 "a") map) 'c)

    (is-true *like-hash-function-called-p*)

    (is (= '(a t) (multiple-value-list (get key map))))
    (is (= '(a t) (multiple-value-list (get (make-custom-key :slot1 #\A :slot2 #\b) map))))

    (is (= '(nil nil) (multiple-value-list (get (make-custom-key :slot1 2 :slot2 1) map))))
    (is (= '(x nil) (multiple-value-list (get (make-custom-key :slot1 2 :slot2 1) map 'x))))

    (is (= '(b t) (multiple-value-list (get (make-custom-key) map))))
    (is (= '(c t) (multiple-value-list (get (make-custom-key :slot1 100 :slot2 "A") map))))

    (is-true *likep-function-called-p*)

    ;; Character Keys

    (setf (get #\X map) 1)
    (is (= '(1 t) (multiple-value-list (get #\X map))))
    (is (= '(1 t) (multiple-value-list (get #\x map))))
    (is (= '(nil nil) (multiple-value-list (get #\z map))))

    ;; List Keys

    (setf (get '(1 'a #\b) map) 2)
    (is (= '(2 t) (multiple-value-list (get '(1 'a #\b) map))))
    (is (= '(2 t) (multiple-value-list (get '(1 'a #\B) map))))

    (is (= '(nil nil) (multiple-value-list (get '(100 'a #\b) map))))
    (is (= '(nil nil) (multiple-value-list (get '(1 'a #\x) map))))
    (is (= '(nil nil) (multiple-value-list (get '(1 'a #\b 3) map))))

    ;; Vector Keys

    (setf (get #(1 'a #\b) map) 2)
    (is (= '(2 t) (multiple-value-list (get #(1 'a #\b) map))))
    (is (= '(2 t) (multiple-value-list (get #(1 'a #\B) map))))

    (is (= '(nil nil) (multiple-value-list (get #(100 'a #\b) map))))
    (is (= '(nil nil) (multiple-value-list (get #(1 'a #\x) map))))
    (is (= '(nil nil) (multiple-value-list (get #(1 'a #\b 3) map))))

    ;; Array Keys

    (setf (get #2A((1 'a) (#\b 'c)) map) 2)
    (is (= '(2 t) (multiple-value-list (get #2A((1 'a) (#\b 'c)) map))))
    (is (= '(2 t) (multiple-value-list (get #2A((1 'a) (#\B 'c)) map))))

    (is (= '(nil nil) (multiple-value-list (get #2A((100 'a) (#\b 'c)) map))))
    (is (= '(nil nil) (multiple-value-list (get #((1 'a) (#\x 'c)) map))))
    (is (= '(nil nil) (multiple-value-list (get #((1 'a 'b) (#\b 'c 'd)) map))))

    ;; String Keys

    (setf (get "hello" map) 'hello)
    (is (= '(hello t) (multiple-value-list (get "hello" map))))
    (is (= '(hello t) (multiple-value-list (get "HELLO" map))))

    (is (= '(nil nil) (multiple-value-list (get "hello world" map))))
    (is (= '(nil nil) (multiple-value-list (get "hell" map))))))


;;; Test standard hash-tables with standard test function

(test hash-map-equal-test
  "Test hash-maps with EQUAL test function"

  (let ((map (make-hash-map :test #'equal))
	(key (make-custom-key :slot1 1 :slot2 2)))

    ;; The following fails on CLISP as for some reason
    ;; EXT:FASTHASH-EQUAL is returned, if the test function is
    ;; EQUAL. Not sure whether this is against the Common Lisp Spec.

    #-clisp
    (is (= 'equal (hash-map-test map)))

    (setf (get key map) 'a)
    (setf (get "alex" map) 1)
    (setf (get "bob" map) 2)
    (setf (get "mary" map) 3)

    (is (= '(a t) (multiple-value-list (get key map))))
    (is (= '(nil nil) (multiple-value-list (get #S(custom-key :slot1 1 :slot2 2) map))))
    (is (= '(nil nil) (multiple-value-list (get "ALEX" map))))
    (is (= '(5 nil) (multiple-value-list (get "ALEX" map 5))))

    (is (= 1 (get "alex" map)))
    (is (= 2 (get "bob" map)))
    (is-true (erase map "mary"))

    (is (= 3 (length map)))))

(test hash-map-cl-equalp-test
  "Test hash-maps with CL:EQUALP test function"

  (let ((map (make-hash-map :test #'cl:equalp)))

    (is (= 'cl:equalp (hash-map-test map)))

    (setf (get "alex" map) 1)
    (setf (get "Alex" map) 2)
    (setf (get "bob" map) 3)

    (is (= 2 (length map)))
    (is (= 2 (get "ALEX" map)))
    (is (= 3 (get "Bob" map)))))

(test raw-hash-table
  "Test hash-map functions on raw hash-tables"

  (let ((map (make-hash-table :test #'cl:equalp)))
    (is-true (emptyp map))

    (setf (get "alex" map) 1)
    (setf (get "Alex" map) 2)
    (setf (get "bob" map) 3)

    (is (= 2 (length map)))
    (is (= '(2 t) (multiple-value-list (get "ALEX" map))))
    (is (= '(3 t) (multiple-value-list (get "Bob" map))))

    (is (= '(nil nil) (multiple-value-list (get "z" map))))
    (is (= '(some-value nil) (multiple-value-list (get "z" map 'some-value))))

    (is-true (erase map "bob"))

    (is (= 1 (length map)))
    (clear map)
    (is (= 0 (length map)))))


;;; Test interface on ALISTS and PLISTS

(test map-alist
  "Test map interface on ALISTS"

  (let ((alist '((#S(custom-key :slot1 5 :slot2 7) . 1) (a . 3) (#(b c) . "x"))))
    (is (= '(1 t) (multiple-value-list (get (make-custom-key :slot1 5 :slot2 7) alist))))
    (is (= '(3 t) (multiple-value-list (get 'a alist))))
    (is (= '("x" t) (multiple-value-list (get #(b c) alist))))
    (is (= '(nil nil) (multiple-value-list (get 'x alist))))
    (is (= '(4 nil) (multiple-value-list (get 'x alist 4))))))

(test map-plist
  "Test map interface on PLISTS"

  (let ((plist '(:a 1 :b 2 :c 3)))
    (is (= '(1 t) (multiple-value-list (get :a plist))))
    (is (= '(2 t) (multiple-value-list (get :b plist))))
    (is (= '(3 t) (multiple-value-list (get :c plist))))
    (is (= '(nil nil) (multiple-value-list (get :d plist))))
    (is (= '(x nil) (multiple-value-list (get :d plist 'x))))))
