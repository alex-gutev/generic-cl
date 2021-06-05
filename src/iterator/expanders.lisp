;;;; expanders.lisp
;;;;
;;;; Copyright 2021 Alexander Gutev
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

;;;; DOSEQ Expanders

(in-package :generic-cl.iterator)

;;; Lists

(defmethod make-doseq list ((type t) (var symbol) form args env)
  (declare (ignore env))

  (values
   nil
   nil
   nil
   `(for ,var in ,form)
   nil))

(defmethod make-doseq list ((type t) (pattern list) form args env)
  (with-gensyms (item)
    (values
     nil
     nil
     nil
     `(for ,item in ,form)
     `(destructuring-bind ,pattern ,item (&body)))))


;;; Vectors

(defmethod make-doseq vector ((type t) (var symbol) form args env)
  (declare (ignore env))

  (values
   nil
   nil
   nil
   `(for ,var across ,form)
   nil))

(defmethod make-doseq vector ((type t) (pattern list) form args env)
  (declare (ignore env))

  (with-gensyms (item)
    (values
     nil
     nil
     nil
     `(for ,item across ,form)
     `(destructuring-bind ,pattern ,item (&body)))))


;;; Hash-Tables

(defmethod make-doseq hash-table ((type t) (var symbol) form args env)
  (declare (ignore env))

  (with-gensyms (table key value)
    (values
     `((,table ,form))
     nil
     nil
     `(for ,key being the hash-keys of ,table
           for ,value being the hash-values of ,table)
     nil)))

(defmethod make-doseq hash-table ((type t) (pattern list) form args env)
  (declare (ignore env))

  (flet ((make-destructure (var pattern &optional body)
           (if (symbolp pattern)
               body
               `(destructuring-bind ,pattern ,var
                  ,(or body '(&body))))))

    (destructuring-bind (key . value) pattern
      (let ((key-var (if (symbolp key) key (gensym "KEY")))
            (value-var (if (symbolp value) value (gensym "VALUE"))))

        (with-gensyms (table)
          (values
           `((,table ,form))
           nil
           nil
           (append
            (when key `(for ,key being the hash-keys of ,table))
            (when value `(for ,value being the hash-values of ,table)))

           (->> (make-destructure key-var key)
                (make-destructure value-var value))))))))


;;; Default

(defmethod make-doseq t ((type t) (var symbol) form args env)
  (declare (ignore env))

  (with-gensyms (it)
    (values
     `((,it (iterator ,form ,@args)))
     `((,var (at ,it)))
     `((prog1 (not (endp ,it)) (advance ,it)))
     nil
     nil)))

(defmethod make-doseq t ((type t) (pattern list) form args env)
  (declare (ignore env))

  (with-gensyms (item it)
    (values
     `((,it (iterator ,form ,@args)))
     `((,item (at ,it)))
     `((prog1 (not (endp ,it)) (advance ,it)))
     nil
     `(destructuring-bind ,pattern ,item
        (&body)))))
