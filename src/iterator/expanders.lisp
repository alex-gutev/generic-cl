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

(defmethod make-doseq list ((type t) (var symbol) form args body env)
  (declare (ignore env))

  (with-gensyms (list)
    (values
     `((,list ,form))

     `(progn
        (unless ,list
          (end-doseq))

        (let ((,var (car ,list)))
         ,body
         (setf ,list (cdr ,list))))

     nil)))

(defmethod make-doseq list ((type t) (pattern list) form args body env)
  (with-gensyms (item)
    (-<> `(destructuring-bind ,pattern ,item ,body)
         (make-doseq type item form args <> env))))


;;; Vectors

(defmethod make-doseq vector ((type t) (var symbol) form args body env)
  (declare (ignore env))

  (with-gensyms (vec index length)
    (values
     `((,vec ,form)
       (,index 0)
       (,length (cl:length ,vec)))

     `(progn
        (unless (cl:< ,index ,length)
          (end-doseq))

        (let ((,var (aref ,vec ,index)))
          ,body
          (incf ,index)))

     nil)))

(defmethod make-doseq vector ((type t) (pattern list) form args body env)
  (with-gensyms (item)
    (-<> `(destructuring-bind ,pattern ,item ,body)
         (make-doseq type item form args <> env))))


;;; Hash-Tables

(defmethod make-doseq hash-table ((type t) (var symbol) form args body env)
  (with-gensyms (key value)
    (-<> `(let ((,var (cons ,key ,value))) ,body)
         (make-doseq type (cons key value) form args <> env))))

(defmethod make-doseq hash-table ((type t) (pattern list) form args body env)
  (declare (ignore env))

  (with-gensyms (more? next)
    (values
     nil

     (match pattern
       ((cons (and (type symbol) key)
              (and (type symbol) value))

        (let ((key (or key (gensym "KEY")))
              (value (or value (gensym "VALUE"))))

          `(multiple-value-bind (,more? ,key ,value)
               (,next)

             (declare (ignorable ,key ,value))

             (unless ,more?
               (end-doseq))

             ,body)))

       (_
        (with-gensyms (key value)
          `(multiple-value-bind (,more? ,key ,value)
               (,next)

             (unless ,more?
               (end-doseq))

             (destructuring-bind ,pattern (cons ,key ,value)
               ,body)))))

     `(with-hash-table-iterator (,next ,form) (&body)))))


;;; Default

(defmethod make-doseq t ((type t) (var symbol) form args body env)
  (declare (ignore env))

  (with-gensyms (it)
    (values
     `((,it (iterator ,form ,@args)))

     `(progn
        (when (endp ,it)
          (end-doseq))

        (let ((,var (at ,it)))
         ,body
         (advance ,it)))

     nil)))

(defmethod make-doseq t ((type t) (pattern list) form args body env)
  (with-gensyms (item)
    (-<> `(destructuring-bind ,pattern ,item ,body)
         (make-doseq type item form args <> env))))
