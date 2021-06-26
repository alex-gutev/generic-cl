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

;;;; DOSEQ Expanders for hash-tables and hash-maps

(in-package :generic-cl.map)


;;; Utilities

(defmacro with-destructure-entry ((key value pattern) (body-var body) &body forms)
  "Like WITH-DESTRUCTURE-PATTERN, except that FORMS should generate
   code which binds the current entry key to KEY and the value to
   VALUE."

  `(make-destructure-pattern
    ,pattern ,body
    (lambda (,key ,value ,body-var)
      ,@forms)))

(defun make-destructure-pattern (pattern body fn)
  (ematch pattern
    ((cons (and (type symbol) key)
           (and (type symbol) value))

     (funcall fn
              (or key (gensym "KEY"))
              (or value (gensym "VALUE"))
              body))

    ((type symbol)
     (with-gensyms (key value)
       (->> `((let ((,pattern (cons ,key ,value)))
                ,@body))
            (funcall fn key value))))

    ((type list)
     (with-gensyms (key value)
       (->> `((destructuring-bind ,pattern (cons ,key ,value)
                ,@body))
            (funcall fn key value))))))


;;; Hash-Tables

(defmethod make-doseq hash-table ((type t) form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (declare (ignore from-end))

    (with-gensyms (table next more? size index place)
      (flet ((make-iter-value (test inc)
               `((pattern &body body)
                 (with-destructure-entry (key value pattern)
                     (body body)

                   `(multiple-value-bind (,',more? ,key ,value)
                        (,',next)
                      (declare (ignorable ,key ,value))

                      (unless ,',test
                        (doseq-finish))

                      ,@',inc
                      ,@body)))))

        (with-constant-values (start end) env
          ((start end)
           (let* ((counted? (or (> start 0) end))

                  (test (if counted?
                            `(and ,more? (cl:< ,index ,size))
                            more?))

                  (inc (when counted?
                         `((cl:incf ,index)))))

             (values
              `((,table ,form)
                ,@(when counted?
                    `((,index ,start)
                      (,size ,(or end `(hash-table-count ,table))))))

              `((with-hash-table-iterator (,next ,table)
                  ,@body))

              (make-iter-value test inc)

              place)))

          (nil
           (values
            `((,table ,form)
              (,index ,start)
              (,size (or ,end (hash-table-count ,table))))

            `((with-hash-table-iterator (,next ,table)
                ,@body))

            (make-iter-value `(and ,more? (cl:< ,index ,size))
                             `((cl:incf ,index)))

            place)))))))


;;; Hash-Maps

(defmethod make-doseq hash-map ((type t) form args body env)
  (multiple-value-bind (bindings body iter-value)
      (make-doseq 'hash-table `(hash-map-table ,form) args body env)

    (values
     bindings

     `((with-custom-hash-table
         ,@body))

     iter-value)))
