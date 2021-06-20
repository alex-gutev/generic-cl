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


;;; Hash-Tables

(defmethod make-doseq hash-table ((type t) (var symbol) form args body env)
  (with-gensyms (key value)
    (-<> `(let ((,var (cons ,key ,value))) ,body)
         (make-doseq type (cons key value) form args <> env))))

(defmethod make-doseq hash-table ((type t) (pattern list) form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (declare (ignore from-end))

    (with-gensyms (table next more? size index)
      (flet ((make-body (test inc)
               (match pattern
                 ((cons (and (type symbol) key)
                        (and (type symbol) value))

                  (let ((key (or key (gensym "KEY")))
                        (value (or value (gensym "VALUE"))))

                    `(multiple-value-bind (,more? ,key ,value)
                         (,next)

                       (declare (ignorable ,key ,value))

                       (when ,test
                         ,@inc
                         ,body))))

                 (_
                  (with-gensyms (key value)
                    `(multiple-value-bind (,more? ,key ,value)
                         (,next)

                       (when ,test
                         ,@inc
                         (destructuring-bind ,pattern (cons ,key ,value)
                           ,body))))))))

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

              (make-body test inc)

              `(with-hash-table-iterator (,next ,table) (&body)))))

          (nil
           (values
            `((,table ,form)
              (,index ,start)
              (,size (or ,end (hash-table-count ,table))))

            (make-body
             `(and ,more? (cl:< ,index ,size))
             `((cl:incf ,index)))

            `(with-hash-table-iterator (,next ,table) (&body)))))))))


;;; Hash-Maps

(defmethod make-doseq hash-map ((type t) pattern form args body env)
  (multiple-value-bind (bindings body parent)
      (make-doseq 'hash-table pattern `(hash-map-table ,form) args body env)

    (values
     bindings
     body
     `(with-custom-hash-table
        ,(or parent '(&body))))))
