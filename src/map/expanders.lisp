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

(defmethod make-doseq hash-table ((type t) form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (declare (ignore from-end))

    (with-gensyms (table next more? size index)
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

            `(lambda (pattern body)
               (bind-hash-table-element pattern body ',next ',more? ',test ',inc)))))

        (nil
         (values
          `((,table ,form)
            (,index ,start)
            (,size (or ,end (hash-table-count ,table))))

          `((with-hash-table-iterator (,next ,table)
              ,@body))

          `(lambda (pattern body)
             (bind-hash-table-element
              pattern body
              ',next ',more?
              '(and ,more? (cl:< ,index ,size))
              '((cl:incf ,index))))))))))

(defun bind-hash-table-element (pattern body next more? test inc)
  "Generate a form which binds the next element of a hash-table.

   PATTERN is the binding pattern.

   BODY is the list of forms to be evaluated, within the environment
   of the binding.

   NEXT is the hash-table iterator next element macro.

   MORE? is the name of the variable to which the more elements flag
   should be bound.

   TEST is the loop termination test form.

   INC is the list of index incrementation forms."

  (ematch pattern
    ((cons (and (type symbol) key)
           (and (type symbol) value))

     (let ((key (or key (gensym "KEY")))
           (value (or value (gensym "VALUE"))))

       `(multiple-value-bind (,more? ,key ,value)
            (,next)

          (unless ,test
            (doseq-finish))

          ,@body)))

    ((type symbol)
     (with-gensyms (key value)
       (bind-hash-table-element
        (cons key value)

        `((let ((,pattern (cons ,key ,value)))
            ,@body))

        next more? test inc)))

    ((type list)
     (with-gensyms (key value)
       (bind-hash-table-element
        (cons key value)

        `((destructuring-bind ,pattern (cons ,key ,value)
            ,@body))

        next more? test inc)))))


;;; Hash-Maps

(defmethod make-doseq hash-map ((type t) form args body env)
  (multiple-value-bind (bindings body bind-value)
      (make-doseq 'hash-table `(hash-map-table ,form) args body env)

    (values
     bindings

     `((with-custom-hash-table
         ,@body))

     bind-value)))
