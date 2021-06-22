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

(defmethod make-doseq list ((type t) form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-constant-values (from-end start end) env
      ((from-end start end)
       (cond
         (from-end
          (make-traverse-list
           `(cl:nreverse (cl:subseq ,form ,start ,end))
           body))

         (end
          (make-traverse-bounded-list form start end body))

         ((> start 0)
          (make-traverse-list `(nthcdr ,start ,form) body))

         (t
          (make-traverse-list form body))))

      ((start end)
       (cond
         (end
          (make-traverse-list
           `(if ,from-end
                (cl:nreverse (cl:subseq ,form ,start ,end))
                (cl:subseq ,form ,start ,end))
           body))

         ((> start 0)
          (make-traverse-list
           `(if ,from-end
                (cl:reverse (nthcdr ,start ,form))
                (nthcdr ,start ,form))
           body))

         (t
          (make-traverse-list
           `(if ,from-end
                (cl:reverse ,form)
                ,form)
           body))))

      (nil
       (make-traverse-list
        `(if ,from-end
             (cl:nreverse (cl:subseq ,form ,start ,end))
             (cl:subseq ,form ,start ,end))
        body)))))

(defun make-traverse-bounded-list (form start end body)
  "Generate a TRAVERSE expansion for a bounded list traversal, when END is non-NIL."

  (multiple-value-bind (bindings body bind-value)
      (make-traverse-list
       `(nthcdr ,start ,form)
       body)

    (with-gensyms (index)
      (values
       `((,index ,start) ,@bindings)

       body

       `(lambda (pattern body)
          (,bind-value
           pattern

           `((unless (< ,',index ,',end)
               (doseq-finish))


             (incf ,',index)
             ,@body)))))))

(defun make-traverse-list (form body)
  "Generate a TRAVERSE expansion for a unbounded list traversal."

  (with-gensyms (list)
    (values
     `((,list ,form))

     body

     `(lambda (pattern body)
        (bind-list-element pattern body ',list)))))

(defun bind-list-element (pattern body list)
  "Generate a form which binds the next element of a list.

   PATTERN is the binding pattern.

   BODY is the list of forms to be evaluated, within the environment
   of the binding.

   LIST is the name of the variable storing the list."

  (etypecase pattern
    (list
     (with-gensyms (var)
       (bind-list-element
        var
        `((destructuring-bind ,pattern ,var
            ,@body))
        list)))

    (symbol
     `(progn
        (unless ,list
          (doseq-finish))

        (let ((,pattern (car ,list)))
          (setf ,list (cdr ,list))
          ,@body)))))


;;; Vectors

(defmethod make-doseq vector ((type t) form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-gensyms (vec index end-index v-from-end v-start v-end)
      (values
       `((,v-from-end ,from-end :constant t)
         (,v-start ,start :constant t)
         (,v-end ,end :constant t)

         (,vec ,form)
         (,end-index (if ,v-end ,v-end (cl:length ,vec)) :constant t)

         (,index
          (if ,v-from-end (cl:1- ,end-index) ,v-start)))

       body

       `(lambda (pattern body)
          (bind-vector-element pattern body ',vec ',index ',v-start ',end-index ',v-from-end))))))

(defun bind-vector-element (pattern body vec index start end from-end)
  "Generate a form which binds the next element of a vector.

   PATTERN is the binding pattern.

   BODY is the list of forms to be evaluated, within the environment
   of the binding.

   VEC is the name of the variable storing the vector.

   INDEX is the name of the variable/symbol-macro storing the element
   index.

   START is the name of the variable/symbol-macro storing the starting
   index.

   END is the name of the variable/symbol-macro storing the ending
   index.

   FROM-END is the name of the variable/symbol-macro storing the
   FROM-END flag."

  (etypecase pattern
    (list
     (with-gensyms (var)
       (bind-vector-element
        var
        `((destructuring-bind ,pattern ,var
            ,@body))

        vec
        index
        start
        end
        from-end)))

    (symbol
     `(progn
        (unless (if ,from-end
                    (cl:>= ,index ,start)
                    (cl:< ,index ,end))
          (doseq-finish))

        (let ((,pattern (aref ,vec ,index)))
            (if ,from-end
                (cl:decf ,index)
                (cl:incf ,index))
            ,@body)))))


;;; Default

(defmethod make-doseq t ((type t) form args body env)
  (declare (ignore env))

  (with-gensyms (it)
    (values
     `((,it (iterator ,form ,@args)))

     body

     `(lambda (pattern body)
        (bind-iter-element pattern body ',it)))))

(defun bind-iter-element (pattern body iter)
  "Generate a form which binds the next element of a generic sequence.

   PATTERN is the binding pattern.

   BODY is the list of forms to be evaluated, within the environment
   of the binding.

   ITER is the variable storing the iterator object."

  (etypecase pattern
    (list
     (with-gensyms (var)
       (bind-iter-element
        var
        `((destructuring-bind ,pattern ,var
            ,@body))

        iter)))

    (symbol
     `(progn
        (when (endp ,iter)
          (doseq-finish))

        (let ((,pattern (at ,iter)))
          (advance ,iter)
          ,@body)))))
