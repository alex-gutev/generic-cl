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


;;; Utilities

(defmacro with-destructure-pattern ((var pattern) (body-var body) &body forms)
  "Automatically generate destructuring code if the binding pattern is
   a destructuring-bind pattern.

   The WITH-ITER-VALUE binding pattern, PATTERN, is checked whether it
   is a symbol naming a variable or a list representing a
   DESTRUCTURING-BIND pattern.

   If PATTERN is a list, a variable name is generated, and bound to
   the variable VAR. BODY-VAR is bound to forms which destructure the
   value stored in the variable, wrapping the forms in BODY.

   If PATTERN is a symbol, the variable given by VAR is bound directly
   to that symbol and the variable given by BODY-VAR is bound directly
   to BODY.

   The body forms of the macro FORMS are evaluated in an implicit
   PROGN, with the bindings to the variables given by VAR and BODY-VAR
   visible. The return value of the last form is returned.

   FORMS should generate code which binds the current sequence element
   to the variable with name stored in VAR."

  `(make-destructure-pattern
    ,pattern ,body
    (lambda (,var ,body-var)
      ,@forms)))

(defun make-destructure-pattern (pattern body fn)
  (etypecase pattern
    (list
     (with-gensyms (var)
       (->> `((destructuring-bind ,pattern ,var
                ,@body))
            (funcall fn var))))

    (symbol
     (funcall fn pattern body))))


;;; Lists

(defmethod make-doseq list ((type t) form args tag body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-constant-values (from-end start end) env
      ((from-end start end)
       (cond
         (from-end
          (make-traverse-list
           `(cl:nreverse (cl:subseq ,form ,start ,end))
           tag
           body))

         (end
          (make-traverse-bounded-list form start end tag body))

         ((> start 0)
          (make-traverse-list `(nthcdr ,start ,form) tag body))

         (t
          (make-traverse-list form tag body))))

      ((start end)
       (cond
         (end
          (make-traverse-list
           `(if ,from-end
                (cl:nreverse (cl:subseq ,form ,start ,end))
                (cl:subseq ,form ,start ,end))
           tag
           body))

         ((> start 0)
          (make-traverse-list
           `(if ,from-end
                (cl:reverse (nthcdr ,start ,form))
                (nthcdr ,start ,form))
           tag
           body))

         (t
          (make-traverse-list
           `(if ,from-end
                (cl:reverse ,form)
                ,form)
           tag
           body))))

      (nil
       (make-traverse-list
        `(if ,from-end
             (cl:nreverse (cl:subseq ,form ,start ,end))
             (cl:subseq ,form ,start ,end))
        tag
        body)))))

(defun make-traverse-bounded-list (form start end tag body)
  "Generate a TRAVERSE expansion for a bounded list traversal, when END is non-NIL."

  (multiple-value-bind (bindings body bind-value bind-place)
      (make-traverse-list
       `(nthcdr ,start ,form)
       tag
       body)

    (with-gensyms (index iter-value iter-place)
      (values
       `((,index ,start) ,@bindings)

       body

       `((pattern &body body)
         `(macrolet ((,',iter-value ,@',bind-value))
            (,',iter-value
             ,pattern

             (unless (< ,',index ,',end)
               (go ,',tag))

             (incf ,',index)
             ,@body)))

       `((name &body body)
         `(macrolet ((,',iter-place ,@',bind-place))
            (,',iter-place
             ,name
             (prog1 (progn,@body)
               (unless (< (incf ,',index) ,',end)
                 (go ,',tag))))))))))

(defun make-traverse-list (form tag body)
  "Generate a TRAVERSE expansion for a unbounded list traversal."

  (with-gensyms (list)
    (values
     `((,list ,form))

     body

     `((pattern &body body)
       (with-destructure-pattern (var pattern)
           (body body)

         `(progn
            (unless ,',list
              (go ,',tag))

            (let ((,var (car ,',list)))
              (setf ,',list (cdr ,',list))

              ,@body))))

     `((name &body body)
       `(symbol-macrolet ((,name (car ,',list)))
          (prog1 (progn ,@body)
            (unless (setf ,',list (cdr ,',list))
              (go ,',tag))))))))


;;; Vectors

(defmethod make-doseq vector ((type t) form args tag body env)
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

       `((pattern &body body)
         (with-destructure-pattern (var pattern)
             (body body)

           `(progn
              (unless (if ,',v-from-end
                          (cl:>= ,',index ,',v-start)
                          (cl:< ,',index ,',end-index))
                (go ,',tag))

              (let ((,var (aref ,',vec ,',index)))
                (if ,',v-from-end
                    (cl:decf ,',index)
                    (cl:incf ,',index))
                ,@body))))

       `((name &body body)
         `(symbol-macrolet ((,name (aref ,',vec ,',index)))
            (prog1 (progn ,@body)
              (unless (if ,',from-end
                          (cl:>= ,',index ,',start)
                          (cl:< ,',index ,',end))
                (go ,',tag)))))))))


;;; Default

(defmethod make-doseq t ((type t) form args tag body env)
  (declare (ignore env))

  (with-gensyms (it)
    (values
     `((,it (iterator ,form ,@args)))

     body

     `((pattern &body body)
       (with-destructure-pattern (var pattern)
           (body body)

         `(progn
            (when (endp ,',it)
              (go ,',tag))

            (let ((,var (at ,',it)))
              (advance ,',it)
              ,@body))))

     `((name &body body)
       `(symbol-macrolet ((,name (at ,',it)))
          (prog1 (progn ,@body)
            (advance ,',it)
            (when (endp ,',it)
              (go ,',tag))))))))
