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

(defmacro iter-macro ((&rest vars) (&rest lambda-list) &body body)
  "Generate a lexical macro definition for WITH-ITER-VALUE/PLACE for an iterator.

   This macro is intended to be used within MAKE-DOSEQ to facilitate
   the definition, by avoiding the need for nested backquotes, of the
   lexical macros, serving as the expansion of WITH-ITER-VALUE and
   WITH-ITER-PLACE for a given iterator type.

   VARS is a list of variables to 'capture' from the lexical scope of
   the ITER-MACRO form. Inside the generated macro definition, a
   symbol-macro is introduced for each variable, by SYMBOL-MACROLET,
   which expands to a QUOTE form which returns the value of the
   variable as it is in the environment where the ITER-MACRO form
   occurs.

   LAMBDA-LIST is the macro lambda-list (not evaluated).

   BODY is the list of body forms of the generated macro. These are
   not evaluated at the time the ITER-MACRO form is evaluated but are
   instead quoted and become the body forms of the generated macro
   definition. The body forms may reference the variables in the
   LAMBDA-LIST and the values of the 'captured' variables listed in
   VARS.

   Returns a lexical macro definition (excluding the name) suitable to
   be returned from MAKE-DOSEQ as the macro definition for the
   iterator's WITH-ITER-VALUE and WITH-ITER-PLACE."

  ``(,',lambda-list
     (symbol-macrolet
         ,(list ,@(loop for var in vars
                     collect ``(,',var ',,var)))

       ,@',body)))


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

       (iter-macro (index end tag iter-value bind-value)
           (pattern &body body)

         `(macrolet ((,iter-value ,@bind-value))
            (,iter-value
             ,pattern

             (unless (< ,index ,end)
               (go ,tag))

             (incf ,index)
             ,@body)))

       (iter-macro (index end tag iter-place bind-place)
           (name more? &body body)

         `(macrolet ((,iter-place ,@bind-place))
            (,iter-place
             ,name
             ,more?

             ,@(if more?
                   `((let ((,more? (and ,more? (< ,index ,end))))
                       (incf ,index)
                       ,@body))

                   `((unless (< ,index ,end)
                       (go ,tag))

                     (incf ,index)
                     ,@body)))))))))

(defun make-traverse-list (form tag body)
  "Generate a TRAVERSE expansion for a unbounded list traversal."

  (with-gensyms (list)
    (values
     `((,list ,form))

     body

     (iter-macro (tag list)
         (pattern &body body)

       (with-destructure-pattern (var pattern)
           (body body)

         `(progn
            (unless ,list
              (go ,tag))

            (let ((,var (car ,list)))
              (setf ,list (cdr ,list))

              ,@body))))

     (iter-macro (tag list)
         (name more? &body body)

       (let ((body
              `(prog1 (progn ,@body)
                 (setf ,list (cdr ,list)))))

         `(symbol-macrolet ((,name (car ,list)))
            ,(if more?
                 `(let ((,more? ,list))
                    ,body)

                 `(progn
                    (unless ,list
                      (go ,tag))

                    ,body))))))))


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

       (iter-macro (tag v-from-end v-start vec end-index index)
           (pattern &body body)

         (with-destructure-pattern (var pattern)
             (body body)

           `(progn
              (unless (if ,v-from-end
                          (>= ,index ,v-start)
                          (< ,index ,end-index))
                (go ,tag))

              (let ((,var (aref ,vec ,index)))
                (if ,v-from-end
                    (decf ,index)
                    (incf ,index))
                ,@body))))

       (iter-macro (tag v-from-end v-start vec end-index index)
           (name more? &body body)

         (let ((test `(if ,v-from-end
                          (>= ,index ,v-start)
                          (< ,index ,end-index)))
               (body `(prog1 (progn ,@body)
                        (if ,v-from-end
                            (decf ,index)
                            (incf ,index)))))

           `(symbol-macrolet ((,name (aref ,vec ,index)))
              ,(if more?
                   `(let ((,more? ,test))
                      ,body)

                   `(progn
                      (unless ,test
                        (go ,tag))

                      ,body)))))))))


;;; Default

(defmethod make-doseq t ((type t) form args tag body env)
  (declare (ignore env))

  (with-gensyms (it)
    (values
     `((,it (iterator ,form ,@args)))

     body

     (iter-macro (it tag)
         (pattern &body body)

       (with-destructure-pattern (var pattern)
           (body body)

         `(progn
            (when (endp ,it)
              (go ,tag))

            (let ((,var (at ,it)))
              (advance ,it)
              ,@body))))

     (iter-macro (it tag)
         (name more? &body body)

       (let ((body `(prog1 (progn ,@body)
                      (advance ,it))))

         `(symbol-macrolet ((,name (at ,it)))
            ,(if more?
                 `(let ((,more? (not (endp ,it))))
                    ,body)

                 `(progn
                    (when (endp ,it)
                      (go ,tag))

                    ,body))))))))
