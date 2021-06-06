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

(defun constant-form-value (form env)
  "Return the value of a form if it is a constant.

   FORM is a form.

   ENV is the environment in which FORM is found.

   Returns two values:

    1. The constant value if the form is constant in the
       environment. Otherwise is FORM itself.

    2. True if the form is constant, otherwise is NIL."

  (labels ((simplify-type (type)
             (match type
               ((list 'member value)
                `(eql ,value))

               (_ type))))

    (if (constantp form env)
        (match (simplify-type (nth-form-type form env 0 t))
          ((list 'eql value)
           (values value t))

          ((eql 'null)
           (values nil t)))

        (values form nil))))

(defun process-iterator-args (args env)
  "Extract the values of the ITERATOR arguments.

   Extracts the FROM-END, START and END arguments interpreted as they
   are in the ITERATOR function. Checks whether the arguments evaluate
   to constant values and returns those constants.

   ARGS is the argument list.

   ENV is the environment in which they occur.

   Returns the following values:

    1. Value of FROM-END argument.

    2. True if the FROM-END argument is a constant, NIL otherwise.

    3. Value of START argument.

    4. True if the START argument is a constant, NIL otherwise.

    5. Value of END argument.

    6. True if the END argument is a constant, NIL otherwise."

  (destructuring-bind (&key from-end (start 0) (end nil))
      args

    (multiple-value-bind (from-end constant-from-end?)
        (constant-form-value from-end env)

      (multiple-value-bind (start constant-start?)
          (constant-form-value start env)

        (multiple-value-bind (end constant-end?)
            (constant-form-value end env)

          (values
           from-end
           constant-from-end?
           start
           constant-start?
           end
           constant-end?))))))

(defmacro oif (cond if-true &optional if-false &environment env)
  "Optimized IF expression.

   If the condition form is a constant the IF expression is replaced
   with the appropriate branch.

   COND is the IF expression condition form.

   IF-TRUE is the form to return if COND evaluates to true.

   IF-FALSE is the form to return if COND evaluates to false."

  (multiple-value-bind (cond const?)
      (constant-form-value cond env)

    (if const?
        (if cond if-true if-false)
        `(if ,cond ,if-true ,if-false))))

(defun make-iter-sequence (seq args)
  "Generate a form which process the ITERATOR arguments.

   The generated form extracts the subsequence and reverses it if
   specified by the arguments.

   SEQ is a form which references the sequence.

   ARGS is the argument list, interpreted as the argument list to the
   ITERATOR function."

  (destructuring-bind (&key from-end (start 0) (end nil))
      args

    (let ((bounded `(cl:subseq ,seq ,start ,end)))
      `(if ,from-end
           (cl:reverse ,bounded)
           ,bounded))))

;;; Lists

(defmethod make-doseq list ((type t) (var symbol) form args body env)
  (labels ((optimize-seq (form)
             (match form
               ((list 'if cond true false)
                (multiple-value-bind (value const?)
                    (constant-form-value cond env)

                  (if const?
                      (optimize-seq
                       (if value true false))
                      form)))

               ((list 'cl:subseq seq start end)
                (multiple-value-bind (end c-end?)
                    (constant-form-value end env)

                  (if (and c-end? (null end))
                      (optimize-seq `(nthcdr ,start ,seq))
                      form)))

               ((list 'nthcdr start seq)
                (multiple-value-bind (start const?)
                    (constant-form-value start env)

                  (if (and const? (zerop start))
                      seq
                      form)))

               ((list 'cl:reverse seq)
                `(cl:reverse ,(optimize-seq seq)))

               (_ form))))

    (with-gensyms (list)
      (let ((form (-> (make-iter-sequence form args)
                      optimize-seq)))

        (values
         `((,list ,form))

         `(when ,list
            (let ((,var (car ,list)))
              (setf ,list (cdr ,list))
              ,body))

         nil)))))

(defmethod make-doseq list ((type t) (pattern list) form args body env)
  (with-gensyms (item)
    (-<> `(destructuring-bind ,pattern ,item ,body)
         (make-doseq type item form args <> env))))


;;; Vectors

(defmethod make-doseq vector ((type t) (var symbol) form args body env)
  (multiple-value-bind (from-end c-from-end? start c-start? end c-end?)
      (process-iterator-args args env)

    (let ((v-from-end (if c-from-end? from-end (gensym "FROM-END")))
          (v-start (if c-start? start (gensym "START")))
          (v-end (if c-end? end (gensym "END"))))

      (with-gensyms (vec index end-index)
        (values
         `((,vec ,form)

           ,@(unless c-from-end?
               `((,v-from-end ,from-end)))

           ,@(unless c-start?
               `((,v-start ,start)))

           ,@(unless c-end?
               `((,v-end ,end)))

           (,end-index
            (oif ,v-end ,v-end (cl:length ,vec)))

           (,index
            (oif ,v-from-end ,end-index ,v-start)))

         `(when (oif ,v-from-end
                     (cl>= ,index ,start)
                     (cl:< ,index ,end-index))

            (let ((,var (aref ,vec ,index)))
              (oif ,v-from-end
                   (cl:decf ,index)
                   (cl:incf ,index))
              ,body))

         nil)))))

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

             (when ,more?
               ,body))))

       (_
        (with-gensyms (key value)
          `(multiple-value-bind (,more? ,key ,value)
               (,next)

             (when ,more?
               (destructuring-bind ,pattern (cons ,key ,value)
                 ,body))))))

     `(with-hash-table-iterator (,next ,form) (&body)))))


;;; Default

(defmethod make-doseq t ((type t) (var symbol) form args body env)
  (declare (ignore env))

  (with-gensyms (it)
    (values
     `((,it (iterator ,form ,@args)))

     `(unless (endp ,it)
        (let ((,var (at ,it)))
          (advance ,it)
          ,body))

     nil)))

(defmethod make-doseq t ((type t) (pattern list) form args body env)
  (with-gensyms (item)
    (-<> `(destructuring-bind ,pattern ,item ,body)
         (make-doseq type item form args <> env))))
