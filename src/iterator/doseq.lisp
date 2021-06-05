;;;; doseq.lisp
;;;;
;;;; Copyright 2018 Alexander Gutev
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

;;;; DOITER, DOITERS, DO-SEQUENCES and DOSEQ macros

(in-package :generic-cl.iterator)


;;; DOITER macros

(defmacro doiters (name/iters &body body)
  "Iterate over one or more sequences with the iterator of each sequence bound to a variable.

   The macro arguments can be in one of the following two forms:

    1. (NAME (&REST ITERS) &BODY BODY)

    2. ((&REST ITERS) &BODY BODY)


   NAME is a symbol serving as the name of the BLOCK from which the
   forms in BODY can return, using (RETURN-FROM NAME ...). If not
   given defaults to NIL.

   ITERS is a list of bindings with each element of the form (IT-VAR
   SEQUENCE . ARGS). IT-VAR is the variable to which the iterator for
   SEQUENCE is bound. ARGS are the remaining arguments passed to the
   ITERATOR function (if any).

   BODY is a list of forms which are evaluated at each iteration. The
   bindings to the iterator variables (IT-VAR's) are visible. At the
   end of each iteration, each iterator is advanced by one position
   with ADVANCE. The loop is terminated, with the DOITERS form
   returning NIL, when at least one of the iterators reaches the end
   of its sequence (ENDP returns true). The loop can be terminated
   early with a RETURN-FROM to the block named NAME."

  (let-if ((name name/iters)
           (iters (first body) name/iters)
           (body (rest body) body))
      (symbolp name/iters)

    (flet ((make-it-binding (it)
	     (destructuring-bind (var &rest args) it
	       `(,var (iterator ,@args))))

	   (make-end-test (it)
	     `(endp ,(car it)))

	   (make-advance (it)
	     `(advance ,(car it))))

      `(let ,(mapcar #'make-it-binding iters)
         (loop named ,name
            until (or ,@(mapcar #'make-end-test iters))
	    do
	      (progn ,@body)
	      ,@(mapcar #'make-advance iters))))))

(defmacro doiter (name/iter &body body)
  "Sames as DOITERS however for the special case of iterating over a
   single sequence.

   The macro arguments can be in one of the following two forms:

    1. (NAME (ITER &REST ARGS) &BODY BODY)

    2. ((ITER &REST ARGS) &BODY BODY)

   NAME is the block name, NIL if not given.

   ITER is the variable to which the sequence iterator is bound.

   ARGS are the arguments passed to the ITERATOR function, the result
   of which is bound to ITER.

   BODY is the list of forms in the loop body."

  (let-if ((name name/iter)
           (iter (first body) name/iter)
           (body (rest body) body))
      (symbolp name/iter)

    (destructuring-bind (iter &rest args) iter
      `(doiters ,name ((,iter ,@args)) ,@body))))


;;; DOSEQ Macros

;;;; Optimizations

(define-method-combination subtype ()
  ((methods * :required t))
  (:arguments type)

  "Dispatch based on a type keyword given as a qualifier.

   With this method combination each method includes a single
   qualifier, which is interpreted as a type specifier symbol.

   The method of which the value given in the first argument,
   interpreted as a type specifier, is a subtype of the method's
   qualified type specifier, given in the first qualifier, is called.

   The methods are ordered such that the methods qualified with the
   most derived type, i.e. a type which is a subtype of the others,
   are called first."

  (labels ((type-qualifier (method)
             (first (method-qualifiers method)))

           (type< (t1 t2)
             (subtypep t1 t2))

           (make-if (method else)
             `(if (subtypep ,type ',(type-qualifier method))
                  (call-method ,method)
                  ,else)))

    (-<> (copy-list methods)
         (sort #'type< :key #'type-qualifier)
         (reduce #'make-if <>
                 :from-end t
                 :initial-value
                 `(method-combination-error "No method for type ~a" ,type)))))

(defgeneric make-doseq (type var seq args env)
  (:method-combination subtype)

  (:documentation
   "Generate iteration code for DOSEQ over a sequence of a given type.

    This method has the SUBTYPE method combination meaning each method
    has a single qualifier which is interpreted as a type specifier
    symbol. The method with the most derived type, which is a subtype
    of the type specifier given in TYPE, is called.

    TYPE is a the type specifier of the sequence, as determined from
    the environment.

    VAR is a symbol naming the variable to which successive elements
    of the sequence are bound. VAR may also be a list in which case it
    should be interpreted as a destructuring pattern (as if to
    DESTRUCTURING-BIND).

    SEQ is the form which returns the sequence over which to iterate.

    ARGS are the additional arguments passed after the sequence which
    should be interpreted as they are interpreted by the ITERATOR
    function.

    ENV is the environment in which the DO-SEQUENCES/DOSEQ form is
    found.

    Methods of this function should return the following values:

     1. A list of bindings which are established before the first
        iteration and remain visible to the body forms throughout all
        iterations.

        Each element is a list of the form (VAR INIT-FORM) where VAR is the
        symbol naming the variable and INIT-FORM is the form to the
        value of which, VAR is bound.

     2. A list of bindings which are established before each
        iteration.

        Each element is a list of the form (VAR INIT-FORM STEP-FORM)
        where VAR is the symbol naming the variable, INIT-FORM is the
        form to the value of which, VAR is bound before the first
        iteration, and STEP-FORM is the form to the value of which,
        VAR is bound before the subsequent iterations. STEP-FORM is
        reevaluated at each iteration following the first. If
        STEP-FORM is omitted, VAR is bound to the evaluation of
        INIT-FORM (reevaluated at each iteration) at each iteration.

     3. The condition form. This is evaluated before each iteration,
        and when it evaluates to false (NIL), the loop is terminated.

     4. A list of additional CL:LOOP keywords, which are inserted
        between the bindings and the condition form.

     5. A form to use as the body form executed at each iteration. If
        NIL the body forms provided to the DOSEQ/DO-SEQUENCES macro
        are used."))

(defun expand-doseq (var seq args env)
  "Generate binding code for a given sequence

   VAR is the variable/pattern to which successive elements of the
   sequence should be bound.

   SEQ is the form which returns the sequence.

   ARGS is the list of additional arguments passed after the sequence,
   interpreted the same as arguments to the ITERATOR function.

   ENV is the environment in which the DOSEQ/DO-SEQUENCES form is
   found."

  (-> (nth-form-type seq env)
      (doseq-expansion var seq args env)))

(defun doseq-expansion (type var form args env)
  (flet ((make-with-binding (binding)
           (destructuring-bind (var init) binding
             `(with ,var = ,init)))

         (make-for-binding (binding)
           (destructuring-bind (var init &optional (step nil step-p))
               binding

             `(for ,var = ,init
                   ,@(when step-p
                       `(then ,step))))))

   (multiple-value-bind (with-bindings for-bindings condition keywords body)
       (make-doseq type var form args env)

     (values
      (append
       (mappend #'make-with-binding with-bindings)
       (mappend #'make-for-binding for-bindings)
       keywords
       (when condition
         `(while ,condition)))

      body))))


;;;; DO-SEQUENCES Macro

(defmacro do-sequences (name/seqs &body forms &environment env)
  "Iterate over the elements of one or more sequences.

   Iteration is performed using the iterator interface, and over
   multiple sequences simultaneously.

   The macro arguments can be in one of the following two forms:

    1. (NAME (&REST SEQS) &BODY BODY)

    2. ((&REST SEQS) &BODY BODY)

   NAME is a symbol serving as the name of the BLOCK from which the
   forms in BODY can return, using (RETURN-FROM NAME ...). If not
   given defaults to NIL.

   Each element of SEQS is a list of the form (VAR SEQUENCE . ARGS),
   where VAR is the variable to which the element of the sequence,
   SEQUENCE, is bound, at each iteration, and ARGS are arguments
   passed to the ITERATOR function, after the sequence.

   BODY is a list of forms evaluated at each iteration. RETURN-FROM to
   the block named NAME may be used to terminate the iteration early
   and return a value from the DO-SEQUENCES form. NIL is returned if
   there is no explicit RETURN."

  (labels ((make-&body (old new)
             (if new
                 `(macrolet ((&body ()
                               ,(make-&body-expansion old)))

                    ,new)
                 old))

           (make-&body-expansion (old)
             (if old
                 ``,',old
                 ``(progn ,@',forms))))

    (let-if ((name name/seqs)
             (seqs (first forms) name/seqs)
             (forms (rest forms) forms))
        (symbolp name/seqs)

      (loop
         for (var seq . args) in seqs
         for (keywords body) =
           (-> (nth-form-type seq env)
               (doseq-expansion var seq args env)
               multiple-value-list)

         for loop-body = (make-&body nil body)
         then (make-&body loop-body body)

         append keywords into loop-keywords
         finally
           (return
             `(loop named ,name ,@loop-keywords do
                   ,@(if loop-body
                         (list loop-body)
                         forms)))))))

(defmacro do-sequences% (name (&rest seqs) &body body)
  "Expansion of DO-SEQUENCES into DOITERS without optimization.

   This macro is meant to be used internally when an optimization
   policy of (SAFETY 3) is used."

  (flet ((bind-elem (elem iter body)
           (match elem
             ((type list)
              `((destructuring-bind ,elem (at ,iter)
                  ,@body)))

             (_
              `((let ((,elem (at ,iter)))
                  ,@body))))))

    (let ((iters (make-gensym-list (cl:length seqs) "ITER")))
      `(doiters ,name
         ,(loop
             for (nil . seq) in seqs
             for iter in iters
             collect `(,iter ,@seq))

         ,@(loop
              for (elem) in seqs
              for iter in iters
              for forms = (bind-elem elem iter body)
              then (bind-elem elem iter forms)
              finally (return forms))))))

;;;; DOSEQ

(defmacro doseq (name/seq &body body)
  "Iterate over the elements of a single sequence SEQUENCE.

   Same as DO-SEQUENCES but for the special case of iterating over a
   single sequence.

   The macro arguments can be in one of the following two forms:

    1. (NAME (ELEMENT SEQUENCE &REST ARGS) &BODY BODY)

    2. ((ELEMENT SEQUENCE &REST ARGS) &BODY BODY)

   NAME is the block name, NIL if not given.

   ELEMENT is the variable to which each element of the sequence is
   bound.

   SEQUENCE is the sequence, with ARGS being the remaining arguments
   passed to the ITERATOR function.

   BODY is the list of forms evaluated on each iteration."

  (let-if ((name name/seq)
           (seq (first body) name/seq)
           (body (rest body) body))
      (symbolp name/seq)

    (destructuring-bind (element sequence &rest args) seq
      `(do-sequences ,name
         ((,element ,sequence ,@args))
         ,@body))))
