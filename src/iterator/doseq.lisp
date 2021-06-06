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

(defgeneric make-doseq (type var seq args body env)
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

    BODY is the form which should be executed at each iteration with
    the given bindings visible to it. The last form in BODY is
    typically transfers control back to the start of the loop, to
    perform the next iteration. Thus a method can control whether the
    next iteration is executed, or the loop terminates by wrapping
    this form in a conditional.

    ENV is the environment in which the DO-SEQUENCES/DOSEQ form is
    found.

    Methods of this function should return the following values:

     1. A list of bindings, as by LET*, which are established before
        the first iteration and remain visible to the body forms
        throughout all iterations.

     3. The new body form to be evaluated at each iteration.

     4. A form to wrap the entire iteration code. The local
        macro (&BODY) is available to it which expands to the entire
        iteration code."))


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

  (let-if ((name name/seqs)
           (seqs (first forms) name/seqs)
           (forms (rest forms) forms))
      (symbolp name/seqs)

    (let ((safety (cadr (assoc 'safety (declaration-information 'optimize env)))))
      `(,(if (> safety 2)
             'do-sequences-safe%
             'do-sequences-fast%)

         ,name
         ,seqs
         ,@forms))))

(defmacro do-sequences-fast% (name (&rest seqs) &body forms &environment env)
  "Optimized expansion of DO-SEQUENCES.

   Generates optimized iteration code for the sequence types using
   MAKE-DOSEQ."

  (labels ((expand-doseq (var seq args body env)
             (-> (nth-form-type seq env)
                 (make-doseq var seq args body env)
                 multiple-value-list))

           (wrap-parent (old new)
             (if new
                 `(macrolet ((&body ()
                               ,(make-&body-expansion old)))

                    ,new)
                 old))

           (make-&body-expansion (old)
             (if old
                 `',old
                 ''(loop-body)))

           (make-loop (bindings body parent)
             (->> (make-tagbody body)
                  (make-block)
                  (make-parent parent)
                  (make-bindings bindings)))

           (make-parent (parent body)
             (if parent
                 `(macrolet ((loop-body ()
                                ',body))
                    ,parent)
                 body))

           (make-tagbody (body)
             (with-gensyms (start)
               `(tagbody
                   ,start
                   (macrolet ((next-iter ()
                                `(go ,',start)))
                     ,body))))

           (make-block (body)
             `(block ,name
                ,body))

           (make-bindings (bindings body)
             `(let* ,bindings ,body)))

    (loop
       for (var seq . args) in seqs
       for (bindings body parent) =
         (expand-doseq var seq args `(progn ,@forms (next-iter)) env) then
         (expand-doseq var seq args loop-body env)

       for loop-body = body
       for loop-parent = (wrap-parent loop-parent parent)

       append bindings into all-bindings

       finally
         (return
           (make-loop all-bindings loop-body loop-parent)))))

(defmacro do-sequences-safe% (name (&rest seqs) &body body)
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
