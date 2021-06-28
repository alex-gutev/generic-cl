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

(defgeneric make-doseq (type seq args tag body env)
  (:method-combination subtype)

  (:documentation
   "Generate the WITH-ITERATORS expansion for a sequence of a given type.

    This method has the SUBTYPE method combination meaning each method
    has a single qualifier which is interpreted as a type specifier
    symbol. The method with the most derived type, which is a subtype
    of the type specifier given in TYPE, is called.

    TYPE is the type specifier of the sequence, as determined from the
    environment.

    SEQ is the form which returns the sequence over which to iterate.

    ARGS are the additional arguments passed after the sequence which
    should be interpreted as they are interpreted by the ITERATOR
    function.

    TAG is the name of the tag (in TAGBODY) to which a non-local jump
    should be performed, by GO, when the end of the container is
    reached.

    BODY is list of forms comprising the body of the WITH-ITERATORS
    form.

    ENV is the environment in which the DO-SEQUENCES/DOSEQ form is
    found.

    Methods of this function should return the following values:

     1. A list of bindings, as by LET*, which are established before
        the first iteration and remain visible to the body forms
        throughout all iterations.

        Each binding, in this list, may optionally provide the
        following keyword arguments following the init-form:

        :CONSTANT

            Flag for whether the variable should be treated as a
            constant. If true and the init-form is a constant-form,
            the symbol is bound by SYMBOL-MACROLET, preceding the
            non-constant bindings, rather than LET.

            NOTE: A constant binding may only reference other bindings
            for which this flag is true.

     2. The new body of the WITH-ITERATORS form.

     3. A lexical macro definition defining the expansion of
        WITH-ITER-VALUE for the sequence's iterator.

        This should be a list of the form:

           (LAMBDA-LIST . BODY)

        where LAMBDA-LIST is the macro lambda-list and BODY is the
        macro definition body. A name should not be provided as a name
        for the macro is generated.

        The lambda-list should have the following arguments:

           (PATTERN &BODY BODY)

        where PATTERN is the binding pattern, corresponding to the
        PATTERN argument of WITH-ITER-VALUE, describing which
        variable(s) to bind to the value of current sequence element.

        This may either be a symbol, naming a variable or a list which
        should be interpreted as a destructuring-bind pattern.

        BODY is the list of body forms of the WITH-ITER-VALUE form,
        corresponding to the BODY argument.

        The macro should expand to a form which binds the current
        sequence element to the variable(s) specified in PATTERN,
        advances the position of the iterator to the next element in
        the sequence, and evaluates the body forms.

        The expansion should jump out of the WITH-ITERATORS form,
        using a GO to the tag name given in the TAG argument.

     4. A lexical macro definition defining the expansion of
        WITH-ITER-PLACE for the sequence's iterator.

        This should be a list of the form:

           (LAMBDA-LIST . BODY)

        where LAMBDA-LIST is the macro lambda-list and BODY is the
        macro definition body. A name should not be provided as a name
        for the macro is generated.

        The lambda-list should have the following arguments:

          (NAME MOREP &BODY FORMS)

        where NAME is the name of the symbol-macro to be introduced,
        expanding to the 'place' of the current sequence element,
        corresponding to the NAME argument of WITH-ITER-PLACE.

        MOREP corresponds to the MOREP argument of WITH-ITER-PLACE
        which is the name of the variable which should be bound to
        true if there are more elements in the sequence and bound to
        NIL if there are no more elements. If MOREP is NIL, the
        expansion should jump out of the WITH-ITERATORS form, skipping
        the evaluation of FORMS, using a GO to the tag name given in
        the TAG argument.

        FORMS are the body forms of the WITH-ITER-PLACE form,
        corresponding to the FORMS argument of WITH-ITER-PLACE.

        If this return value is NIL it is assumed the sequence is
        immutable, and any uses of WITH-ITER-PLACE on it will result
        in an error being signalled."))


;;;; Local WITH-ITER-VALUE and WITH-ITER-PLACE expansions

(defun iter-value-macros (env)
  "Retrieve the association list mapping iterator identifiers to the
   names of the macros serving as the expansion of WITH-ITER-VALUE
   when given those iterator identifiers. This information is
   retrieved from the environmnet ENV by macroexpanding the
   symbol-macro ITER-VALUE-MACROS."

  (multiple-value-bind (macros expanded?)
      (macroexpand 'iter-value-macros env)

    (when expanded?
      macros)))

(defun iter-value-places (env)
  "Retrieve the association list mapping iterator identifiers to the
   names of the macros serving as the expansion of WITH-ITER-PLACE
   when given those iterator identifiers. This information is
   retrieved from the environmnet ENV by macroexpanding the
   symbol-macro ITER-VALUE-PLACES."

  (multiple-value-bind (macros expanded?)
      (macroexpand 'iter-value-places env)

    (when expanded?
      macros)))


;;;; WITH-ITERATORS MACRO

(defmacro with-iterators (seqs &body forms &environment env)
  "Create static iterators for one or more sequence.

   This macro attempts to determine the type of each sequence, and calls
   MAKE-DOSEQ to generate optimal static iterator code for the given
   sequence types, rather than creating dynamic iterator objects.

   SEQS:

     A list of the form (ITER SEQUENCE . ARGS) where ITER is a symbol
     identifying the iterator for the sequence, SEQUENCE is the form
     which evaluates to a sequence, and ARGS are the remaining
     iteration arguments, interpreted as they are to the ITERATOR
     function.

     Each iterator identifier (ITER) can be passed to
     WITH-ITER-VALUE, within the body forms of WITH-ITERATORS, to
     retrieve the values of the elements of the sequence.

     The iterator identifiers are in a namespace of their own that is
     they do not name lexical variables/symbol-macros nor functions/macros.


   FORMS:

     A list of forms evaluated in an implicit TAGBODY, thus symbols
     are interpreted as tag names.

     The WITH-ITER-VALUE macro can be used, within FORMS, to retrieve
     the current element of the sequence and advance the iterator to
     the next position.

     NOTE: The value of the last form is not returned, due to it being
     evaluated in a TAGBODY, instead NIL is returned. RETURN-FROM, to
     an outer BLOCK, should be used to return a value from this form.

     NOTE: Whilst the intended use of WITH-ITERATORS is to implement
     iteration macros, such as DOSEQ, the FORMS are only evaluated
     once. It is up to the user to implement the actual loop, using
     the provided TAGBODY facility."

  (with-gensyms (end-tag)
    (labels ((expand-doseq (seq args body env)
               (-> (nth-form-type seq env)
                   (make-doseq seq args end-tag body env)
                   multiple-value-list))

             (make-form (bindings get-values places body)
               (->> (make-macros get-values places body)
                    (make-bindings bindings)))

             (make-macros (get-values places body)
               `(symbol-macrolet
                    ((iter-value-macros
                      ,(append get-values (iter-value-macros env)))

                     (iter-value-places
                      ,(append places (iter-value-places env))))

                  ,@body))

             ;; Tagbody

             (make-tagbody (body)
               `((tagbody
                    ,@body
                    ,end-tag)))


             ;; Bindings

             (make-bindings (bindings body)
               (loop
                  for binding in bindings
                  for (init constant?) = (multiple-value-list (make-binding binding))
                  if constant? collect init into constants
                  else collect init into vars
                  finally
                    (return
                      `(symbol-macrolet ,constants
                         (let* ,vars ,body)))))

             (make-binding (binding)
               (destructuring-bind (var init &key constant) binding
                 (if constant
                     (multiple-value-bind (value constant?)
                         (constant-form-value init env)

                       (values
                        `(,var ,value)
                        constant?))

                     (values
                      `(,var ,init)
                      nil)))))

      (loop
         for (var seq . args) in seqs
         for (bindings body value place) =
           (expand-doseq seq args (make-tagbody forms) env) then
           (expand-doseq seq args form-body env)

         for iter-value = (gensym "ITER-VALUE")
         for iter-place = (gensym "ITER-PLACE")
         for form-body = `((macrolet ((,iter-value ,@value)
                                      (,iter-place
                                          `((&rest args)
                                            (declare (ignore args))
                                            (error "In WITH-ITER-PLACE: Iterator ~s points to immutable sequence." ',var))))
                             ,@body))

         append bindings into all-bindings
         collect (cons var iter-value) into get-values
         collect (cons var iter-place) into places

         finally
           (return
             (make-form all-bindings get-values places form-body))))))

(defmacro with-iter-value ((pattern iter) &body forms &environment env)
  "Bind the current element of a sequence, pointed to by a static iterator, to a variable.

   This macro may only be used within the body of a WITH-ITERATORS
   macro.

   The current value of the sequence, with iterator ITER, is bound to
   the variable(s) specified by PATTERN, with the bindings visible to
   FORMS.

   If the iterator is already at the end of the sequence a non-local
   jump, to the end of the enclosing WITH-ITERATORS form, is
   performed..

   After binding, the iterator is advanced to the next element of the
   sequence.

   PATTERN:

     A binding pattern specifying the variable(s) to which the current
     element of the sequence is bound.

     This may either be a symbol, naming a variable, or a list which
     is interpreted as a pattern to `destructuring-bind`.

   ITER:

     Symbol identifying the iterator, as established by the
     WITH-ITERATOR form.

     This must name an iterator introduced in a parent WITH-ITERATORS
     form.

   FORMS:

     List of forms evaluated in an implicit PROGN. The binding(s) for
     the current element are visible to the forms.

     NOTE: If there are no more elements in the sequence, the FORMS
     are not evaluated and a non-local jump to the end of the
     WITH-ITERATORS form is performed."

  (let ((bind-macros (iter-value-macros env)))
    (unless bind-macros
      (error "Illegal use of WITH-ITER-VALUE outside WITH-ITERATORS."))

    (if-let ((macro (assoc iter bind-macros)))
      (list* (cdr macro) pattern forms)
      (error "In WITH-ITER-VALUE: ~s not one of ~{~s~^ or~} passed to WITH-ITERATORS."
             iter (mapcar #'car bind-macros)))))

(defmacro with-iter-place ((name iter &optional morep) &body forms &environment env)
  "Introduce an identifier serving as a place to the current sequence element.

   This macro may only be used within the body of a WITH-ITERATORS
   macro.

   A symbol-macro, with identifier NAME, is introduced which expands
   to a place, suitable for use with SETF, to the current element of
   the sequence, pointed by the iterator ITER. This symbol-macro is
   visible to the body FORMS of the WITH-ITER-PLACE form.

   If the iterator is already at the end of the sequence a non-local
   jump, to the end of the enclosing WITH-ITERATORS form, is
   performed..

   Simultaneously the iterator is also advanced to the next element of
   the sequence. However, the iterator is only guaranteed to be
   advanced on a normal exit from the WITH-ITER-PLACE form. If a
   non-local jump is used, via GO, RETURN-FROM or THROW, the iterator
   might not be advanced.

   NAME:

     Identifier of the symbol-macro which is introduced.

     NOTE: Unlike in WITH-ITER-VALUE this must be a symbol, and cannot
     be a destructuring-bind pattern.

   MOREP (Optional)

     The name of the variable which is bound to true if there are more
     elements in the sequence, and to NIL when there are no more
     elements in the sequence.

     If NON-NIL it is not checked whether the end of the sequence has
     been reached, and hence the body FORMS are not skipped if the end
     of the sequence has been reached. It is up to the programmer to
     check the value of this variable and perform whatever logic
     should be performed when the end of the sequence has been
     reached.

   ITER:

     Symbol identifying the iterator, as established by the
     WITH-ITERATOR form.

     This must name an iterator introduced in a parent WITH-ITERATORS
     form.

   FORMS:

     List of forms evaluated in an implicit PROGN. The binding(s) for
     the current element are visible to the forms.

     NOTE: If there are no more elements in the sequence, the FORMS
     are not evaluated and a non-local jump to the end of the
     WITH-ITERATORS form is performed."

  (let ((place-macros (iter-value-places env)))
    (unless place-macros
      (error "Illegal use of WITH-ITER-PLACE outside WITH-ITERATORS."))

    (if-let ((macro (assoc iter place-macros)))
      (list* (cdr macro) name morep forms)
      (error "In WITH-ITER-PLACE: ~s not one of ~{~s~^ or~} passed to WITH-ITERATORS."
             iter (mapcar #'car place-macros)))))

(defmacro do-iter-values ((&rest iters) &body forms)
  "Iterate over the remaining elements of a sequence pointed to by static iterators.

   Evaluate FORMS at each iteration, for each remaining element, until
   one of the iterators reaches the end of the sequence.

   NOTE: May only be used inside a WITH-ITERATORS form.

   ITERS:

     List of bindings. Each element is of the form (PATTERN ITER),
     interpreted as if to WITH-ITER-VALUE, where PATTERN is a binding
     pattern, for the binding to the sequence element value, and ITER
     is a static iterator identifier symbol, introduced in a parent
     WITH-ITERATORS form.

   FORMS

     List of forms evaluated for each remaining element in the
     sequences pointed to by the static iterators.

     At each evaluation the next element of each sequence, pointed by
     each ITER in ITERS, is bound to the variable(s) specified by the
     corresponding PATTERN. After which the iterators are advanced to
     the next elements.

   If one of the iterators has reached the end of its sequence, a
   non-local jump outside the enclosing WITH-ITERATORS form is
   performed, i.e. any forms following this form, within the enclosing
   WITH-ITERATORS form, will not be evaluated."

  (flet ((make-iter-value (iter forms)
           (destructuring-bind (pattern iter) iter
             `((with-iter-value (,pattern ,iter)
                 ,@forms)))))

    (with-gensyms (start)
      `(tagbody
          ,start
          ,@(cl:reduce #'make-iter-value iters :initial-value forms :from-end t)
          (go ,start)))))

(defmacro do-iter-places ((&rest iters) &body forms)
  "Like DO-ITER-VALUES but instead of binding the value of each
   sequence element, to variables, by WITH-ITER-VALUE, a symbol-macro
   expanding to the 'place' of the current sequence element, is
   introduced, as if by WITH-ITER-PLACE."

  (flet ((make-iter-place (iter forms)
           (destructuring-bind (var iter) iter
             `((with-iter-place (,var ,iter)
                 ,@forms)))))

    (with-gensyms (start)
      `(tagbody
          ,start
          ,@(cl:reduce #'make-iter-place iters :initial-value forms :from-end t)
          (go ,start)))))


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

(defmacro do-sequences-fast% (name (&rest seqs) &body forms)
  "Optimized expansion of DO-SEQUENCES.

   Generates optimized iteration code for the sequence types using
   MAKE-DOSEQ."

  (flet ((make-iter-binding (iter seq)
           `(,iter ,@(rest seq)))

         (make-value-binding (seq iter)
           `(,(first seq) ,iter)))

    (let ((iters (make-gensym-list (length seqs))))
      `(block ,name
         (with-iterators
             ,(mapcar #'make-iter-binding iters seqs)

           (do-iter-values ,(mapcar #'make-value-binding seqs iters)
             ,@forms))))))

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

;;;; DO-SEQUENCES!

(defmacro do-sequences! (name/seqs &body forms &environment env)
  "Mutable version of DO-SEQUENCES.

   This is the same as DO-SEQUENCES however each NAME, is the name of
   a symbol-macro, that is introduced, which expands to the 'place',
   suitable for use with SETF, of the current sequence element, rather
   than a variable storing its value. This allows the elements of the
   sequence to be mutated.

   NOTE: This macro does not support destructuring of the sequence
   elements."

  (let-if ((name name/seqs)
           (seqs (first forms) name/seqs)
           (forms (rest forms) forms))
      (symbolp name/seqs)

    (let ((safety (cadr (assoc 'safety (declaration-information 'optimize env)))))
      `(,(if (> safety 2)
             'do-sequences!-safe%
             'do-sequences!-fast%)

         ,name
         ,seqs
         ,@forms))))

(defmacro do-sequences!-fast% (name (&rest seqs) &body forms)
  "Optimized expansion of DO-SEQUENCES!.

   Generates optimized iteration code for the sequence types using
   MAKE-DOSEQ."

  (flet ((make-iter-binding (iter seq)
           `(,iter ,@(rest seq)))

         (make-value-binding (seq iter)
           `(,(first seq) ,iter)))

    (let ((iters (make-gensym-list (length seqs))))
      `(block ,name
         (with-iterators
             ,(mapcar #'make-iter-binding iters seqs)

           (do-iter-places ,(mapcar #'make-value-binding seqs iters)
             ,@forms))))))

(defmacro do-sequences!-safe% (name (&rest seqs) &body body)
  "Expansion of DO-SEQUENCES! into DOITERS without optimization.

   This macro is meant to be used internally when an optimization
   policy of (SAFETY 3) is used."

  (let ((iters (make-gensym-list (cl:length seqs) "ITER")))
    `(doiters ,name
       ,(loop
           for (nil . seq) in seqs
           for iter in iters
           collect `(,iter ,@seq))

       (symbol-macrolet
           ,(loop
               for (name) in seqs
               for iter in iters
               collect `(,name (at ,iter)))

         ,@body))))


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

(defmacro doseq! (name/seq &body body)
  "Mutable version of DOSEQ, expanding into DO-SEQUENCES!."

  (let-if ((name name/seq)
           (seq (first body) name/seq)
           (body (rest body) body))
      (symbolp name/seq)

    (destructuring-bind (element sequence &rest args) seq
      `(do-sequences! ,name
         ((,element ,sequence ,@args))
         ,@body))))
