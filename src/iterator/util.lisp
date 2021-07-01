;;;; util.lisp
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

(in-package :generic-cl.iterator)

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
         (stable-sort #'type< :key #'type-qualifier)
         (reduce #'make-if <>
                 :from-end t
                 :initial-value
                 `(method-combination-error "No method for type ~a" ,type)))))

;;; Destructuring

(defmacro with-destructure-pattern ((var pattern) (body-var decl-var body) &body forms)
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

   DECL-VAR is the name of a variable which receives the list of
   DECLARE expressions applying to the variable, given by VAR. These
   should be inserted in the correct place, where VAR is bound, in the
   result returned.

   The body forms of the macro FORMS are evaluated in an implicit
   PROGN, with the bindings to the variables given by VAR and BODY-VAR
   visible. The return value of the last form is interpreted as the
   body form making up the WITH-ITER-VALUE expansion returned.

   FORMS should generate code which binds the current sequence element
   to the variable with name stored in VAR.

   This macro automatically takes care of handling declarations, that
   is if the list returned by BODY contains declarations, those
   applying to the variables in the destructuring pattern are inserted
   in the `destructuring-bind` form, DECL-VAR is bound to those which
   apply to the variable VAR, and a LOCALLY form, wrapping the form
   returned by FORMS, is generated in which the remaining declarations
   are inserted."

  `(make-destructure-pattern
    ,pattern ,body
    (lambda (,var ,decl-var ,body-var)
      ,@forms)))

(defmacro split-declarations-forms ((decl forms) body-form &body body)
  "Split a 'body' into declare expressions and the forms to be evaluate.

   DECL is the name of the variable to receive the list of DECLARE
   expressions.

   FORMS is the name of the variable to receive the body forms.

   BODY-FORM is a form (evaluated) which produces the body.

   BODY is the list of forms, evaluated in an implicit PROGN with DECL
   and FORMS bound to the declarations and forms respectively. The
   value of the last form is returned."

  `(multiple-value-bind (,forms ,decl)
       (parse-iter-macro-body ,body-form)
     ,@body))

(defmacro with-variable-declarations ((&rest bindings) forms-var body-form &body body)
  "Split a body into the declarations, applying to specific variables, and the forms.

   BINDINGS:

     List specifying for which variables to extract the declarations.

     Each element is of the form (DECL-VAR VAR), where DECL-VAR is the
     name of the variable which is to receive the list of declarations
     applying to the variable given by VAR (evaluated).

   FORMS-VAR:

     Name of the variable to receive the list of forms

   BODY-FORM:

     The body to split (evaluated).

   BODY:

     The macro body forms evaluated in an implicit PROGN. The value
     returned by the last form is included in the result returned.

   The value returned by the macro is a LOCALLY form containing the
   declarations not applying to any of the variables listed in
   BINDINGS and the body of which is the form returned by the last
   form in BODY."

  (flet ((make-partition (spec body)
           (destructuring-bind (decl-var var) spec
             (destructuring-bind (decl-other . body) body
               (with-gensyms (decl)
                 (cons
                  decl

                  `((multiple-value-bind (,decl-var ,decl-other)
                        (partition-declarations (list ,var) ,decl)
                      ,@body))))))))

    (with-gensyms (decl-other)
      (destructuring-bind (decl . body)
          (reduce
           #'make-partition
           bindings
           :from-end t
           :initial-value
           (cons decl-other
                 `(`(locally ,@,decl-other
                      ,(progn ,@body)))))

        `(split-declarations-forms (,decl ,forms-var) ,body-form
           ,@body)))))

(defun make-destructure-pattern (pattern body fn)
  (multiple-value-bind (forms decl)
      (parse-iter-macro-body body)

    (etypecase pattern
      (list
       (multiple-value-bind (decl-xs decl-other)
           (-> (destructure-vars pattern)
               (partition-declarations decl))

         (with-gensyms (var)
           `(locally ,@decl-other
              ,(->> `((destructuring-bind ,pattern ,var
                        ,@decl-xs
                        ,@forms))
                    (funcall fn var nil))))))

      (symbol
       (multiple-value-bind (decl-var decl-other)
           (partition-declarations (list pattern) decl)

         `(locally ,@decl-other
            ,(funcall fn pattern decl-var forms)))))))

(defun parse-iter-macro-body (body)
  "Split the BODY of a WITH-ITER-VALUE/PLACE macro into declaration
   expressions and forms.

   If BODY is a list of a single LOCALLY form, the body of the LOCALLY
   form is parsed instead."

  (match body
    ((or (list (list* 'cl:locally body)) body)
     (parse-body body :documentation nil))))

(defun destructure-vars (lambda-list)
  "Return the list of variables introduced by a destructuring-lambda-list.

   LAMBDA-LIST is a destructuring-lambda-list as found in
   DESTRUCTURING-BIND. It should not have &ENVIRONMENT parameters.

   Returns the list of all variables introduced by the lambda-list,
   not in any particular order.

   NOTE: If the lambda-list is malformed, or an unknown lambda-list
   keyword is encountered, this function simply returns the variable
   names it has encountered so far, and silently ignores the remaining
   malformed part."

  (check-type lambda-list list)

  (labels ((process-required (list vars)
             (nlet process ((list list)
                            (vars vars))

               (match list
                 (nil vars)

                 ((type symbol)
                  (cons list vars))

                 ((cons (and (type list) arg) list)
                  (->> (process-required arg vars)
                       (process list)))

                 ((cons (guard keyword (member keyword lambda-list-keywords))
                        list)

                  (process-next keyword list vars))

                 ((cons (and (type symbol) var) list)
                  (process list (cons var vars)))

                 (_ vars))))

           (process-next (keyword list vars)
             (case keyword
               (&optional (process-optional list vars))
               (&whole (process-whole list vars))
               ((&rest &body) (process-rest list vars))
               (&key (process-key list vars))
               (&allow-other-keys (process-next-section list vars))
               (&aux (process-aux list vars))
               (otherwise
                ;; Unknown lambda-list keyword.
                vars)))

           (process-optional (list vars)
             (nlet process ((list list) (vars vars))
               (match list
                 ((list* (guard keyword (member keyword lambda-list-keywords)) list)
                  (process-next keyword list vars))

                 ((list*
                   (or (list (and (type symbol) var)
                             _
                             (and (type symbol) initp))

                       (list (and (type symbol) var) _)
                       (list (and (type symbol) var))
                       (and (type symbol) var))

                   list)

                  (->> (append (ensure-list initp) vars)
                       (cons var)
                       (process list)))

                 (_ vars))))

           (process-rest (list vars)
             (match list
               ((list* (and (type list) arg) list)
                (->> (process-required arg vars)
                     (process-next-section list)))

               ((list* (and (type symbol) var) list)
                (process-next-section list (cons var vars)))

               (_ vars)))

           (process-whole (list vars)
             (match list
               ((list* (and (type symbol) (not nil) var) list)
                (process-required list (cons var vars)))

               (_ vars)))

           (process-next-section (list vars)
             (match list
               ((list* (guard keyword lambda-list-keywords) list)
                (process-next keyword list vars))

               (_ vars)))

           (process-key (list vars)
             (nlet process ((list list) (vars vars))
               (match list
                 ((list* (guard keyword (member keyword lambda-list-keywords)) list)
                  (process-next keyword list vars))

                 ((list*
                   (or (list (or (and (type symbol) var)
                                 (list _ (and (type symbol) var)))
                             _
                             (and (type symbol) initp))

                       (list (or (and (type symbol) var)
                                 (list _ (and (type symbol) var)))
                             _)
                       (list (or (and (type symbol) var)
                                 (list _ (and (type symbol) var))))
                       (and (type symbol) var))

                   list)

                  (->> (append (ensure-list initp) vars)
                       (cons var)
                       (process list)))

                 (_ vars))))

           (process-aux (list vars)
             (nlet process ((list list) (vars vars))
               (match list
                 ((list*
                   (or (list (and (type symbol) var) _)
                       (list (and (type symbol) var))
                       (and (type symbol) var))

                   list)

                  (process list (cons var vars)))

                 (_ vars)))))

    (process-required lambda-list nil)))

;;; Iterator Macros

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

;;; Parsing declarations

;; From Serapeum / macro-tools.lisp

;; Copyright (c) 2014 Paul M. Rodriguez

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defun partition-declarations (xs declarations &optional env)
  "Split DECLARATIONS into those that do and do not apply to XS.
Return two values, one with each set.

Both sets of declarations are returned in a form that can be spliced
directly into Lisp code:

     (locally ,@(partition-declarations vars decls) ...)"
  (let ((env2 (parse-declarations declarations env)))
    (flet ((build (env)
             (build-declarations 'declare env)))
      (if (null xs)
          (values nil (build env2))
          (values
           (build (filter-declaration-env env2 :affecting xs))
           (build (filter-declaration-env env2 :not-affecting xs)))))))
