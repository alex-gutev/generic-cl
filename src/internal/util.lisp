;;;; util.lisp
;;;;
;;;; Copyright 2018-2020 Alexander Gutev
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

(in-package :generic-cl.internal)

(defun numbers? (args env)
  "Returns true if each form in ARGS evaluates (within the lexical
   environment ENV) to a type that is a subtype of NUMBER."

  (flet ((number? (thing)
	   ;; For some reason SUBTYPEP on CMUCL does not take a third
	   ;; environment parameter
	   (subtypep (nth-form-type thing env) 'number #-cmucl env)))

    (every #'number? args)))

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

(defmacro with-constant-values ((&rest things) env &body clauses)
  "Check whether one or more forms are constant and retrieve their values.

   THINGS:

     A list of bindings each of the form (VAR INITFORM).

     Each INITFORM is evaluated and the result is checked whether it
     is a constant form in the environment ENV. If so the constant
     value is stored in VAR otherwise the resulting form (to which
     INITFORM evaluates) itself is stored in VAR.

     Alternatively each element of THINGS can be a symbol in which
     case it is a shorthand for (VAR VAR).

   ENV:

     The environment in which to check whether the forms are
     constants.

   CLAUSES:

     A list of clauses each of the form (VARS . FORMS), where
     VARS is a list of variables, listed in THINGS, which should be
     constant in order for the corresponding FORMS to be evaluated.

     If all variables in VARS are constants FORMS are evaluated in an
     implicit PROGN, with the result of the last form returned.

     If not all of VARS are constant, FORMS are not evaluated and the
     next clause is tried. If no clause succeeds NIL is returned."

  (let ((const-vars (pairlis (mapcar #'ensure-car things)
                             (make-gensym-list (length things) "CONST?"))))

    (labels ((make-get-value (thing body)
               (destructuring-bind (var &optional (form var))
                   (ensure-list thing)

                 (let ((const-var (cdr (assoc var const-vars))))
                   (assert const-var)

                   `(multiple-value-bind (,var ,const-var)
                        (constant-form-value ,form ,env)

                      ,body))))

             (const-var (var)
               (let ((cvar (cdr (assoc var const-vars))))
                 (unless cvar
                   (error "Variable ~s in clause not one of ~s."
                          var (mapcar #'ensure-car things)))

                 cvar))

             (make-clause (clause)
               (destructuring-bind (vars &rest body) clause
                 `((and ,@(mapcar #'const-var vars))
                   ,@body))))

      (cl:reduce
       #'make-get-value
       things
       :from-end t
       :initial-value
       `(cond ,@(mapcar #'make-clause clauses))))))
