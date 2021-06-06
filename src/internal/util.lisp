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
