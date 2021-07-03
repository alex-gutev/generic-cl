;;;; optimization.lisp
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

;;;; Compiler-macros for sequence functions

(in-package :generic-cl.sequence)


;;; Utilities

(defun sequence? (form env)
  "Return true if FORM is of type CL:SEQUENCCE in environment ENV."

  (subtypep (nth-form-type form env) 'cl:sequence env))

(defun sequences? (forms env)
  "Return true if each form in FORMS is of type CL:SEQUENCE in environment ENV."

  (cl:every (rcurry #'sequence? env) forms))

(defmacro optimize-seq-fn ((vars block) (seqs result) &body body)
  "Generate optimized code for an optimized sequence function.

   The macro expands to a form which generates a DO-SEQUENCES forms
   with the elements of the sequence in SEQS, bound to gensym'd
   variables. The DO-SEQUENCE form is followed by the form RESULT with
   the entire generated form wrapped in a BLOCK.

   VARS is the name of a variable which is bound to the list of the
   GENSYM'd names of the variables bound to the elements of the
   sequences.

   BLOCK is a variable bound to the name of the block in which the
   entire generated code is contained.

   SEQS (evaluated) is the list of forms which are placed in the
   sequences of the DO-SEQUENCES forms.

   RESULT (evaluated) is the form which is placed last in the
   generated BLOCK, thus its value is returned if there is no
   non-local exit from the block.

   BODY is evaluated, in an implicit PROGN, the result of which is
   spliced into the body of the generated DO-SEQUENCES form. The
   bindings to VARS and BLOCK are available to the forms in BODY."

  (with-gensyms (do-block)
    `(let ((,vars (make-gensym-list (cl:length ,seqs))))
       (with-gensyms (,block)
         `(block ,,block
            (do-sequences ,',do-block
              ,(mapcar #'list ,vars ,seqs)

              ,,@body)

            ,,result)))))


;;; Sequence Predicates

(define-compiler-macro every (f &rest seqs &environment env)
  (if (sequences? seqs env)
      `(cl:every ,f ,@seqs)

      (optimize-seq-fn (vars block) (seqs t)
        `(unless (funcall ,f ,@vars)
           (return-from ,block nil)))))

(define-compiler-macro some (f &rest seqs &environment env)
  (if (sequences? seqs env)
      `(cl:some ,f ,@seqs)

      (optimize-seq-fn (vars block) (seqs nil)
        (with-gensyms (value)
          `(let ((,value (funcall ,f ,@vars)))
             (when ,value
               (return-from ,block ,value)))))))

(define-compiler-macro notany (f &rest seqs &environment env)
  (if (sequences? seqs env)
      `(cl:notany ,f ,@seqs)

      (optimize-seq-fn (vars block) (seqs t)
        `(when (funcall ,f ,@vars)
           (return-from ,block nil)))))

(define-compiler-macro notevery (f &rest seqs &environment env)
  (if (sequences? seqs env)
      `(cl:notevery ,f ,@seqs)

      (optimize-seq-fn (vars block) (seqs nil)
        `(unless (funcall ,f ,@vars)
           (return-from ,block t)))))


;;; Concatenation

(define-compiler-macro concatenate-to (&whole whole type &rest seqs &environment env)
  (multiple-value-bind (result-type const?)
      (constant-form-value type env)

    (if (and const?
             (subtypep result-type 'sequence env)
             (sequences? seqs env))

        `(cl:concatenate ,type ,@seqs)
        (static-dispatch whole env))))


;;; Mapping

(define-compiler-macro map-to (&whole whole type f &rest seqs &environment env)
  (multiple-value-bind (result-type const?)
      (constant-form-value type env)

    (if (and const?
             (subtypep result-type 'sequence env)
             (sequences? seqs env))

        `(cl:map ,type ,f ,(first seqs) ,@(rest seqs))
        (static-dispatch whole env))))

(define-compiler-macro nmap (&whole whole result f &rest seqs &environment env)
  (let ((result-type (nth-form-type result env)))
    (if (and (subtypep result-type '(or simple-vector simple-string simple-bit-vector list) env)
             (sequences? seqs env))

        `(cl:map-into ,result ,f ,result ,@seqs)
        (static-dispatch whole env))))

(define-compiler-macro map (&whole whole f seq &rest sequences &environment env)
  (let ((result-type (nth-form-type seq env)))
    (if (and (subtypep result-type 'sequence env)
             (sequences? sequences env))

        `(cl:map ',result-type ,f ,seq ,@sequences)
        (static-dispatch whole env))))

(define-compiler-macro foreach (f &rest seqs &environment env)
  (if (sequences? seqs env)
      `(cl:map nil ,f ,(first seqs) ,@(rest seqs))

      (optimize-seq-fn (vars block) (seqs nil)
        `(funcall ,f ,@vars))))
