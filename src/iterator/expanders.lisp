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


;;; Lists

(defmethod make-doseq list ((type t) form args tag body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-gensyms (list index v-start v-end v-from-end with-value with-place place)
      (values
       `((,v-start ,start :constant t)
         (,v-end ,end :constant t)
         (,v-from-end ,from-end :constant t)

         (,list
          (if ,v-from-end
              (sublist ,form ,v-start ,v-end ,v-from-end)
              (nthcdr ,v-start ,form))))

       (let ((value-macro
              (iter-macro (tag list place)
                  (pattern &body body)

                (with-destructure-pattern (var pattern)
                    (body decl body)

                  `(progn
                     (unless ,list
                       (go ,tag))

                     (let ((,var (,place ,list)))
                       ,@decl

                       (setf ,list (cdr ,list))
                       ,@body)))))

             (place-macro
              (iter-macro (tag list place)
                  (name more? &body body)

                (let ((body
                       `(prog1 (progn ,@body)
                          (setf ,list (cdr ,list)))))

                  `(symbol-macrolet ((,name (,place ,list)))
                     ,(if more?
                          `(let ((,more? ,list))
                             ,body)

                          `(progn
                             (unless ,list
                               (go ,tag))

                             ,body)))))))

         (with-gensyms (list-value list-place)
           `((cond
               (,v-from-end
                (macrolet ((,with-value . ,value-macro)
                           (,with-place . ,place-macro)
                           (,place (thing)
                             `(caar ,thing)))

                  ,@body))

               (,v-end
                (let ((,index ,v-start))
                  (macrolet ((,list-value . ,value-macro)
                             (,list-place . ,place-macro)
                             (,place (thing)
                               `(car ,thing))

                             (,with-value .
                               ,(iter-macro (tag index v-end list-value)
                                    (pattern &body body)

                                  (split-declarations-forms (decl forms) body
                                    `(,list-value
                                      ,pattern

                                      ,@decl
                                      (unless (< ,index ,v-end)
                                        (go ,tag))

                                      (incf ,index)
                                      ,@forms))))

                             (,with-place .
                               ,(iter-macro (tag index v-end list-place)
                                    (name more? &body body)

                                  `(,list-place
                                    ,name
                                    ,more?

                                    ,@(if more?
                                          `((let ((,more? (and ,more? (< ,index ,v-end))))
                                              (incf ,index)
                                              ,@body))

                                          `((unless (< ,index ,v-end)
                                              (go ,tag))

                                            (incf ,index)
                                            ,@body))))))
                    ,@body)))

               (t
                (macrolet ((,with-value . ,value-macro)
                           (,with-place . ,place-macro)
                           (,place (thing)
                             `(car ,thing)))

                  ,@body))))))


       (iter-macro (with-value)
           (pattern &body body)
         `(,with-value ,pattern ,@body))

       (iter-macro (with-place)
           (name more? &body body)

         `(,with-place ,name ,more? ,@body))))))

(defun sublist (list start end from-end)
  "Return the list of CONS cells making up a subsequence of a list.

   LIST is the list.

   START is the index of the first element of the subsequence.

   END is the index 1 past the last element of the subsequence. If NIL
   the subsequence extends till the end of the list.

   If FROM-END is true the cells are collected starting from the last
   element of the subsequence, otherwise they are collected starting
   from the first element of the subsequence.

   The return value is a list of CONS cells of the original list,
   corresponding to the cells containing the elements of the
   subsequence. This allows modifying the original list by modifying
   the cons cells."

  (if from-end
      (let (cells)
        (loop
           for cell on (nthcdr start list)
           for i from start
           while (or (null end) (< i end))
           do
             (push cell cells))

        cells)

      (loop
         for cell on (nthcdr start list)
         for i from start
         while (or (null end) (< i end))
         collect cell)))


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
             (body decl body)

           `(progn
              (unless (if ,v-from-end
                          (>= ,index ,v-start)
                          (< ,index ,end-index))
                (go ,tag))

              (let ((,var (aref ,vec ,index)))
                ,@decl

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
           (body decl body)

         `(progn
            (when (endp ,it)
              (go ,tag))

            (let ((,var (at ,it)))
              ,@decl
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
