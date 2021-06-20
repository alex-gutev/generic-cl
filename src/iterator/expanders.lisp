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

(defmethod make-doseq list ((type t) (var symbol) form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-constant-values (from-end start end) env
      ((from-end start end)
       (cond
         (from-end
          (make-traverse-list
           var
           `(cl:nreverse (cl:subseq ,form ,start ,end))
           body))

         (end
          (make-traverse-bounded-list var form start end body))

         ((> start 0)
          (make-traverse-list var `(nthcdr ,start ,form) body))

         (t
          (make-traverse-list var form body))))

      ((start end)
       (cond
         (end
          (make-traverse-list
           var
           `(if ,from-end
                (cl:nreverse (cl:subseq ,form ,start ,end))
                (cl:subseq ,form ,start ,end))
           body))

         ((> start 0)
          (make-traverse-list
           var
           `(if ,from-end
                (cl:reverse (nthcdr ,start ,form))
                (nthcdr ,start ,form))
           body))

         (t
          (make-traverse-list
           var
           `(if ,from-end
                (cl:reverse ,form)
                ,form)
           body))))

      (nil
       (make-traverse-list
        var
        `(if ,from-end
             (cl:nreverse (cl:subseq ,form ,start ,end))
             (cl:subseq ,form ,start ,end))
        body)))))

(defmethod make-doseq list ((type t) (pattern list) form args body env)
  (with-gensyms (item)
    (-<> `(destructuring-bind ,pattern ,item ,body)
         (make-doseq type item form args <> env))))

(defun make-traverse-bounded-list (element form start end body)
  "Generate a TRAVERSE expansion for a bounded list traversal, when END is non-NIL."

  (multiple-value-bind (bindings body parent)
      (make-traverse-list
       element
       `(nthcdr ,start ,form)
       body)

    (with-gensyms (index)
      (values
       `((,index ,start) ,@bindings)

       `(when (< ,index ,end)
          (incf ,index)
          ,body)

       parent))))

(defun make-traverse-list (element form body)
  "Generate a TRAVERSE expansion for a unbounded list traversal."

  (with-gensyms (list)
    (values
     `((,list ,form))

     `(when ,list
        (let ((,element (car ,list)))
          (setf ,list (cdr ,list))
          ,body))

     nil)))


;;; Vectors

(defmethod make-doseq vector ((type t) (var symbol) form args body env)
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

       `(when (if ,v-from-end
                  (cl:>= ,index ,v-start)
                  (cl:< ,index ,end-index))

          (let ((,var (aref ,vec ,index)))
            (if ,v-from-end
                (cl:decf ,index)
                (cl:incf ,index))
            ,body))

       nil))))

(defmethod make-doseq vector ((type t) (pattern list) form args body env)
  (with-gensyms (item)
    (-<> `(destructuring-bind ,pattern ,item ,body)
         (make-doseq type item form args <> env))))


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
