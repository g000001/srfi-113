(cl:in-package :srfi-113.internal)
(in-readtable :rnrs)

;;;; Implementation of count-up and count-down macros for SRFI xxx

;;; These macros are used within iset.scm and enumset.scm to count
;;; up and down.  The variable i is bound to successive values from
;;; start to (- end 1) for count-up, or from (- end 1) to start
;;; for count-down, and the statements of the body are evaluated.
;;; The keyword return may be invoked to prematurely exit from the loop;
;;; if not, the value of result is returned.


(cl:defmacro call/cc (f)
  (cl:let ((name (cl:gensym "block-"))
           (arg (cl:gensym "arg-")))
    `(cl:block ,name
       (_,f (cl:lambda (,arg)
                        (cl:return-from ,name ,arg))))))


(define-syntax count-up (syntax-rules ()
  ((count-up (i start end) . body)
   (with ((from (cl:gensym))
          (to (cl:gensym)))
     (let ((from start) (to end))
       (do ((i from (+ i 1)))
           ((>= i to) :undef)
         . body))))
  ((count-up (return result) (i start end) . body)
   (with ((from (cl:gensym))
          (to (cl:gensym)))
     (call/cc
      (lambda (return)
        (cl:declare (cl:ignorable return))
        (let ((from start) (to end))
          (do ((i from (+ i 1)))
              ((>= i to) result)
            . body))))))))

(define-syntax count-down (syntax-rules ()
  ((count-up (i start end) . body)
   (with ((from (cl:gensym))
          (to (cl:gensym)))
     (let ((from start) (to end))
       (do ((i (- to 1) (- i 1)))
           ((< i from) :undef)
         . body))))
  ((count-up (return result) (i start end) . body)
   (with ((from (cl:gensym))
          (to (cl:gensym)))
     (call/cc
      (lambda (return)
        (cl:declare (cl:ignorable return))
        (let ((from start) (to end))
          (do ((i (- to 1) (- i 1)))
              ((< i from) result)
            . body))))))))

;;; *EOF*


