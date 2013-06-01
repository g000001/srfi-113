;;;; srfi-113.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :srfi-113
  :serial t
  :depends-on (:fiveam
               :named-readtables
               :rnrs-compat
               :srfi-69
               :srfi-9
               :srfi-23
               :srfi-13
               :srfi-39
               :mbe
               )
  :components ((:file "package")
               (:file "count")
               (:file "srfi-4-shim")
               (:file "sets-impl")
               (:file "bags-impl")
               (:file "isets-impl")
               (:file "enums-impl")
               (:file "srfi-113")
               (:file "sets-test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-113))))
  (load-system :srfi-113)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-113.internal :srfi-113))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

