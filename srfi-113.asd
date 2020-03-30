;;;; srfi-113.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :srfi-113
  :version "20200331"
  :description "SRFI 113 for CL: Sets, bags, integer sets, enumeration sets"
  :long-description "SRFI 113 for CL: Sets, bags, integer sets, enumeration sets
https://srfi.schemers.org/srfi-113"
  :author "John Cowan"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (fiveam
               named-readtables
               rnrs-compat
               srfi-4
               srfi-69
               srfi-9
               srfi-23
               srfi-13
               srfi-39
               mbe)
  :components ((:file "package")
               (:file "count")
               (:file "srfi-4-shim")
               (:file "sets-impl")
               (:file "bags-impl")
               (:file "isets-impl")
               (:file "enums-impl")
               (:file "sets-test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-113))))
  (let ((name "https://github.com/g000001/srfi-113")
        (nickname :srfi-113))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-113))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-113#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-113)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
