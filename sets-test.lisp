(cl:in-package :srfi-113.internal)


(def-suite srfi-113)


(in-suite srfi-113)


(define-syntax t=
  (syntax-rules ()
    ((t= expr)
     (is-true expr))))


(define-syntax ==
  (syntax-rules ()
    ((== arg cl:***)
     (is (current-test-comparator arg cl:***)))))


(define-syntax let-current-test-comparator 
  (syntax-rules ()
    ((let-current-test-comparator fn body cl:***)
     (cl:flet ((current-test-comparator (cl:&rest args)
                 (apply fn args)))
       body cl:***))))


(define-syntax >_<
  (syntax-rules ()
    ((>_< expr)
     (signals (cl:error) expr))))


(define current-test-comparator #'equal?)


(define (big x) (> x 5))


(def-suite* sets :in srfi-113)


(test sets/simple
  (let* ((nums (make-set #'=))
         ;; nums is now {}
         (syms (set #'eq? 'a 'b 'c 'd))
         ;; syms is now {a, b, c, d}
         (nums2 (set-copy nums))
         ;; nums2 is now {}
         (syms2 (set-copy syms))
         ;; syms2 is now {a, b, c, d}
         (total 0))
    (t= (set? nums))
    (t= (set? syms))
    (t= (set? nums2))
    (t= (set? syms2))
    (t= (not (set? 'a)))
    (set-add! nums 2)
    (set-add! nums 3)
    (set-add! nums 4)
    (set-add! nums 4)
    ;; nums is now {2, 3, 4}
    (== 3 (set-size nums))
    (== 4 (set-size syms))
    (== 0 (set-size nums2))
    (== 4 (set-size syms2))
    (t= (set-delete! nums 2))  
    ;; nums is now {3, 4}
    (t= (not (set-delete! nums 1)))
    (== 2 (set-size nums))
    (set! nums2 (set-map #'= (lambda (x) (* 10 x)) nums))
    ;; nums2 is now {30, 40}
    (t= (set-member? nums2 30))
    (t= (not (set-member? nums2 3)))
    (set-for-each (lambda (x) (set! total (+ total x))) nums2)
    (== 70 total)
    (== 10 (set-fold #'+ 3 nums))
    (set! nums (set #'eqv? 10 20 30 40 50))
    ;; nums is now {10, 20, 30, 40, 50}
    (t=
     (set=? nums (set-unfold #'eqv?
                             (lambda (i) (> i 0))
                             (lambda (i) (* i 10))
                             (lambda (i) (- i 1))
                             5)))
    (== '(a) (set->list (set #'eq? 'a)))
    (set! syms2 (list->set #'eq? '(e f)))
    ;; syms2 is now {e, f}
    (== 2 (set-size syms2))
    (t= (set-member? syms2 'e))
    (t= (set-member? syms2 'f))))


(test sets/subsets
  (let* ((set2 (set #'= 1 2))
         (other-set2 (set #'= 1 2))
         (set3 (set #'= 1 2 3))
         (set4 (set #'= 1 2 3 4))
         #|(setx (set #'= 10 20 30 40))|#)
    (t= (set=? set2 other-set2))
    (t= (not (set=? set2 set3)))
    (t= (not (set=? set2 set3 other-set2)))
    (t= (set<? set2 set3 set4))
    (t= (not (set<? set2 other-set2)))
    (t= (set<=? set2 other-set2 set3))
    (t= (not (set<=? set2 set3 other-set2)))
    (t= (set>? set4 set3 set2))
    (t= (not (set>? set2 other-set2)))
    (t= (set>=? set3 other-set2 set2))
    (t= (not (set>=? other-set2 set3 set2)))))


(test sets/ops
  ;; Potentially mutable
  (let ((abcd (set #'eq? 'a 'b 'c 'd))
        (efgh (set #'eq? 'e 'f 'g 'h))
        (abgh (set #'eq? 'a 'b 'g 'h))
        ;; Never get a chance to be mutated
        (other-abcd (set #'eq? 'a 'b 'c 'd))
        #|(other-efgh (set #'eq? 'e 'f 'g 'h))|#
        #|(other-abgh (set #'eq? 'a 'b 'g 'h))|#
        (all (set #'eq? 'a 'b 'c 'd 'e 'f 'g 'h))
        (none (set #'eq?))
        (ab (set #'eq? 'a 'b))
        (cd (set #'eq? 'c 'd))
        #|(ef (set #'eq? 'e 'f))|#
        (gh (set #'eq? 'g 'h))
        (cdgh (set #'eq? 'c 'd 'g 'h))
        (abcdgh (set #'eq? 'a 'b 'c 'd 'g 'h))
        (abefgh (set #'eq? 'a 'b 'e 'f 'g 'h)))
    (let-current-test-comparator #'set=?
      (== all (set-union abcd efgh))
      (== abcdgh (set-union abcd abgh))
      (== abefgh (set-union efgh abgh))
      (== none (set-intersection abcd efgh))
      (== ab (set-intersection abcd abgh))
      (== ab (set-intersection abgh abcd))
      (== cd (set-difference abcd ab))
      (== abcd (set-difference abcd gh))
      (== none (set-difference abcd abcd))
      (== cdgh (set-xor abcd abgh))
      (== all (set-xor abcd efgh))
      (== none (set-xor abcd other-abcd))
      ;;--- FIXME
      ;; (== "abcd smashed?" other-abcd abcd)
      ;; (== "efgh smashed?" other-efgh efgh)
      ;; (== "abgh smashed?" other-abgh abgh)
      )))


(test sets/mismatch
  (let* ((nums (set #'= 1 2 3))
         (syms (set #'eq? 'a 'b 'c)))
    (>_< (set=? nums syms))
    (>_< (set<? nums syms))
    (>_< (set<=? nums syms))
    (>_< (set>? nums syms))
    (>_< (set>=? nums syms))
    (>_< (set-union nums syms))
    (>_< (set-intersection nums syms))
    (>_< (set-difference nums syms))
    (>_< (set-xor nums syms))
    (>_< (set-union! nums syms))
    (>_< (set-intersection! nums syms))
    (>_< (set-difference! nums syms))
    (>_< (set-xor! nums syms)))) 


(test sets/whole
  (let* ((whole (set #'eqv? 1 2 3 4 5 6 7 8 9 10))
         (bottom (set #'eqv? 1 2 3 4 5))
         (top (set #'eqv? 6 7 8 9 10)))
    (cl:multiple-value-bind (topx bottomx)
         (set-partition #'big whole)
      (s= top (set-filter #'big whole))
      (s= bottom (set-remove #'big whole))
      (s= top topx)
      (s= bottom bottomx)
      (== 5 (set-count #'big whole)))))


(test sets/value
  (let ((bucket (set #'string-ci=? "abc" "def")))
    (t= (set-member? bucket "abc"))
    (t= (set-member? bucket "ABC"))
    (== "def" (set-value bucket "DEF"))))


(def-suite* bags :in srfi-113)


(test bags/simple
  (let* ((nums (make-bag #'=))
         ;; nums is now {}
         (syms (bag #'eq? 'a 'b 'c 'd))
         ;; syms is now {a, b, c, d}
         (nums2 (bag-copy nums))
         ;; nums2 is now {}
         (syms2 (bag-copy syms))
         ;; syms2 is now {a, b, c, d}
         (total 0))
    (t= (bag? nums))
    (t= (bag? syms))
    (t= (bag? nums2))
    (t= (bag? syms2))
    (t= (not (bag? 'a)))
    (bag-add! nums 2)
    (bag-add! nums 3)
    (bag-add! nums 4)
    ;; nums is now {2, 3, 4}
    (== 3 (bag-size nums))
    (== 4 (bag-size syms))
    (== 0 (bag-size nums2))
    (== 4 (bag-size syms2))
    (t= (bag-delete! nums 2))
    ;; nums is now {3, 4}
    (t= (not (bag-delete! nums 1)))
    (== 2 (bag-size nums))
    (set! nums2 (bag-map #'= (lambda (x) (* 10 x)) nums))
    ;; nums2 is now {30, 40}
    (t= (bag-member? nums2 30))
    (t= (not (bag-member? nums2 3)))
    (bag-for-each (lambda (x) (set! total (+ total x))) nums2)
    (== 70 total)
    (== 10 (bag-fold #'+ 3 nums))
    (set! nums (bag #'eqv? 10 20 30 40 50))
    ;; nums is now {10, 20, 30, 40, 50}
    (t=
     (bag=? nums (bag-unfold #'eqv?
                             (lambda (i) (> i 0))
                             (lambda (i) (* i 10))
                             (lambda (i) (- i 1))
                             5)))
    (== '(a) (bag->list (bag #'eq? 'a)))
    (set! syms2 (list->bag #'eq? '(e f)))
    ;; syms2 is now {e, f}
    (== 2 (bag-size syms2))
    (t= (bag-member? syms2 'e))
    (t= (bag-member? syms2 'f))
    ))


(test bags/subbags
  (let* ((bag2 (bag #'= 1 2))
         (other-bag2 (bag #'= 1 2))
         (bag3 (bag #'= 1 2 3))
         (bag4 (bag #'= 1 2 3 4))
         #|(bagx (bag #'= 10 20 30 40))|#)
    (t= (bag=? bag2 other-bag2))
    (t= (not (bag=? bag2 bag3)))
    (t= (not (bag=? bag2 bag3 other-bag2)))
    (t= (bag<? bag2 bag3 bag4))
    (t= (not (bag<? bag2 other-bag2)))
    (t= (bag<=? bag2 other-bag2 bag3))
    (t= (not (bag<=? bag2 bag3 other-bag2)))
    (t= (bag>? bag4 bag3 bag2))
    (t= (not (bag>? bag2 other-bag2)))
    (t= (bag>=? bag3 other-bag2 bag2))
    (t= (not (bag>=? other-bag2 bag3 bag2)))))


(test bags/ops
  ;; Potentially mutable
  (let* ((abcd (bag #'eq? 'a 'b 'c 'd))
         (efgh (bag #'eq? 'e 'f 'g 'h))
         (abgh (bag #'eq? 'a 'b 'g 'h))
         ;; Never get a chance to be mutated
         #|(other-abcd (bag #'eq? 'a 'b 'c 'd))|#
         #|(other-efgh (bag #'eq? 'e 'f 'g 'h))|#
         #|(other-abgh (bag #'eq? 'a 'b 'g 'h))|#
         (all (bag #'eq? 'a 'b 'c 'd 'e 'f 'g 'h))
         (none (bag #'eq?))
         (ab (bag #'eq? 'a 'b))
         (cd (bag #'eq? 'c 'd))
         #|(ef (bag #'eq? 'e 'f))|#
         #|(gh (bag #'eq? 'g 'h))|#
         #|(cdgh (bag #'eq? 'c 'd 'g 'h))|#
         (abcdgh (bag #'eq? 'a 'b 'c 'd 'g 'h))
         (abefgh (bag #'eq? 'a 'b 'e 'f 'g 'h)))
    (let-current-test-comparator #'bag=? 
      (== all (bag-union abcd efgh))
      (== abcdgh (bag-union abcd abgh))
      (== abefgh (bag-union efgh abgh))
      (== none (bag-intersection abcd efgh))
      (== ab (bag-intersection abcd abgh))
      (== ab (bag-intersection abgh abcd))
      (== cd (bag-difference abcd ab))
      #|(== abcd (bag-difference abcd gh))|#
      (== none (bag-difference abcd abcd))
      #|(== "abcd smashed?" other-abcd abcd)|#
      #|(== "efgh smashed?" other-efgh efgh)|#
      #|(== "abgh smashed?" other-abgh abgh)|#)))


(test bags/mismatch
  (let* ((nums (bag #'= 1 2 3))
         (syms (bag #'eq? 'a 'b 'c)))
    (>_< (bag=? nums syms))
    (>_< (bag<? nums syms))
    (>_< (bag<=? nums syms))
    (>_< (bag>? nums syms))
    (>_< (bag>=? nums syms))
    (>_< (bag-union nums syms))
    (>_< (bag-intersection nums syms))
    (>_< (bag-difference nums syms))
    ;; (>_< (bag-xor nums syms)) --- FIXME
    (>_< (bag-union! nums syms))
    (>_< (bag-intersection! nums syms))
    (>_< (bag-difference! nums syms))))


(test bags/whole
  (let* ((whole (bag #'eqv? 1 2 3 4 5 6 7 8 9 10))
         (bottom (bag #'eqv? 1 2 3 4 5))
         (top (bag #'eqv? 6 7 8 9 10)))
    (cl:multiple-value-bind (topx bottomx)
         (bag-partition #'big whole)
      (let-current-test-comparator #'bag=?
        (== top (bag-filter #'big whole))
        (== bottom (bag-remove #'big whole))
        (== top topx)
        (== bottom bottomx)))
    (== 5 (bag-count #'big whole))))


(test bags/semantics
  (let* ((mybag (bag #'= 1 2)))
    ;; mybag is {1, 2}
    (== 2 (bag-size mybag))
    (bag-add! mybag 1)
    ;; mybag is {1, 1, 2}
    (== 3 (bag-size mybag))
    (t= (bag-delete! mybag 2))
    ;; mybag is {1, 1}
    (t= (not (bag-delete! mybag 2)))
    (== 2 (bag-size mybag))
    (bag-increment! mybag 1 3)
    ;; mybag is {1, 1, 1, 1, 1}
    (== 5 (bag-size mybag))
    (t= (bag-decrement! mybag 1 2))
    ;; mybag is {1, 1, 1}
    (== 3 (bag-size mybag))
    (t= (not (bag-decrement! mybag 1 5)))
    ;; mybag is {}
    (== 0 (bag-size mybag))))


(def-suite* isets :in srfi-113)


(test isets/simple
  (let* ((nums (make-integer-set 10))
         ;; nums is now {}
         (bignums (integer-set 100 1 2 3 4))
         ;; bignums is now {1, 2, 3, 4}
         (nums2 (integer-set-copy nums))
         ;; nums2 is now {}
         (bignums2 (integer-set-copy bignums))
         ;; bignums2 is now {1, 2, 3, 4}
         (total 0))
    (t= (integer-set? nums))
    (t= (integer-set? bignums))
    (t= (integer-set? nums2))
    (t= (integer-set? bignums2))
    (t= (not (integer-set? 1)))
    (integer-set-add! nums 2)
    (integer-set-add! nums 3)
    (integer-set-add! nums 4)
    (integer-set-add! nums 4)
    ;; nums is now {2, 3, 4}
    (== 3 (integer-set-size nums))
    (== 4 (integer-set-size bignums))
    (== 0 (integer-set-size nums2))
    (== 4 (integer-set-size bignums2))
    (t= (integer-set-delete! nums 2))
    ;; nums is now {3, 4}
    (t= (not (integer-set-delete! nums 1)))
    (== 2 (integer-set-size nums))
    (set! nums2 (integer-set-map 100 (lambda (x) (* 10 x)) nums))
    ;; nums2 is now {30, 40}
    (t= (integer-set-member? nums2 30))
    (t= (not (integer-set-member? nums2 3)))
    (integer-set-for-each (lambda (x) (set! total (+ total x))) nums2)
    (== 70 total)
    (== 10 (integer-set-fold #'+ 3 nums))
    (set! nums (integer-set 100 10 20 30 40 50))
    ;; nums is now {10, 20, 30, 40, 50}
    (t= (integer-set=? nums (integer-set-unfold 100
                                                (lambda (i) (> i 0))
                                                (lambda (i) (* i 10))
                                                (lambda (i) (- i 1))
                                                5)))
    (== '(1) (integer-set->list (integer-set 100 1)))
    (set! bignums2 (list->integer-set 100 '(5 6)))
    ;; bignums2 is now {e, f}
    (== 2 (integer-set-size bignums2))
    (t= (integer-set-member? bignums2 5))
    (t= (integer-set-member? bignums2 6))))


(test isets/subisets
  (let* ((integer-set2 (integer-set 10 1 2))
         (other-set2 (integer-set 10 1 2))
         (integer-set3 (integer-set 10 1 2 3))
         (integer-set4 (integer-set 10 1 2 3 4))
         #|(integer-setx (integer-set 100 10 20 30 40))|#)
    (t= (integer-set=? integer-set2 other-set2))
    (t= (not (integer-set=? integer-set2 integer-set3)))
    (t= (not (integer-set=? integer-set2 integer-set3 other-set2)))
    (t= (integer-set<? integer-set2 integer-set3 integer-set4))
    (t= (not (integer-set<? integer-set2 other-set2)))
    (t= (integer-set<=? integer-set2 other-set2 integer-set3))
    (t= (not (integer-set<=? integer-set2 integer-set3 other-set2)))
    (t= (integer-set>? integer-set4 integer-set3 integer-set2))
    (t= (not (integer-set>? integer-set2 other-set2)))
    (t= (integer-set>=? integer-set3 other-set2 integer-set2))
    (t= (not (integer-set>=? other-set2 integer-set3 integer-set2)))))

(integer-set=? (integer-set 10 1 2 3 4 5 6 7 8)
               (integer-set-union 
                (integer-set 10 1 2 3 4)
                (integer-set 10 5 6 7 8)))


(test isets/ops
  (let* (;; Potentially mutable
        (abcd (integer-set 10 1 2 3 4))
        (efgh (integer-set 10 5 6 7 8))
        (abgh (integer-set 10 1 2 7 8))
        ;; Never get a chance to be mutated
        (other-abcd (integer-set 10 1 2 3 4))
        #|(other-efgh (integer-set 10 5 6 7 8))|#
        #|(other-abgh (integer-set 10 1 2 7 8))|#
        (all (integer-set 10 1 2 3 4 5 6 7 8))
        (none (integer-set 10))
        (ab (integer-set 10 1 2))
        (cd (integer-set 10 3 4))
        #|(ef (integer-set 10 5 6))|#
        (gh (integer-set 10 7 8))
        (cdgh (integer-set 10 3 4 7 8))
        (abcdgh (integer-set 10 1 2 3 4 7 8))
        (abefgh (integer-set 10 1 2 5 6 7 8)))
  (let-current-test-comparator #'integer-set=?
    (== all (integer-set-union abcd efgh))
    (== abcdgh (integer-set-union abcd abgh))
    (== abefgh (integer-set-union efgh abgh))
    (== none (integer-set-intersection abcd efgh))
    (== ab (integer-set-intersection abcd abgh))
    (== ab (integer-set-intersection abgh abcd))
    (== cd (integer-set-difference abcd ab))
    (== abcd (integer-set-difference abcd gh))
    (== none (integer-set-difference abcd abcd))
    (== cdgh (integer-set-xor abcd abgh))
    (== all (integer-set-xor abcd efgh))
    (== none (integer-set-xor abcd other-abcd))
    #|(== "abcd smashed?" other-abcd abcd)|#
    #|(== "efgh smashed?" other-efgh efgh)|#
    #|(== "abgh smashed?" other-abgh abgh)|#)))


(test isets/mismatch
  (let* ((nums (integer-set 10 1 2 3))
         (bignums (integer-set 100 1 2 3)))
    (>_< (integer-set=? nums bignums))
    (>_< (integer-set<? nums bignums))
    (>_< (integer-set<=? nums bignums))
    (>_< (integer-set>? nums bignums))
    (>_< (integer-set>=? nums bignums))
    (>_< (integer-set-union nums bignums))
    (>_< (integer-set-intersection nums bignums))
    (>_< (integer-set-difference nums bignums))
    (>_< (integer-set-xor nums bignums))
    (>_< (integer-set-union! nums bignums))
    (>_< (integer-set-intersection! nums bignums))
    (>_< (integer-set-difference! nums bignums))
    (>_< (integer-set-xor! nums bignums))))


(test isets/whole
  (let* ((whole (integer-set 20 1 2 3 4 5 6 7 8 9 10))
         (bottom (integer-set 20 1 2 3 4 5))
         (top (integer-set 20 6 7 8 9 10)))
    (cl:multiple-value-bind (topx bottomx)
         (integer-set-partition #'big whole)
      (let-current-test-comparator #'integer-set=?
        (== top (integer-set-filter #'big whole))
        (== bottom (integer-set-remove #'big whole))
        (== top topx)
        (== bottom bottomx))
      (== 5 (integer-set-count #'big whole)))))


(test isets/other
  (let* ((all (make-universal-integer-set 10))
         (bottom (integer-set 10 0 1 2 3 4))
         (top (integer-set 10 5 6 7 8 9))
         (top2 (integer-set 10 5 6 7 8 9))
         (empty (make-integer-set 10)))
    (== 10 (integer-set-size all))
    (t= (not (integer-set-member? top 10)))
    (t= (not (integer-set-member? top -10)))
    (let-current-test-comparator #'integer-set=?
      (== top (integer-set-complement bottom))
      (== bottom (integer-set-complement top))
      (set! top2 (integer-set-complement! top2))
      (== bottom top2))
    (>_< (integer-set-add! top2 'x))
    (>_< (integer-set-add! top 1.5))
    (>_< (integer-set-add! top2 10))
    (>_< (integer-set-add! top2 -1))
    (t= (not (integer-set-member? top 10)))
    (== 0 (integer-set-min bottom))
    (== 0 (integer-set-delete-min! bottom))
    ;; bottom is now {1, 2, 3, 4}
    (== 1 (integer-set-min bottom))
    (== 4 (integer-set-max bottom))
    (== 4 (integer-set-delete-max! bottom))
    ;; bottom is now {1, 2, 3}
    (== 3 (integer-set-max bottom))
    (== cl:NIL (integer-set-min empty))
    (== cl:NIL (integer-set-delete-min! empty))
    (== cl:NIL (integer-set-max empty))
    (== cl:NIL (integer-set-delete-max! empty))))

#|(test-group "enums"
            (define capsym-type (make-enum-type '(A B C D E F G H)))
            (define sym-type (make-enum-type '(a b c d e f g h)))
            (define (symbol-downcase s)
              (string->symbol (string-downcase (symbol->string s))))
            (define (symbol-append s1 s2)
              (string->symbol (string-append (symbol->string s1) (symbol->string s2)))))|#

#|(test-group "enums/simple"
  (define capsyms (make-enum-set capsym-type))
  ;; capsyms is now {}
  (define syms (enum-set sym-type 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define capsyms2 (enum-set-copy capsyms))
  ;; capsyms2 is now {}
  (define syms2 (enum-set-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define total 'z)
  (t= (enum-set? capsyms))
  (t= (enum-set? syms))
  (t= (enum-set? capsyms2))
  (t= (enum-set? syms2))
  (t= (not (enum-set? 'a)))
  (enum-set-add! capsyms 'B)
  (enum-set-add! capsyms 'C)
  (enum-set-add! capsyms 'D)
  (enum-set-add! capsyms 'D)
  ;; capsyms is now {B, C, D}
  (== 3 (enum-set-size capsyms))
  (== 4 (enum-set-size syms))
  (== 0 (enum-set-size capsyms2))
  (== 4 (enum-set-size syms2))
  (t= (enum-set-delete! capsyms 'B))
  ;; capsyms is now {C, D}
  (t= (not (enum-set-delete! capsyms 'A)))
  (== 2 (enum-set-size capsyms))
  (set! capsyms2
    (enum-set-map sym-type (lambda (x) (symbol-downcase x)) capsyms))
  ;; capsyms2 is now {c, d} (and is of enum-type sym-type)
  (t= (enum-set-member? capsyms2 'c))
  (t= (not (enum-set-member? capsyms2 'C)))
  (enum-set-for-each (lambda (x) (set! total (symbol-append total x))) capsyms2)
  (== total 'zcd)
  (== 'DCXYZ (enum-set-fold symbol-append 'XYZ capsyms))
  (set! syms (enum-set sym-type 'a 'b 'c 'd 'e))
  ;; syms is now {a, b, c, d, e}
  (t=
    (enum-set=? syms (enum-set-unfold sym-type
       (lambda (i) (> i 96))
       (lambda (i) (string->symbol (string (integer->char i))))
       (lambda (i) (- i 1))
       101)))
  (== '(a) (enum-set->list (enum-set sym-type 'a)))
  (set! syms2 (list->enum-set sym-type '(e f)))
  ;; syms2 is now {e, f}
  (== 2 (enum-set-size syms2))
  (t= (enum-set-member? syms2 'e))
  (t= (enum-set-member? syms2 'f))
)|#

#|(test-group "enums/types"
  (== 2 (enum-type-index sym-type 'c))
  (== '(a b c d e f g h) (enum-type-symbols sym-type))
  (== #f (enum-type-index capsym-type 'c))
  (t= (enum=? sym-type 'a 'a 'a))
  (t= (not (enum=? sym-type 'a 'b 'c)))
  (t= (enum<? sym-type 'a 'c 'e))
  (t= (not (enum<? sym-type 'a 'a 'e)))
  (t= (not (enum<? sym-type 'e 'c 'a)))
  (t= (enum>? sym-type 'e 'c 'a))
  (t= (not (enum>? sym-type 'e 'a 'a)))
  (t= (not (enum>? sym-type 'a 'c 'e)))
  (t= (enum<=? sym-type 'a 'c 'e))
  (t= (enum<=? sym-type 'a 'a 'e))
  (t= (not (enum<=? sym-type 'e 'c 'a)))
  (t= (enum>=? sym-type 'e 'c 'a))
  (t= (enum>=? sym-type 'e 'a 'a))
  (t= (not (enum>=? sym-type 'a 'c 'e)))
)|# ; end enums/ordering

#|(test-group "enums/subenums"
  (define enum-set2 (enum-set capsym-type 'A 'B))
  (define other-set2 (enum-set capsym-type 'A 'B))
  (define enum-set3 (enum-set capsym-type 'A 'B 'C))
  (define enum-set4 (enum-set capsym-type 'A 'B 'C 'D))
  (define enum-setx (enum-set sym-type 'a 'b 'c 'd))
  (t= (enum-set=? enum-set2 other-set2))
  (t= (not (enum-set=? enum-set2 enum-set3)))
  (t= (not (enum-set=? enum-set2 enum-set3 other-set2)))
  (t= (enum-set<? enum-set2 enum-set3 enum-set4))
  (t= (not (enum-set<? enum-set2 other-set2)))
  (t= (enum-set<=? enum-set2 other-set2 enum-set3))
  (t= (not (enum-set<=? enum-set2 enum-set3 other-set2)))
  (t= (enum-set>? enum-set4 enum-set3 enum-set2))
  (t= (not (enum-set>? enum-set2 other-set2)))
  (t= (enum-set>=? enum-set3 other-set2 enum-set2))
  (t= (not (enum-set>=? other-set2 enum-set3 enum-set2)))
)|#

#|(test-group "enums/ops"
  ;; Potentially mutable
  (define abcd (enum-set sym-type 'a 'b 'c 'd))
  (define efgh (enum-set sym-type 'e 'f 'g 'h))
  (define abgh (enum-set sym-type 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (enum-set sym-type 'a 'b 'c 'd))
  (define other-efgh (enum-set sym-type 'e 'f 'g 'h))
  (define other-abgh (enum-set sym-type 'a 'b 'g 'h))
  (define all (enum-set sym-type 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (enum-set sym-type))
  (define ab (enum-set sym-type 'a 'b))
  (define cd (enum-set sym-type 'c 'd))
  (define ef (enum-set sym-type 'e 'f))
  (define gh (enum-set sym-type 'g 'h))
  (define cdgh (enum-set sym-type 'c 'd 'g 'h))
  (define abcdgh (enum-set sym-type 'a 'b 'c 'd 'g 'h))
  (define abefgh (enum-set sym-type 'a 'b 'e 'f 'g 'h))
  (parameterize ((current-test-comparator enum-set=?))
    (== all (enum-set-union abcd efgh))
    (== abcdgh (enum-set-union abcd abgh))
    (== abefgh (enum-set-union efgh abgh))
    (== none (enum-set-intersection abcd efgh))
    (== ab (enum-set-intersection abcd abgh))
    (== ab (enum-set-intersection abgh abcd))
    (== cd (enum-set-difference abcd ab))
    (== abcd (enum-set-difference abcd gh))
    (== none (enum-set-difference abcd abcd))
    (== cdgh (enum-set-xor abcd abgh))
    (== all (enum-set-xor abcd efgh))
    (== none (enum-set-xor abcd other-abcd))
    (== "abcd smashed?" other-abcd abcd)
    (== "efgh smashed?" other-efgh efgh)
    (== "abgh smashed?" other-abgh abgh))
)|#

#|(test-group "enums/mismatch"
  (define capsyms (enum-set capsym-type 'A 'B 'C))
  (define syms (enum-set sym-type 'a 'b 'c))
  (>_< (enum-set=? capsyms syms))
  (>_< (enum-set<? capsyms syms))
  (>_< (enum-set<=? capsyms syms))
  (>_< (enum-set>? capsyms syms))
  (>_< (enum-set>=? capsyms syms))
  (>_< (enum-set-union capsyms syms))
  (>_< (enum-set-intersection capsyms syms))
  (>_< (enum-set-difference capsyms syms))
  (>_< (enum-set-xor capsyms syms))
  (>_< (enum-set-union! capsyms syms))
  (>_< (enum-set-intersection! capsyms syms))
  (>_< (enum-set-difference! capsyms syms))
  (>_< (enum-set-xor! capsyms syms))
)|#

#|(test-group "enums/whole"
  (define (big x) (string>? (symbol->string x) "d"))
  (define whole (enum-set sym-type 'a 'b 'c 'd 'e 'f 'g 'h))
  (define bottom (enum-set sym-type 'a 'b 'c 'd))
  (define top (enum-set sym-type 'e 'f 'g 'h))
  (define-values (topx bottomx)
    (enum-set-partition big whole))
  (parameterize ((current-test-comparator enum-set=?))
    (== top (enum-set-filter big whole))
    (== bottom (enum-set-remove big whole))
    (== top topx)
    (== bottom bottomx))
  (== 4 (enum-set-count big whole))
)|# ; end enums/whole


(test enums/other
  (let* ((ten (make-enum-type '(a b c d e f g h i j)))
         (all (make-universal-enum-set ten))
         (bottom (enum-set ten 'a 'b 'c 'd 'e))
         (top (enum-set ten 'f 'g 'h 'i 'j))
         (top2 (enum-set ten 'f 'g 'h 'i 'j))
         (empty (make-enum-set ten)))
    (== 10 (enum-set-size all))
    (t= (not (enum-set-member? top 'k)))
    (let-current-test-comparator #'enum-set=?
      (== top (enum-set-complement bottom))
      (== bottom (enum-set-complement top))
      (set! top2 (enum-set-complement! top2))
      (== bottom top2))
    (>_< (enum-set-add! top2 10))
    (>_< (enum-set-add! top2 'z))
    (t= (not (enum-set-member? top 10)))
    (== 'a (enum-set-min bottom))
    (== 'a (enum-set-delete-min! bottom))
    ;; bottom is now {b, c, d, e}
    (== 'b (enum-set-min bottom))
    (== 'e (enum-set-max bottom))
    (== 'e (enum-set-delete-max! bottom))
    ;; bottom is now {b, c, d}
    (== 'd (enum-set-max bottom))
    (== cl:NIL (enum-set-min empty))
    (== cl:NIL (enum-set-delete-min! empty))
    (== cl:NIL (enum-set-max empty))
    (== cl:NIL (enum-set-delete-max! empty))))


(test enums/projection
  (let* ((small (make-enum-type '(a b c)))
         (big (make-enum-type '(a b c d e f)))
         (target (enum-set small 'b 'c))
         (source (enum-set big 'b 'c 'd 'e)))
    (t= (enum-set=? target (enum-set-projection source small)))))


;;; *EOF*


