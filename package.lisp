;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-113
  (:use)
  (:export
    :set? :make-set :set-size :set-member? :set-add! :set-delete! :set-for-each
    :set-fold :set-unfold :set :set-copy :set-empty-copy :set-map
    :set->list :list->set :set-filter :set-partition :set-remove
    :set-count :set=? :set<? :set<=? :set>? :set>=? :set-union
    :set-intersection :set-difference :set-xor
    :set-union! :set-intersection! :set-difference! :set-xor! :set-value)
  (:export
    :bag? :make-bag :bag-size :bag-member? :bag-add! :bag-delete! :bag-for-each
    :bag-fold :bag-unfold :bag-element-count
    :bag-increment! :bag-decrement! :bag :bag-copy :bag-empty-copy :bag-map
    :bag->list :list->bag :bag-filter :bag-partition :bag-remove :bag-count
    :bag=? :bag<? :bag<=? :bag>? :bag>=? :bag-union :bag-intersection
    :bag-difference :bag-union! :bag-intersection! :bag-difference!)
  (:export
    :integer-set? :make-integer-set :integer-set-size :integer-set-member?
    :integer-set-add! :integer-set-delete! :integer-set-for-each
    :integer-set-fold :integer-set-unfold :integer-set :integer-set-copy
    :integer-set-empty-copy :integer-set-map :integer-set->list
    :list->integer-set :integer-set-filter :integer-set-partition
    :integer-set-remove :integer-set-count :integer-set=? :integer-set<?
    :integer-set<=? :integer-set>? :integer-set>=? :integer-set-union
    :integer-set-intersection :integer-set-difference
    :integer-set-xor :integer-set-union! :integer-set-intersection!
    :integer-set-difference! :integer-set-xor! :make-universal-integer-set
    :integer-set-complement :integer-set-complement! :integer-set-min
    :integer-set-delete-min! :integer-set-max :integer-set-delete-max!)
  (:export
    :enum-set? :make-enum-set :enum-set-size
    :enum-set-member? :enum-set-add! :enum-set-delete! :enum-set-for-each
    :enum-set-fold :enum-set-unfold :enum-set :enum-set-copy
    :enum-set-empty-copy :enum-set-map :enum-set->list :list->enum-set
    :enum-set-filter :enum-set-partition :enum-set-remove :enum-set-count
    :enum-set=? :enum-set<? :enum-set<=? :enum-set>? :enum-set>=?
    :enum-set-union :enum-set-intersection :enum-set-difference :enum-set-xor
    :enum-set-union! :enum-set-intersection! :enum-set-difference!
    :enum-set-xor! :make-enum-type :enum-type? :make-universal-enum-set
    :enum-set :enum-type-symbols :enum-type-index
    :enum=? :enum<? :enum>? :enum<=? :enum>=? :enum-set-complement
    :enum-set-complement! :enum-set-projection :enum-set-min
    :enum-set-delete-min! :enum-set-max :enum-set-delete-max!))


(defpackage :srfi-113.internal
  (:use :srfi-113 :rnrs :fiveam
        :srfi-69 :srfi-9 :srfi-23
        :srfi-4 
        :mbe
        :named-readtables)
  (:import-from :5am :def-suite*))

