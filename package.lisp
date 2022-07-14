(uiop:define-package #:goal-lib
    (:use #:cl)
    (:export
     :else
     :null?
     :integer?
     :float?
     :true
     :false
     :equal?
     :==
     :!=
     ;;
     :string-append
     ;;
     :list-ref
     ;;
     :first-arr
     :last-arr
     :last-idx-arr
     :arr-idx-of
     ;;
     :set!
     :inc
     :+!
     :1+!
     :dec
     :-!
     :1-!
     :*!
     :/!
     :false!
     :minmax
     :fminmax
     :mimax!
     :fminmax!
     :maxmin
     :fmaxmin
     :&+!
     :&-
     :&->
     :logior!
     :logxor!
     :logand!
     :logclear
     :logclear!
     :logtesta?
     :/-0-guard
     :mod-0-guard
     :float->int
     ;;
     :align-n
     :align16
     :align64
     :bit-field     
   ))

(uiop:define-package #:type-system
  (:use #:cl #:goal-lib)
  (:export
   ;;
   :type-tag
   :type-tag-new 
   ;;
   :typespec
   :typespec-new
   :typespec-args-add
   :typespec-args-count
   :typespec-args-ref
   :typespec-inspect
   :typespec-basetype
   :typespec-add-new-tag
   :typespec-try-get-tag
   :typespec-modify-tag
   :typespec-add-or-modify-tag)
  (:reexport :goal-lib))


