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
   :notequal?
   :==
   :!=
   ;;
   :string-append
   :string-append!
   :string-join
   ;;
   :list-ref
   :array->list
   :list->array
   ;;
   :first-arr
   :last-arr
   :last-idx-arr
   :arr-idx-of
   :arr-new
   :arr-ref
   :arr-set!
   :arr-count
   :arr-push
   ;;
   :hash-map
   :hash-count
   :hash-ref
   :hash-set!
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
   ;;
   :my/with-slots
   )
  )

(uiop:define-package #:type-system/interfaces
  (:use #:cl)
  (:export
   :RegClass-GPR-64
   :RegClass-FLOAT
   :RegClass-INT-128
   :RegClass-VECTOR_FLOAT
   :RegClass-INVALID
   :to-str
   :diff
   :compare
   :is-reference?
   :get-load-size
   :get-load-signed
   :get-size-in-memory
   :get-offset
   :get-in-memory-alignment
   :get-inl-array-stride-align
   :get-inl-array-start-align
   :get-preferred-reg-class)
  )

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
  (:reexport :goal-lib)
  (:reexport :type-system/interfaces)
  )


