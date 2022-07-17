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
   :list->string
   :append!
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
   :get-preferred-reg-class
   )
  )

(uiop:define-package #:type-system/typespec
  (:use #:cl #:goal-lib #:type-system/interfaces) 
  (:export
   ;;
   :type-tag
   :type-tag-new 
   ;;
   :typespec
   :typespec-p
   :typespec-new
   :typespec-basetype
   ;;
   :typespec-args-add
   :typespec-args-count
   :typespec-args-ref
   :typespec-args-empty?
   ;;
   :typespec-add-new-tag
   :typespec-try-get-tag
   :typespec-modify-tag
   :typespec-add-or-modify-tag
   :typespec-tags-empty?
   )
  )

(uiop:define-package #:type-system/type
  (:use #:cl #:type-system/interfaces #:goal-lib #:type-system/typespec)
  (:export
   :GOAL-NEW-METHOD
   :GOAL-DEL-METHOD
   :GOAL-PRINT-METHOD
   :GOAL-INSPECT-METHOD
   :GOAL-LENGTH-METHOD
   :GOAL-ASIZE-METHOD
   :GOAL-COPY-METHOD
   :GOAL-RELOC-METHOD
   :GOAL-MEMUSAGE-METHOD
   :method-info
   :method-info-new
   :type-flags
   :type-flags-new
   :type-flags-flag
   :gtype
   :gtype-new
   :gtype-disallow-in-runtime
   :gtype-has-parent?
   :gtype-get-parent
   :gtype-get-my-method
   :gtype-get-my-method-by-id
   :gtype-get-my-last-method
   :gtype-get-my-new-method
   :gtype-add-method
   :gtype-add-new-method
   :gtype-set-runtime-type
   :gtype-add-state
   :gtype-find-state
   :incompatible-diff
   )
  )




(uiop:define-package #:type-system
  (:use #:cl
	#:goal-lib
	#:type-system/type
	#:type-system/interfaces
	#:type-system/typespec
	) 
  (:export
   )
  (:reexport :goal-lib)
  (:reexport :type-system/interfaces)
  (:reexport :type-system/type)
  (:reexport :type-system/typespec)
  )


