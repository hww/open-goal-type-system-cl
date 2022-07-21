;; ----------------------------------------------------------------------------
;;
;; Valeriya P.
;; https://github.com/hww
;; _______ ________ ________
;; |   |   |  |  |  |  |  |  |
;; |       |  |  |  |  |  |  |
;; |___|___|________|________|
;;
;; ----------------------------------------------------------------------------

(in-package :type-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Builtin Type Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add a simple structure type - don't use this outside of add_builtin_types as
;; it forces you to do things in the wrong order.

(defun add-builtin-structure (this parent type-name &optional (boxed nil))
;;(->* (type-system? symbol? symbol?) (boolean?) struct-type?)
  (add-type this type-name (struct-type-new parent type-name boxed false false 0))
  (get-type-of-type this #'struct-type-p type-name))


;; Add a simple basic type - don't use this outside of add-builtin-types as it
;; forces you to do things in the wrong order.

(defun add-builtin-basic (this parent type-name)
  ;;(-> type-system? symbol? symbol? basic-type?)
  (add-type this type-name (basic-type-new parent type-name false 0))
  (get-type-of-type this #'basic-type-p type-name))

;; Add a simple value type - don't use this outside of add-builtin-types as it
;; forces you to do things in the wrong order.

(defun add-builtin-value-type (this parent type-name size &optional (boxed nil) (sign-extend nil) (reg-kind REG-CLASS-GPR-64))
  ;;(->* (type-system? symbol? symbol? integer?) (boolean? boolean? integer?) void)
  (add-type this type-name (value-type-new parent type-name boxed size sign-extend reg-kind))
  (get-type-of-type this #'value-type-p type-name))

;; Helper for inheritance of structure types when setting up builtin types.

(defun builtin-structure-inherit (this st)
  ;;(-> type-system? struct-type? void)
  (struct-type-inherit st (get-type-of-type this #'struct-type-p (gtype-get-parent st))))

;; Add types which are built-in to GOAL.

(defun add-builtin-types (this)
  ;;(-> type-system? void?)

  (let* ((obj-type (types-find this "object"))
         ;; some of the basic types have confusing circular dependencies so this is
         ;; done manually. there are no inlined things so its ok to do some things out
         ;; of order because the actual size doesn't really matter.


         (structure-type   (add-builtin-structure this "object" "structure"))
         (basic-type       (add-builtin-basic this "structure" "basic"))
         (symbol-type      (add-builtin-basic this "basic" "symbol"))
         (type-type        (add-builtin-basic this "basic" "type"))
         (string-type      (add-builtin-basic this "basic" "string")))

    (basic-type-set-final string-type)  ;; no virtual calls used on string.
    (let* ((function-type    (add-builtin-basic this "basic" "function"))
           (vu-function-type (add-builtin-structure this "structure" "vu-function"))
           (link-block-type  (add-builtin-basic this "basic" "link-block"))
           (kheap-type       (add-builtin-structure this "structure" "kheap"))
           (array-type       (add-builtin-basic this "basic" "array"))
           (pair-type        (add-builtin-structure this "object" "pair" true))
           (connectable-type (add-builtin-structure this "structure" "connectable"))
           (file-stream-type (add-builtin-basic this "basic" "file-stream")))
      ;;;
      (add-builtin-value-type this "object" "pointer" 4)
      ;;;
      (let ((inline-array-type (add-builtin-value-type this "object" "inline-array" 4)))
              (gtype-set-runtime-type  inline-array-type "pointer"))

      (add-builtin-value-type this "object" "number" 8)  ;; sign extend?
      (add-builtin-value-type this "number" "float" 4 false false REG-CLASS-FLOAT)
      (add-builtin-value-type this "number" "integer" 8 false false)   ;; sign extend?
      (add-builtin-value-type this "integer" "binteger" 8 true false)  ;; sign extend?
      (add-builtin-value-type this "integer" "sinteger" 8 false true)
      (add-builtin-value-type this "sinteger" "int8" 1 false true)
      (add-builtin-value-type this "sinteger" "int16" 2 false true)
      (add-builtin-value-type this "sinteger" "int32" 4 false true)
      (add-builtin-value-type this "sinteger" "int64" 8 false true)
      (add-builtin-value-type this "sinteger" "int128" 16 false true REG-CLASS-INT-128)
      (add-builtin-value-type this "integer" "uinteger" 8)
      (add-builtin-value-type this "uinteger" "uint8" 1)
      (add-builtin-value-type this "uinteger" "uint16" 2)
      (add-builtin-value-type this "uinteger" "uint32" 4)
      (add-builtin-value-type this "uinteger" "uint64" 8)
      (add-builtin-value-type this "uinteger" "uint128" 16 false false REG-CLASS-INT-128)

      ;; (add special units types.
      (let ((meters (add-builtin-value-type this "float" "meters" 4 false false REG-CLASS-FLOAT))
            (degrees (add-builtin-value-type this "float" "degrees" 4 false false REG-CLASS-FLOAT))
            (seconds (add-builtin-value-type this "int64" "seconds" 8 false true)))
        (gtype-set-runtime-type meters "float")
        (gtype-set-runtime-type degrees "float")
        (gtype-set-runtime-type seconds"int64"))

      (let ((int-type (add-builtin-value-type this "integer" "int" 8 false true))
            (uint-type (add-builtin-value-type this "uinteger" "uint" 8 false false)))
        (gtype-disallow-in-runtime int-type)
        (gtype-disallow-in-runtime uint-type))

      ;; Methods and Fields
      (forward-declare-type-as this "'memory-usage-block" "basic")

      ;; OBJECT
      (declare-method this obj-type "new" false (make-function-typespec this '("symbol" "type" "int") "_type_") false)
      (declare-method this obj-type "delete" false (make-function-typespec this '("_type_") "none") false)
      (declare-method this obj-type "print" false (make-function-typespec this '("_type_") "_type_") false)
      (declare-method this obj-type "inspect" false (make-function-typespec this '("_type_") "_type_") false)
      (declare-method this obj-type "length" false (make-function-typespec this '("_type_") "int") false)  ;; todo - this integer type?
      (declare-method this obj-type "asize-of" false (make-function-typespec this '("_type_") "int") false)
      (declare-method this obj-type "copy" false (make-function-typespec this '("_type_" "symbol") "_type_") false)
      (declare-method this obj-type "relocate" false (make-function-typespec this '("_type_" "int") "_type_") false)
      (declare-method this obj-type "mem-usage" false (make-function-typespec this '("_type_" "memory-usage-block" "int") "_type_") false)

      ;; STRUCTURE
      ;; structure new doesn"t" support dynamic sizing which is kinda weird - it grabs the size from
      ;; the type.  Dynamic structures use new-dynamic-structure which is used exactly once ever.
      (declare-method this structure-type "new" false (make-function-typespec this '("symbol" "type") "_type_") false)
      ;; structure-type is a field-less StructureType so we have to do this to match the runtime.
      ;;  structure_type_>override-size-in-memory(4)

      ;; BASIC
      ;; we intentionally don"t" inherit from structure because structure"s" size is weird.
      (add-field-to-type this basic-type "type" (make-typespec this "type"))
      ;; the default new basic doesn"t" support dynamic sizing. anything dynamic will override this
      ;; and then call (method object new) to do the dynamically-sized allocation.
      (declare-method this basic-type "new" false (make-function-typespec this '("symbol" "type") "_type_") false)

      ;; SYMBOL
      (builtin-structure-inherit this symbol-type)
      (add-field-to-type this symbol-type "value" (make-typespec this "object"))
      ;; a new method which returns type none means new is illegal.
      (declare-method this symbol-type "new" false (make-function-typespec this '() "none") false)

      ;; TYPE
      (builtin-structure-inherit this type-type)
      (declare-method this type-type "new" false (make-function-typespec this '("symbol" "type" "int") "_type_") false)
      (add-field-to-type this type-type "symbol" (make-typespec this "symbol"))
      (add-field-to-type this type-type "parent" (make-typespec this "type"))
      (add-field-to-type this type-type "size" (make-typespec this "uint16"))  ;; actually u16
      (add-field-to-type this type-type "psize" (make-typespec this "uint16"))  ;; todo u16 or s16. what really is this?
      (add-field-to-type this type-type "heap-base" (make-typespec this "uint16"))         ;; todo
      (add-field-to-type this type-type "allocated-length" (make-typespec this "uint16"))  ;; todo
      (add-field-to-type this type-type "method-table" (make-typespec this "function") false true)

      ;; STRING

      (builtin-structure-inherit this string-type)
      (add-field-to-type this string-type "allocated-length" (make-typespec this "int32"))   ;; todo integer type
      (add-field-to-type this string-type "data" (make-typespec this "uint8") false true)  ;; todo integer type
      ;; string is never deftype"d" for the decompiler so we need to manually give the constructor
      ;; type here.
      (declare-method this string-type "new" false (make-function-typespec this '("symbol" "type" "int" "string") "_type_") false)

      ;; FUNCTION
      (builtin-structure-inherit this function-type)
      ;; ???

      ;; VU FUNCTION
      ;; don"t" inherit
      (add-field-to-type this vu-function-type "length" (make-typespec this "int32"))   ;; todo integer type
      (add-field-to-type this vu-function-type "origin" (make-typespec this "int32"))   ;; todo sign extend?
      (add-field-to-type this vu-function-type "qlength" (make-typespec this "int32"))  ;; todo integer type
      (add-field-to-type this vu-function-type "data" (make-typespec this "uint8") false true)

      ;; link block
      (builtin-structure-inherit this link-block-type)
      (add-field-to-type this link-block-type "allocated-length" (make-typespec this "int32"))  ;; todo integer type
      (add-field-to-type this link-block-type "version" (make-typespec this "int32"))  ;; todo integer type
      ;; there"s" probably some dynamically sized stuff after this...

      ;; kheap
      (add-field-to-type this kheap-type "base" (make-typespec this "pointer"))
      (add-field-to-type this kheap-type "top" (make-typespec this "pointer"))
      (add-field-to-type this kheap-type "current" (make-typespec this "pointer"))
      (add-field-to-type this kheap-type "top-base" (make-typespec this "pointer"))

      ;; todo
      (builtin-structure-inherit this array-type)
      (declare-method this array-type "new" false (make-function-typespec this '("symbol" "type" "type" "int") "_type_") false)
      ;; array has: number number type
      (add-field-to-type this array-type "length" (make-typespec this "int32"))
      (add-field-to-type this array-type "allocated-length" (make-typespec this "int32"))
      (add-field-to-type this array-type "content-type" (make-typespec this "type"))
      (add-field-to-type this array-type "data" (make-typespec this "uint8") false true)

      ;; pair
      (struct-type-override-offset pair-type 2)
      (declare-method this pair-type "new" false (make-function-typespec this '("symbol" "type" "object" "object") "_type_") false)
      (add-field-to-type this pair-type "car" (make-typespec this "object"))
      (add-field-to-type this pair-type "cdr" (make-typespec this "object"))

      ;; this type is very strange as the compiler knows about it in gkernel-h yet it is
      ;; defined inside of connect.
      (add-field-to-type this connectable-type "next"0 (make-typespec this "connectable"))
      (add-field-to-type this connectable-type "prev"0 (make-typespec this "connectable"))
      (add-field-to-type this connectable-type "next"1 (make-typespec this "connectable"))
      (add-field-to-type this connectable-type "prev"1 (make-typespec this "connectable"))

      ;; todo
      (builtin-structure-inherit this file-stream-type)
      (add-field-to-type this file-stream-type "flags" (make-typespec this "uint32"))
      (add-field-to-type this file-stream-type "mode" (make-typespec this "symbol"))
      (add-field-to-type this file-stream-type "name" (make-typespec this "string"))
      (add-field-to-type this file-stream-type "file" (make-typespec this "uint32"))
      (declare-method this file-stream-type "new" false (make-function-typespec this '("symbol" "type" "string" "symbol") "_type_") false)
      "Builtings Initialized"
      )))

(defun type-system-new! ()
  ;;(-> type-system?)
  (let ((this (type-system-new)))
    (add-builtin-types this)
    this))

;; Type spec text--------------------------------------------------------

;(let ((this (type-system-new)))
;  (add-builtin-types this)
;  (print (inspect-all-type-information this)))
