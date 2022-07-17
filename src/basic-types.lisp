#lang racket/base
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

(require racket/struct
         racket/contract
         racket/list
         racket/format
         data/gvector)
(require (for-syntax racket/base syntax/parse))

(require "vmc-lib.rkt" "type.rkt" "type-spec.rkt" "interfaces.rkt")

(provide (all-defined-out))

(define POINTER-SIZE 4)
(define STRUCTURE-ALIGNMENT 16)


;; ----------------------------------------------------------------------------
;; The field type
;; ----------------------------------------------------------------------------

(define-struct field
  (
   name              ; string?
   type              ; type-spec?
   offset            ; int? -1;
   is-inline         ; bool? false does not make sense if m-type is value, and not an array and not dynamic
   is-dynamic        ; bool? false;
   is-array          ; bool? false;
   array-size        ; int? 0;
   alignment         ; int?-1;
   skip-in-static-decomp; bool? = false;
   is-user-placed    ; bool? = false;  // was this field placed manually by the user?
   field-score       ; double? = 0.;
   )
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [
   (define (inspect this)
     (field-inspect this)) ]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (field-diff this other))
   ]
  )

(define (field-new name type offset)
  (-> symbol? type-spec? field?)
  (field
   name   ; name string?
   type   ; type type-spec?
   offset ; offset int? -1;
   #f     ; is-inline bool? false does not make sense if m-type is value, and not an array and not dynamic
   #f     ; is-dynamic bool? false;
   #f     ; is-array bool? false;
   0      ; array-size int? 0;
   #f     ; alignment int?-1;
   #f     ; skip-in-static-decom bool? = false;
   #f     ; is-user-paced bool? = false;  // was this field placed manually by the user?
   #f     ; field-score double? = 0.;
))


(define/contract (field-is-array? this)
  (-> field? boolean?)
  (field-is-array this))
;; Mark a field as inline. This has a different meaning depending on if the field is also an array
(define/contract (field-set-inline this)
  (-> field? void)
  (set-field-is-inline! this #t))
;; Mark a field as dynamic, indicating it has array semantics.
(define/contract (field-set-dynamic this)
  (-> field? void)
  (set-field-is-dynamic! this #t))
;; Mark a field as a fixed size array.
(define/contract (field-set-array this size)
  (-> field? integer? void)
  (set-field-array-size! this size)
  (set-field-is-array! this true))

(define/contract (field-set-alignment this align)
  (-> field? integer? void)
  (set-field-alignment! this align))

(define/contract (field-set-offset this align)
  (-> field? integer? void)
  (set-field-offset! this align))

(define/contract (field-set-skip-in-static-decomp this)
  (-> field? void   )
  (set-field-skip-in-static-decomp! this #t))

(define/contract (field-mark-as-user-placed this)
  (-> field? void)
  (set-field-is-user-placed! this #t))

(define/contract (field-set-field-score this score)
  (-> field? integer? void)
  (set-field-field-score! this score))



(define (field-diff this other)
  (unless (field? other) (error (incompatible-diff 'field other)))
  (define-with-struct l. (field name type offset is-inline is-dynamic
                                is-array array-size alignment  skip-in-static-decomp
                                is-user-placed field-score) this)
  (define-with-struct r. (field name type offset is-inline is-dynamic
                                is-array array-size alignment  skip-in-static-decomp
                                is-user-placed field-score) other)
  (define result "")
  (when (!= l.name r.name)
    (string-append! result (format "name: ~a vs. ~a\n" l.name r.name)))
  (when (!= l.type r.type)
    (string-append! result (format "type: ~a vs. ~a\n" (inspect l.type) (inspect r.type))))
  (when (!= l.offset r.offset)
    (string-append! result (format "offset: ~a vs. ~a\n" l.offset r.offset)))
  (when (!= l.is-inline r.is-inline)
    (string-append! result (format "inline: ~a vs. ~a\n" l.is-inline r.is-inline)))
  (when (!= l.is-dynamic r.is-dynamic)
    (string-append! result (format "dynamic: ~a vs. ~a\n" l.is-dynamic r.is-dynamic)))
  (when (!= l.is-array r.is-array)
    (string-append! result (format "array: ~a vs. ~a\n" l.is-array r.is-array)))
  (when (!= l.array-size r.array-size)
    (string-append! result (format "array-size: ~a vs. ~a\n" l.array-size r.array-size)))
  (when (!= l.alignment r.alignment)
    (string-append! result (format "alignment: ~a vs. ~a\n" l.alignment r.alignment)))
  (when (!= l.skip-in-static-decomp r.skip-in-static-decomp)
    (string-append! result (format "skip-in-static-decomp: ~a vs. ~a\n"
                                   l.skip-in-static-decomp
                                   r.skip-in-static-decomp)))
  result)

(define/contract (field-inspect this)
  (-> field? string?)
  (format
   "[Field] ~a inline ~a dynamic: ~a array: ~a array-size: ~a align: ~a skip: ~a"
   (format "(~a type: ~a offset: ~a)"
                (field-name this)
                (inspect (field-type this))
                (field-offset this))
   (~a (field-is-inline this) #:width 5)
   (~a (field-is-dynamic this) #:width 5)
   (~a (field-is-array this) #:width 5)
   (~a (field-array-size this) #:width 3)
   (~a (field-alignment this) #:width 2)
   (field-skip-in-static-decomp this)))

;; ----------------------------------------------------------------------------
;; The null type
;;
;; Used only for "none" - this is a type that the compiler can use for "this
;; has no value". Attempting to do anything with a NoneType is an error.
;; ----------------------------------------------------------------------------

(define-struct (null-type type)
  ()
  #:transparent
  #:mutable
  #:methods gen:inspectable
  [
   (define (inspect this)
     (format "[~a]" (symbol->string (type-name this))))
   ]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (null-type-diff this other))
   ]
  #:methods gen:typeable
  [
   (define (is-reference? this)
     (error "is_reference called on NullType"))
   (define (get-load-size this)
     (error "get_load_size called on NullType"))
   (define (get-size-in-memory this)
     (error "get_size_in_memory called on NullType"))
   (define (get-load-signed this)
     (error "get_load_size called on NullType"))
   (define (get-offset this)
     (error "get_offset called on NoneType"))
   (define (get-in-memory-alignment this)
     (error "get_in_memory_alignment called on NullType"))
   (define (get-inl-array-stride-align this)
     (error "get_inline_array_start_alignment called on NullType"))
   (define (get-inl-array-start-align this)
     (error "get_inline_array_stride_alignment called on NullType"))
   (define (get-preferred-reg-class this)
     (error "get_preferred_reg_class called on NullType"))
   ]
  )

;; Constructor

(define/contract (null-type-new name)
  (-> symbol? null-type?)
  (let ((base (type-new EMPTY-SYMBOL name #f 0)))
    (type-copy null-type base)))

(define (null-type-diff this other)
  (unless (null-type? other) (error (incompatible-diff 'null-type other)))
  (define result "")
   (string-append! result (type-diff this other))
  (when (!= this other)
    (string-append! result "NullType error"))
  result)


;; ----------------------------------------------------------------------------
;; A type which is treated as a value, as opposed to a reference. These types
;; fit in a register and are always passed by value in arguments/returns.
;; ----------------------------------------------------------------------------

(define-struct (value-type type)
  (size        ; int? -1;
   offset      ; int? 0;
   sign-extend ; bool? false;
   reg-kind    ; RegClass::INVALID;
   )
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [
   (define (inspect this)
     (define result "")
     (string-append! result (format "[ValueType] ~a\n parent: ~a\n boxed: ~a\n size: ~a\n sext: ~a\n"
                                    (type-name this)
                                    (type-parent this)
                                    (type-is-boxed this)
                                    (value-type-size this)
                                    (value-type-sign-extend this)))
     (string-append! result (type-methods-inspect this))
     result)
  ]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (value-type-diff this other))
   ]
  #:methods gen:typeable
  [
   (define (is-reference? this)
     #f)
   (define (get-load-size this)
     (value-type-size this))
   (define (get-size-in-memory this)
     (value-type-size this))
   (define (get-load-signed this)
     (value-type-sign-extend this))
   (define (get-offset this)
     (value-type-offset this))
   (define (get-in-memory-alignment this)
     (value-type-size this))
   (define (get-inl-array-stride-align this)
     (value-type-size this))
   (define (get-inl-array-start-align this)
     (value-type-size this))
   (define (get-preferred-reg-class this)
     (value-type-reg-kind this))
   ]
  )

;; Constructor

(define/contract (value-type-new parent name is-boxed size sign-extend (reg-kind RegClass::INVALID))
  (->* (symbol? symbol? boolean? integer? boolean?) (integer?) value-type?)
  (let* ((base (type-new parent name is-boxed 0))
         (newt (type-copy value-type base size 0 sign-extend reg-kind)))
    newt))

;; Diff for this type

(define (value-type-diff this other)
  (unless (value-type? other) (error (incompatible-diff 'value-type other)))
  (define-with-struct l. (value-type size offset sign-extend reg-kind) this)
  (define-with-struct r. (value-type size offset sign-extend reg-kind) other)

  (define result "")
  (string-append! result (type-diff this other))
  (when (!= l.size r.size)
    (string-append! result (format "size: ~a vs. ~a\n" l.size r.size)))
  (when (!= l.offset r.offset)
    (string-append! result (format "offset: ~a vs. ~a\n" l.offset r.offset)))
  (when (!= l.sign-extend r.sign-extend)
    (string-append! result (format "sign-extend: ~a vs. ~a\n" l.sign-extend r.sign-extend)))
  (when (!= l.reg-kind r.reg-kind)
    (string-append! result (format "reg-kind: ~a vs ~a\n" l.reg-kind r.reg-kind)))
  result)

(define/contract (value-type-inherit this parent)
  (-> value-type? value-type? void)
  (set-value-type-sign-extend! this (value-type-sign-extend parent))
  (set-value-type-size! this (value-type-size parent))
  (set-value-type-offset! this (value-type-offset parent))
  (set-value-type-reg-kind! this (value-type-reg-kind parent)))

;; TEST -----------------------------------------------------------------------
(module+ test
  (require rackunit)
  (check-equal? (inspect (value-type-new 'foo 'bar #f 10 #t))
                "[ValueType] bar\n parent: foo\n boxed: #f\n size: 10\n sext: #t\n")
  )

;; ----------------------------------------------------------------------------
;; The refference type
;;
;; ReferenceType is an abstract class that's a parent for everything that uses
;; reference semantics. This means this type behaves like a C pointer - the
;; thing that's passed around is a reference to some memory somewhere.
;; ----------------------------------------------------------------------------

(define-struct (reference-type type)
  ()
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [
   (define (inspect this)
     (string-append
      (format "[ReferenceType] ~a\n parent: ~a\n boxed: ~a\n"
              (type-name this) (type-parent this)
              (type-is-boxed this))
      (type-methods-inspect this)))
   ]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (type-diff this other))
   ]
  #:methods gen:typeable
  [
   (define (is-reference? this)
     #t)
   (define (get-load-size this)
     POINTER-SIZE)
   (define (get-size-in-memory this)
     POINTER-SIZE)
   (define (get-load-signed this)
     #f)
   (define (get-offset this)
     POINTER-SIZE)
   (define (get-in-memory-alignment this)
     POINTER-SIZE)
   (define (get-inl-array-stride-align this)
     POINTER-SIZE)
   (define (get-inl-array-start-align this)
     POINTER-SIZE)
   (define (get-preferred-reg-class this)
     RegClass::GPR-64)
   ]
  )

;; Constructor

(define/contract (reference-type-new parent name is-boxed heap-base)
  (-> symbol? symbol? boolean? integer? reference-type?)
  (let ((base (type-new parent name is-boxed heap-base)))
    (type-copy reference-type base)))

;; TEST -----------------------------------------------------------------------
(module+ test
  (require rackunit)
  (check-equal? (inspect (reference-type-new 'foo 'bar #f 10))
                "[ReferenceType] bar\n parent: foo\n boxed: #f\n")
  )


;; ----------------------------------------------------------------------------
;; The struct type
;;
;; StructureType is a ReferenceType which has fields.  It's also the parent of BasicType,
;; which is a structure with runtime typing information.
;; ----------------------------------------------------------------------------

(define-struct (struct-type reference-type)
  (
     fields                     ; std::vector<Field>
     dynamic                    ; bool? = false;
     size-in-mem                ; int? = 0;
     pack                       ; bool? = false;
     allow-misalign             ; bool? = false;
     offset                     ; int? = 0;
     always-stack-singleton     ; bool? = false;
     idx-of-first-unique-field  ; int? = 0;
   )
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [(define (inspect this)
     (struct-type-inspect this))]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (struct-type-diff this other))
   ]
  #:methods gen:typeable
  [
   (define (is-reference? this)
     #t)
   (define (get-load-size this)
     POINTER-SIZE)
   (define (get-size-in-memory this)
     (struct-type-size-in-mem this))
   (define (get-load-signed this)
     #f)
   (define (get-offset this)
     (struct-type-offset this))
   (define (get-in-memory-alignment this)
     STRUCTURE-ALIGNMENT)
   (define (get-inl-array-stride-align this)
     (get-inl-array-stride-align-impl this))
   (define (get-inl-array-start-align this)
     (get-inl-array-start-align-impl this))
   ]
  )

(define/contract (struct-type-new parent name boxed dynamic pack heap-base)
  (-> symbol? symbol? boolean? boolean? boolean? integer? struct-type?)
  (let ((base (reference-type-new parent name boxed heap-base)))
    (type-copy struct-type base
               (make-gvector) ; fields
               dynamic
               0             ; size_in_mem
               pack
               #f            ; allow_misalign
               0             ; offset
               #f            ; always_stack_singleton
               0             ; idx_of_first_unique_field
               )))

(define/contract (struct-type-inspect this)
  (-> struct-type? string?)
  (define result "")
  (string-append! result (format "[StructType] ~a\n parent: ~a\n boxed: ~a\n size: ~a\n pack: ~a\n"
                                 (type-name this)
                                 (type-parent this)
                                 (type-is-boxed this)
                                 (struct-type-size-in-mem this)
                                 (struct-type-pack this)))
  (string-append! result (format " misalign: ~a\n heap-base: ~a\n stack-singleton: ~a\n fields:\n~a\n"
                                 (struct-type-allow-misalign this)
                                 (type-heap-base this)
                                 (struct-type-always-stack-singleton this)
                                 (struct-type-inspect-fields this)))
  (string-append! result " methods:\n")
  (string-append! result (type-methods-inspect this))
  (string-append! result "\n")
  result)

;; Diff for this type

(define (struct-type-diff this other)
  (unless (struct-type? other) (error (incompatible-diff 'struct-type other)))
  (define-with-struct l. (struct-type
                          fields dynamic size-in-mem pack allow-misalign offset
                          always-stack-singleton idx-of-first-unique-field) this)
  (define-with-struct r. (struct-type
                          fields dynamic size-in-mem pack allow-misalign offset
                          always-stack-singleton idx-of-first-unique-field) other)
  (define l.fields.size (gvector-count l.fields))
  (define r.fields.size (gvector-count r.fields))
  (define min-fields (min l.fields.size r.fields.size))
  (define result "")
  (string-append! result (type-diff this other))
  (when (!= l.fields r.fields)
    (when (!= l.fields.size r.fields.size)
      (string-append! result (format "Number of fields ~a vs. ~a\n" l.fields.size r.fields.size)))

    (for ((i (in-range min-fields)))
      (let ((lf (gvector-ref l.fields i))
            (rf (gvector-ref r.fields i)))
        (when (!= lf rf)
          (string-append! result (format "field ~a (~a/~a):\n"
                                         i (field-name lf) (field-name rf)))
          (field-diff lf rf)))))

  (when (!= l.dynamic r.dynamic)
    (string-append! result (format "dynamic: ~a vs. ~a\n" l.dynamic r.dynamic)))
  (when (!= l.size-in-mem r.size-in-mem)
    (string-append! result (format "size-in-mem: ~a vs. ~a\n" l.size-in-mem r.size-in-mem)))
  (when (!= l.pack r.pack)
    (string-append! result (format "pack: ~a vs. ~a\n" l.pack r.pack)))
  (when (!= l.allow-misalign r.allow-misalign)
    (string-append! result (format "allow-misalign: ~a vs. ~a\n" l.allow-misalign r.allow-misalign)))
  (when (!= l.always-stack-singleton r.always-stack-singleton)
    (string-append! result (format "always-stack-singleton: ~a vs. ~a\n"
                                   l.always-stack-singleton
                                   r.always-stack-singleton)))
  (when (!= l.offset r.offset)
    (string-append! result (format "offset: ~a vs. ~a\n" l.offset r.offset)))
  (when (!= l.idx-of-first-unique-field r.idx-of-first-unique-field)
    (string-append! result (format "idx-of-first-unique-field: ~a vs. ~a\n"
                                   l.idx-of-first-unique-field
                                   r.idx-of-first-unique-field)))
  result)

;; So the GOAL compiler was weird here.
;; It seems like there were two states:
;; - don't care about alignment of both the first element and the later
;; - don't care about the alignment, but pad the stride.
;; so you end up with a misaligned array of padded structures which seems very stupid.

(define (get-inl-array-stride-align-impl this)
  (cond
    ((struct-type-pack this)
    ;; make elements of inline array the minimum allowable alignment.
    (let ((alignment  1))
      ;; TODO - I don't know if GOAL actually did this check, maybe packed inline arrays could
      ;; violate these?
      (for/gvector ((field  (in-gvector (struct-type-fields this))))
        (set! alignment (max alignment  (field-alignment field))))
      alignment))
    (else
     ;; make elements of inline array properly aligned structures
     STRUCTURE-ALIGNMENT)))

(define (get-inl-array-start-align-impl this)
  (cond
    ((or (struct-type-pack this) (struct-type-allow-misalign this))
     ;; make elements of inline array the minimum allowable alignment.
     (let ((alignment  1))
       ;; TODO - I don't know if GOAL actually did this check, maybe packed inline arrays could
       ;; violate these?
       (for/gvector ((field (in-gvector (struct-type-fields this))))
         (set! alignment (max alignment  (field-alignment field))))
       alignment))
    (else
     ;; make elements of inline array properly aligned structures
     STRUCTURE-ALIGNMENT)))

;; Find the filed by name

(define/contract (struct-type-lookup-field this name)
  (-> struct-type? symbol? (or/c #f field?))
  (for/gvector-find-item
      (lambda (field)
        (== (field-name field) name))
      (struct-type-fields this)))

;; Print all fields

(define/contract (struct-type-inspect-fields this)
  (-> struct-type? string?)
  (apply string-append
         (map (lambda (f) (format "    ~a\n" (inspect f)))
              (gvector->list (struct-type-fields this)))))

;; Find the method wih name

(define/contract (struct-lookup-field this name)
  (-> struct-type? symbol? method-info?)
  (for/gvector-find-item
      (lambda (x) (eq? (method-info-name x) name))))

(define/contract (struct-type-inherit this parent)
  (-> struct-type? struct-type? void)
    (set-struct-type-fields! this (list->gvector (gvector->list (struct-type-fields parent))))
  (set-struct-type-dynamic! this (struct-type-dynamic parent))
  (set-struct-type-size-in-mem! this (struct-type-size-in-mem parent))
  (set-struct-type-idx-of-first-unique-field! this (gvector-count (struct-type-fields parent))))

(define/contract (struct-type-add-field this f new-size-in-mem)
  (-> struct-type? field? integer? void)
  (gvector-add! (struct-type-fields this) f)
  ;(printf "ADD FIELD (~a) NEW-SIZE ~a\n" (inspect f) new-size-in-mem)
  (set-struct-type-size-in-mem! this new-size-in-mem))

(define/contract (struct-type-get-size-in-memory this)
  (-> struct-type? integer?)
  (struct-type-size-in-mem this))

(define/contract (struct-type-override-size-in-memory this size)
  (-> struct-type? integer? void?)
  (set-struct-type-size-in-mem! this size))

(define/contract (struct-type-override-offset this offset)
  (-> struct-type? integer? void)
  (set-struct-type-offset! this offset))

(define/contract (struct-type-fields-count this)
  (-> struct-type? integer?)
  (gvector-count (struct-type-fields this)))

(define/contract (struct-type-fields-ref this idx)
  (-> struct-type? integer? field?)
  (gvector-ref (struct-type-fields this) idx))

;; TEST -----------------------------------------------------------------------
(module+ test
  (require rackunit)

  (check-equal? (inspect (struct-type-new EMPTY-SYMBOL 'foo #f #f #f  10))
                "[StructType] foo\n parent: \n boxed: #f\n size: 0\n pack: #f\n misalign: #f\n heap-base: 10\n stack-singleton: #f\n fields:\n\n methods:\n\n"))


;; ----------------------------------------------------------------------------
;; The basic type
;; ----------------------------------------------------------------------------

(define-struct (basic-type struct-type)
  (
   is-final ; bool?
   )
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [(define (inspect this)
     (string-append
      (struct-type-inspect this)
      (format " is-final:~a\n" (basic-type-is-final this))))]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (basic-type-diff this other))
   ]
  )

(define-syntax (basic-type-new stx)
  (syntax-parse stx
    ((_ parent name dynamic heap-base)
     (syntax/loc stx
       (let ((base (struct-type-new parent name #t dynamic #f heap-base)))
         (type-copy basic-type base #f))))
    ((_ parent name)
     (syntax/loc stx
       (let ((base (struct-type-new parent name #f #f #f 0)))
         (type-copy basic-type base #f))))))

(define (basic-type-diff this other)
  (unless (basic-type? other) (error (incompatible-diff 'basic-type other)))
  (define-with-struct l. (basic-type is-final) this)
  (define-with-struct r. (basic-type is-final) other)
  (string-append
   (flatten
    (list
     (struct-type-diff this other)
     (when (!= l.is-final r.is-final)
       (format "final: ~a vs. ~a\n" l.is-final r.is-final))))))

;; Make this type as final

(define/contract (basic-type-set-final this)
  (-> basic-type? void)
  (set-basic-type-is-final! this #t))


;; ----------------------------------------------------------------------------
;; Bitfield
;; ----------------------------------------------------------------------------

(define-struct sbitfield (
  type                  ; type-spec?
  name                  ; string?
  offset                ; int? -1;  in bits
  size                  ; int = -1; in bits.
  skip-in-static-decomp ; bool? = false;
  )
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [(define (inspect this) (sbitfield-inspect this))]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (sbitfield-diff this other))
   ]
  )

(define/contract (sbitfield-new type name offset size skip-in-static-decomp)
  (-> type-spec? symbol? integer? integer?  boolean? sbitfield?)
  (make-sbitfield type name offset size skip-in-static-decomp))

(define/contract (sbitfield-inspect this)
  (-> sbitfield? string?)
  (format "[~a ~a] sz ~a off ~a"
          (sbitfield-name this)
          (inspect (sbitfield-type this))
          (sbitfield-size this)
          (sbitfield-offset this)))

;; Diff

(define/contract (sbitfield-diff this other)
  (-> sbitfield? type? void)
  (unless (sbitfield? other) (error (incompatible-diff 'sbitfield other)))
  (define-with-struct l. (sbitfield type name offset size skip-in-static-decomp) this)
  (define-with-struct r. (sbitfield type name offset size skip-in-static-decomp) other)
  (define result "")
  (when (!= l.type r.type)
    (string-append! result (format "type: ~a vs. ~a\n" (inspect l.type) (inspect r.type))))
  (when (!= l.name r.name)
    (string-append! result (format "name: ~a vs. ~a\n" l.name r.name)))
  (when (!= l.offset r.offset)
    (string-append! result (format "offset: ~a vs. ~a\n" l.offset r.offset)))
  (when (!= l.size r.size)
    (string-append! result (format "size: ~a vs. ~a\n" l.size r.size)))
  (when (!= l.skip-in-static-decomp r.skip-in-static-decomp)
    (string-append! result (format "skip-in-static-decomp: ~a vs. ~a\n" l.skip-in-static-decomp
                                   r.skip-in-static-decomp)))
  result)

;; ----------------------------------------------------------------------------
;; BitfieldType
;; ----------------------------------------------------------------------------

(define-struct (bitfield-type value-type)
  (fields) ; sbitfield?
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [
   (define (inspect this) (bitfield-type-inspect this))
   ]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (bitfield-type-diff this other))
   ]
  )

(define/contract (bitfield-type-new parent name  size sign-extend)
  (-> symbol? symbol? integer? boolean? bitfield-type?)
  (let ((base (value-type-new parent name #f size sign-extend)))
    (type-copy bitfield-type base (make-gvector #:capacity 8))))

(define/contract (bitfield-type-inspect this)
  (-> bitfield-type? string?)
  (define result "")
  (string-append!
   result
   (format "Parent type: ~a\nFields:\n" (type-parent this)))

  (for ((it (in-gvector (bitfield-type-fields this))))
    (string-append!
     result
     (format "~a\n" (inspect it))))
  (string-append!
   result
   (format "Mem size: ~a, load size: ~a, signed ~a, align ~a\n"
            (get-size-in-memory this)
            (get-load-size this)
            (get-load-signed this)
            (get-in-memory-alignment this)))
  result)

;; Diff for this type

(define (bitfield-type-diff this other)
  (unless (bitfield-type? other) (error (incompatible-diff 'bitfield-type other)))
  (define-with-struct l. (bitfield-type fields) this)
  (define-with-struct r. (bitfield-type fields) other)
  (define l.fields.size (gvector-count l.fields))
  (define r.fields.size (gvector-count r.fields))
  (define min-fields (min l.fields.size r.fields.size))

  (define result "")
  (string-append! result (type-diff this other))
  (string-append! result (value-type-diff this other))

  (when (!= l.fields r.fields)
    (when (!= l.fields.size r.fields.size)
      (string-append!
       result
       (format "Number of fields !a vs. !a\n" l.fields.size r.fields.size)))

    (for ((i (in-range min-fields)))
      (let ((lf (gvector-ref l.fields i))
            (rf (gvector-ref r.fields i)))
        (when (!= lf rf)
          (string-append!
           result
           (format "field !a (!a/!a):\n~a\n"
                   i (sbitfield-name lf) (sbitfield-name rf)
                   (sbitfield-diff lf rf)))))))
  result)

;; Find the method wih name

(define/contract (lookup-bitfield this name)
  (-> bitfield-type? symbol? sbitfield?)
  (for/gvector-find-item
      (lambda (x) (eq? (sbitfield-name x) name))))

;; TEST -----------------------------------------------------------------------
(module+ test
  (require rackunit)
  (check-equal? (inspect (bitfield-type-new 'foo 'bar 10 #t))
                "Parent type: foo\nFields:\nMem size: 10, load size: 10, signed #t, align 10\n"))
;; ----------------------------------------------------------------------------
;; Enum
;; ----------------------------------------------------------------------------


(define-struct (enum-type value-type)
  (
   is-bitfield      ; bool?
   entries          ; hash-table of enum-entry?
   )
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [
   (define (inspect this) (format "[EnumType] ~a" (type-name this)))
   ]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (enum-type-diff this other))
   ]

  )

;; @params entries - map of symbols to the integergs

(define/contract (enum-type-new parent name is-bitfield entries)
  (-> value-type? symbol? boolean? hash? enum-type?)
  (let ((base (value-type-new
               (type-name parent)
               name
               (type-is-boxed parent)
               (get-load-size parent)
               (get-load-signed parent))))
    (type-copy enum-type base is-bitfield entries)))

;; Find item

(define/contract (enum-type-find this name)
  (-> enum-type? symbol? (or/c #f integer?))
  (hash-ref (enum-type-entries this) name #f))

;; Diff for this type

(define (enum-type-diff this other)
  (unless (enum-type? other) (error (incompatible-diff 'enum-type other)))
  (define-with-struct l. (enum-type is-bitfield entries) this)
  (define-with-struct r. (enum-type is-bitfield entries) other)
  (define l.entries.size (hash-count l.entries))
  (define r.entries.size (hash-count r.entries))

  (define result "")
  (string-append! result (type-diff this other))
  (string-append! result (value-type-diff this other))

  (when (!= l.is-bitfield r.is-bitfield)
    (string-append! result (format "is_bitfield: ~a vs ~a\n" l.is-bitfield r.is-bitfield)))

  (when (!= l.entries r.entries)
    (string-append! result "Entries are different:\n")
    (when (!= l.entries.size r.entries.size)
      (string-append!
       result
       (format "Number of entries ~a vs ~a\n" l.entries.size r.entries.size)))

    (hash-for-each l.entries
                   (lambda (lk lv)
                     (let ((rv (hash-ref r.entries lk)))
                       (cond
                         ((not rv)
                          (string-append!
                           result
                           (format("  ~a is in one, but not the other.\n" lk))))
                         ((!= lv rv)
                          (string-append!
                           result
                           (format "  ~a is defined differently: ~a vs ~a\n"
                                                        lk lv rv))))))))
  result)

;; TEST -----------------------------------------------------------------------
(module+ test
  (require rackunit)
  (let* ((base (value-type-new 'foo 'bar #f 10 #f))
         (enum1 (enum-type-new base 'bar #f (make-hash '((foo . 1) (bar . 2)))))
         (enum2 (enum-type-new base 'bar #f (make-hash '((foo . 1) (bar . 2) (baz . 3))))))
    (check-equal? (inspect enum1) "[EnumType] bar")
    (check-equal? (diff enum1 enum2) "Entries are different:\nNumber of entries 2 vs 3\n")
    ))
