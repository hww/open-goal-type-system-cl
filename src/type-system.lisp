
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

(require racket/format)
(require racket/contract racket/string)
(require data/gvector)

(require "type.rkt" "basic-types.rkt" "type-spec.rkt" "vmc-lib.rkt" "interfaces.rkt")

(provide (all-defined-out))

;; ==============================================================================
;; Globals
;; ==============================================================================

(define-struct type-system
  (
   ;; The table of all known types -- contains the types
   types
   ;; The table of all forward declared types - contains only the symbols
   ;; aka type names
   forward-declared-types
   ;; The table of all forward declared types - contains quantiry of
   ;; methods for types
   forward-declared-method-counts
   ;; Keep the types after redefinition
   old-types
   ;; Allow the type redefinition
   allow-redefinition
   )
  #:mutable
  #:transparent)

(define/contract (type-system-new)
  (-> type-system?)
  (define this (type-system (make-hash) (make-hash) (make-hash) (make-gvector #:capacity 256) #t))
  (define-initial this)
  this)


;; ------------------------------------------------------------------------------
;; Helpers for a globals
;; ------------------------------------------------------------------------------

;; Find the type
(define/contract (types-find this name)
  (-> type-system? symbol? (or/c #f type?))
  (hash-ref (type-system-types this) name #f))


(define/contract (types-set! this name val)
  (-> type-system? symbol? type? void?)
  (hash-set! (type-system-types this) name val))

;; Find the type
(define/contract (forward-declared-types-find this name)
  (-> type-system? symbol? (or/c #f symbol?))
  (hash-ref (type-system-forward-declared-types this) name #f))

(define/contract (forward-declared-types-set! this name val)
  (-> type-system? symbol? symbol? void?)
  (hash-set! (type-system-forward-declared-types this) name val))

;; Find the type
(define/contract (forward-declared-method-counts-find this name)
  (-> type-system? symbol? (or/c #f symbol?))
  (hash-ref (type-system-forward-declared-method-counts this) name #f))

(define/contract (forward-declared-method-counts-set! this name val)
  (-> type-system? symbol? integer? void?)
  (hash-set! (type-system-forward-declared-method-counts this) name val))

;; Store old type

(define/contract (old-types-push this item)
  (-> type-system? any/c void?)
  (gvector-add! (type-system-old-types this) item))

;; Debugging function to print out all types, and their methods and fields.

(define/contract (inspect-all-type-information this)
  (-> type-system? string?)
  (string-append
   "\nALL TYPES ===================\n"
   (string-join (hash-map (type-system-types this) (lambda (k v) (inspect v))) "\n")
   "\nEND OF ALL TYPES ============\n"))

;; Clear types
(define/contract (types-clear this)
  (-> type-system? void)
  (hash-clear! (type-system-types this))
  (hash-clear! (type-system-forward-declared-types this))
  (hash-clear! (type-system-forward-declared-method-counts this))
  (set-type-system-old-types! this (make-gvector #:capacity 256))
  (define-initial this)
  )

(define/contract (define-initial this)
  (-> type-system? void?)
  ;; the "none" and "_type_" types are included by default.
  (add-type this 'none      (null-type-new 'none))
  (add-type this '_type_    (null-type-new '_type_))
  (add-type this '_varargs_ (null-type-new '_varargs_))
  ;; OBJECT
  (add-type this 'object    (value-type-new 'object 'object #f 4 #f))
  (void))

;; ==============================================================================

(define/contract (base-type o)
  (-> (or/c type? type-spec?) symbol?)
  (cond
    ((type-spec? o) (type-spec-base-type o))
    ((type? o) (type-base-type o))
    (else (assert false))))

;; ==============================================================================
;; How to define a type
;; ==============================================================================

;; Add a new type. If the type exists, and this new type is different,
;; it is an error if throw_on_redefine is set. The type should be
;; fully set up (fields, etc) before running this.

(define/contract (add-type this name type)
  (-> type-system? symbol? type? (or/c #f type?))
  (define method-kv (forward-declared-method-counts-find this name))
  (when method-kv
    (let ((method-count (get-next-method-id type)))
      (when (!= method-count method-kv)
        (error
         (format "Type ~a was defined with ~a methods but was forward declared with ~a\n"
                 name  method-count method-kv)))))
  (let ((kv (types-find this name)))
    (cond
      (kv
       ;; exists already
       (cond
         ((compare kv type)
          ;; exists and we are trying to change it!
          (when (type-system-allow-redefinition this)
            (log-warning "[TypeSystem] Type ~a was originally\n~a\nand is redefined as\n~a\n"
                 (type-name kv) (inspect kv) (inspect type))
            ;; extra dangerous we have allowed type redefinition!
            ;; keep the unique-ptr around just in case somebody references this old type pointer.
            (old-types-push this kv)
            ;; update the type
            (types-set! this name type)))
         (else (

                (error (format "Inconsistent type definition. Type ~a was originally\n~a\nand is redefined as\n~a\nDiff:\n~a\n"
                               (type-name kv) (inspect kv) (inspect type) (diff kv type)))))))
      (else
       ;; newly defined!
       ;; none/object get to skip these checks because they are roots.
       (when (and (!= name 'object)
                  (!= name 'none)
                  (!= name '-type-)
                  (!= name '-varargs-))
         (when (forward-declared-types-find this (type-get-parent type))
           (error (format "Cannot create new type `~a`. The parent type `~a` is not fully defined.\n"
                          (type-name type) (type-get-parent type)))

           (unless (types-find this (type-get-parent type))
             (error (format "Cannot create new type `~a`. The parent type `~a` is not defined.\n"
                            (type-name type) (type-get-parent type))))))

       (types-set! this name type)
       (define fwd-it (forward-declared-types-find this name))
       (when fwd-it
         ;; need to check parent is correct.
         (when (not (tc this (type-spec-new fwd-it) (type-spec-new name)))
           (error (format "Type ~a was original declared as a child of ~a but is not.\n" name fwd-it))))
       (hash-remove! (type-system-forward-declared-types this) name))))
  (define res (types-find this name))
  (if res
      (log-debug  "Defined         ~a\n" (inspect res))
      (log-warning "Was not defined ~a\n" name))
  res)

;; ==============================================================================
;; Forward declare
;; ==============================================================================

;; Inform the type system that there will eventually be a type named "name".
;; This will allow the type system to generate TypeSpecs for this type, but
;; not access detailed information, or know the exact size.

(define/contract (forward-declare-type-as-type this name)
  (-> type-system? symbol? void)
  (log-debug "forward-declare: ~a\n" name)
  (unless (types-find this name)
    ;; only if not defined
    (let ((it (forward-declared-types-find this name )))
      (if it
          (error (format "Tried to forward declare ~a as a type multiple times.  Previous: ~a Current: object" name it))
          (forward-declared-types-set! this name 'object)))))

;; Inform the type system that there will eventually be a type named "name"
;; and that it's a basic. This allows the type to be used in a few specific
;; places. For instance a basic can have a field where an element is the
;; same type.

(define/contract (forward-declare-type-as this new-type parent-type)
  (-> type-system? symbol? symbol? void)
  (log-debug "forward-declare: ~a as: ~a\n" new-type parent-type)
  (let ((type-it (types-find this new-type)))
    (if type-it
        ;; the type is already defined
        (begin
          (let ((parent-it (types-find this parent-type)))
            (when (not parent-it)
              (error
               "Got a forward declaration for known type ~a where the parent ~a is unknown"
               new-type parent-type)))
          (when (not (tc this (type-spec-new parent-type) (type-spec-new new-type)))
            (error
             "Got a forward definition that type ~a is a ~a which disagrees with existing fully-defined types."
             new-type parent-type))
          ;; ignore forward declaration
          )
        ;; the new type is not found
        (begin
          ;; find forward declaration
          (let ((fwd-it (forward-declared-types-find this new-type)))
            (cond
              ((not fwd-it)
               ;; define new type
               (forward-declared-types-set! this new-type parent-type))
              (else
               ;; there is a forward declaration then
               (when (!= fwd-it parent-type)
                   ;; new declaration is different from previous
                   (let ((old-parent-it (types-find this fwd-it))
                         (new-parent-it (types-find this parent-type))
                         (old-ts (type-spec-new fwd-it)))
                     (cond
                       ((and old-parent-it new-parent-it)
                        ;; old and new types are found
                        ;; check the posibility of casting
                        (let ((new-ts (type-spec-new (type-name new-parent-it))))
                          (cond
                            ((tc this old-ts new-ts)
                             ;; new is more specific or equal to old:
                             (forward-declared-types-set! this new-type (base-type new-ts)))
                            ((tc this new-ts old-ts)
                             ;; old is more specific or equal to new:
                             #f ;; dummy value
                             )
                            (else
                             (error
                              (format (string-append
                                       "Got a forward declaration that type ~a is a ~a, "
                                       "which disagrees with a previous forward declaration "
                                       "that it was a ~a (incompatible types)")
                                      new-type parent-type fwd-it))))))
                       (else
                        ;; not enough info to know if this is safe or not!.
                        (error
                         (format (string-append
                                  "Got a forward declaration that type ~a is a ~a, which disagrees with a previous "
                                  "forward declaration that it was a ~a (not enough information to know this is okay, "
                                  "forward declare more types to resolve this)")
                                 new-type parent-type fwd-it)))))))))))))

;; Forward declare method count

(define/contract (forward-declare-type-method-count this name num-methods)
  (-> type-system? symbol? integer? void)
  (define existing-fwd (forward-declared-method-counts-find name))

  (when (and existing-fwd (!= existing-fwd num-methods))
    (error (format
        "Type ~a was originally forward declared with ~a methods and is now being forward declared with ~a methods"
        name existing-fwd num-methods)))

  (define existing-type (types-find name))
  (when existing-type
    (let ((existing-count (get-next-method-id existing-type)))
      (when (!= existing-count num-methods)
        (error (format
                "Type () was defined with ~a methods and is now being forward declared with ~a methods"
                name existing-count num-methods)))))
  (forward-declared-method-counts-set! this name num-methods))

;; Forward declare text ------------------------------------------------

(module+ test
  (require rackunit)
  (let ((this (type-system-new)))
    (check-equal? (forward-declared-types-find this 'object) #f)
    (forward-declare-type-as-type this 'foo)
    (check-equal? (forward-declared-types-find this 'foo) 'object)))

;; ==============================================================================
;; Get method count
;; ==============================================================================

(define/contract (get-type-method-count name)
  (-> symbol? integer?)
  (define result (try-get-type-method-count(name)))
  (if result
      result;
      (error (format "Tried to find the number of methods on type ~a but it is not defined."
                     name))))

;; Get method count for fully declared or for partialy declared

(define/contract (try-get-type-method-count name)
  (-> symbol? (or/c #f integer?))
  (let ((type-it (types-find name)))
    (cond
      (type-it
       (get-next-method-id type-it))
      (else
       (forward-declared-method-counts-find name)))))

;; Get the runtime type (as a name string) of a TypeSpec.
;; Gets the runtime type of the primary type of the TypeSpec.

(define/contract (get-runtime-type ts)
 (-> type-spec? symbol?)
  (type-runtime-name (lookup-type ts)))

;; ==============================================================================
;; Deref
;; ==============================================================================
(define-struct deref-info
  (
   can-deref
   mem-deref
   sign-extend
   stride
   load-size
   result-type
   )
  #:transparent
  #:mutable)

(define/contract (deref-info-new-0)
  (-> deref-info?)
  (make-deref-info #f #f #f -1 -1 #f))

(define/contract (get-deref-info type)
  (-> type-spec? deref-info?)
  (error "TODO")
  (deref-info-new-0)
  )
;; ==============================================================================
;; Few predicates
;; ==============================================================================

(define/contract (fully-defined-type-exists this type-or-name)
  (-> type-system? (or/c symbol? type-spec?) any/c)
      (if (symbol? type-or-name)
          (types-find this type-or-name)
          (fully-defined-type-exists this (base-type type-or-name))))

(define/contract (partially-defined-type-exists this name)
  (-> type-system? symbol? (or/c symbol? #f))
  (forward-declared-types-find this name))

;; ==============================================================================
;; The type specification builder
;; ==============================================================================

;; Make the type specification for the type
;
(define/contract (make-typespec this name)
  (-> type-system? symbol? type-spec?)
  (if (or (types-find this name)
          (forward-declared-types-find this name))
        (type-spec-new name)
        (error (format "Type `~a` is unknown" name))))


(define/contract (make-array-typespec element-type)
  (-> type-spec? type-spec?)
  (type-spec-new 'array (element-type)))

;; Create a typespec for a function.  If the function doesn't return
;; anything use "none" as the return type.

(define/contract (make-function-typespec this arg-types return-type)
  (-> type-system? (listof symbol?) symbol? type-spec?)
  (define ts (make-typespec this 'function))
  (for ((it (in-list arg-types)))
    (type-spec-args-add ts (make-typespec this it)))
  (type-spec-args-add ts (make-typespec this return-type))
  ts)

;; Create a TypeSpec for a pointer to a type.

(define/contract (make-pointer-typespec this type-or-name)
  (-> type-system? (or/c symbol? type-spec?) type-spec?)
  (if (symbol? type-or-name)
      (type-spec-new this 'pointer (make-typespec this type-or-name))
      (type-spec-new this 'pointer type)))

;; Create a TypeSpec for an inline-array of type

(define/contract (make-inline-array-typespec this type)
  (-> type-system? (or/c symbol? type-spec?) type-spec?)
  (if (symbol? type)
      (type-spec-new this 'inline-array (make-typespec this type))
      (type-spec-new this 'inline-array type)))

;; Type spec text--------------------------------------------------------

(module+ test
  (require rackunit)
  (let ((this (type-system-new)))
    (forward-declare-type-as-type this 'foo)
    (forward-declare-type-as-type this 'bar)
    (let ((obj (make-typespec this 'object))
          (foo (make-typespec this 'foo))
          (bar (make-typespec this 'bar)))
      (check-equal? obj (type-spec 'object '() '()))
      (check-equal? foo (type-spec 'foo '() '()))
      (check-true (tc this obj foo))
      (check-true (tc this obj foo))
      (check-equal? (lowest-common-ancestor this foo bar) (type-spec 'object '() '()))
      (check-equal? (get-path-up-tree this 'foo) '(foo object))
      (check-equal? (get-path-up-tree this 'bar) '(bar object))
      )))

;; ==============================================================================
;; Lookup types
;; ==============================================================================

;; Get full type information. Throws if the type doesn't exist. If the given
;; type is redefined after a call to lookup_type, the Type* will still be valid,
;; but will point to the old data. Whenever possible, don't store a Type* and
;; store a TypeSpec instead. The TypeSpec can then be used with lookup_type to
;; find the most up-to-date type information.

(define/contract (lookup-type-by-name this name)
  (-> type-system? symbol? type?)
  (let ((type (types-find this name)))
    (cond  (type type)
           ((forward-declared-types-find this name)
            (error (format "Type ~a is not fully defined." name)))
           (else
            (error (format "Type ~a is not defined." name))))))

;; Get full type information. Throws if the type doesn't exist. If the given
;; type is redefined after a call to lookup_type, the Type* will still be valid,
;; but will point to the old data. Whenever possible, don't store a Type* and
;; store a TypeSpec instead. The TypeSpec can then be used with lookup_type to
;; find the most up-to-date type information.

(define/contract (lookup-type this ts-or-name)
  (-> type-system? (or/c type-spec? symbol?) type?)
  (if (symbol? ts-or-name)
      (lookup-type-by-name this ts-or-name)
      (lookup-type this (base-type ts-or-name))))

;; Get type info. If the type is not fully defined (ie, we are parsing its
;; deftype now) and its forward defined as a basic or structure, just get
;; basic/structure.

(define/contract (lookup-type-allow-partial-def this ts-or-name)
  (-> type-system? (or/c symbol? type-spec?) type?)
  (if (symbol? ts-or-name)
      (lookup-type-allow-partial-def-by-name this ts-or-name)
      (lookup-type-allow-partial-def this (base-type ts-or-name))))

;; Get type info. If the type is not fully defined (ie, we are parsing its
;; deftype now) and its forward defined as a basic or structure, just get
;; basic/structure.

(define/contract (lookup-type-allow-partial-def-by-name this name)
  (-> type-system? symbol? type?)
  (let ((type (types-find this name)))
    ;; the type is found
    (cond
      (type type)
      ;; the type was not found
      (else
       ;; recursively find with forward declaration
       (let :loop: ((cur-name name))
         ;; find a forward declaration for cur-name
         (let ((frw-type (forward-declared-types-find this cur-name)))
           (cond
             ;; there is no forward declaration with cur-name
             ((not frw-type)
              (if (== name cur-name)
                  (error (format "The type ~a is unknown." name))
                  (error (format "When looking up forward defined type ~a, could not find a type ~a."
                         name cur-name))))
             ;; found forward declaration. try lookup the type
             (else
              ;; find the type of fwd declaration
              (let ((type (types-find this frw-type)))
                (if type
                    ;; the type is found
                    type
                    ;; the type is not found, try deduct fw-decl
                    ;; of cur-name
                    (if (== frw-type cur-name)
                        (error (format "The type ~a reffer to byself by forward declaration but there is no type structure for it"
                                       frw-type))
                        (:loop: frw-type))))))))))))

;; Get a type by name and cast to a child class of Type*. Must succeed.

(define (get-type-of-type this type-predicate type-name)
  (-> type-system? procedure? symbol? any/c)
  (let ((type (types-find this type-name)))
    ;; the type is found
    (cond
      ((and type (type-predicate type)) type)
      (else
       (error (format "Failed to get ~a as the right type ~a, found ~a"
                      type-name type-predicate type))))))

;; ==============================================================================
;; Gen size of the type
;; ==============================================================================

;; Get load size for a type. Will succeed if one of the two conditions is true:
;;
;; - Is a fully defined type.
;; - Is partially defined, but structure is in the parent.
;;
;; This should be safe to use to load a value from a field.

(define/contract (get-load-size-allow-partial-def this ts)
  (-> type-system? type-spec? integer?)
  (define fully-defined-it (types-find (base-type ts)))
  (cond
    (fully-defined-it
     (get-load-size fully-defined-it))
    (else

     (let ((partial-def (lookup-type-allow-partial-def this ts)))
       (when (not (tc
                   (type-spec-new 'structure)
                   ts))
         (error
          (format
           "Cannot perform a load or store from partially defined type ~a"
             (inspect ts))))
       (assert (== (get-load-size partial-def) 4))
       (get-load-size partial-def)))))

;; ==============================================================================
;; Methods
;; ==============================================================================

;; Uses other method

(define/contract (declare-method this
                                 type-or-name
                                 method-name
                                 no-virtual
                                 ts
                                 override-type)
  (-> type-system? (or/c symbol? type?) symbol? boolean? type-spec? boolean? method-info?)

  (if (symbol? type-or-name)
      (declare-method-for-type this (lookup-type this (make-typespec this type-or-name))
                               method-name no-virtual ts override-type)
      (declare-method-for-type this type-or-name  method-name no-virtual ts override-type)))

;; Add a method, if it doesn't exist. If the method already exists (possibly in
;; a parent), checks to see if this is an identical definition. If not, it's an
;; error, and if so, nothing happens. Returns the info of either the existing or
;; newly created method.
;;
;; This is not used to override methods, but instead to create truly new
;; methods. The one exception is overriding the "new" method - the TypeSystem
;; will track that because overridden new methods may have different arguments.

(define/contract (declare-method-for-type this
                                          type
                                          method-name
                                          no-virtual
                                          ts
                                          override-type
                                          (id -1))
  (->* (type-system? type? symbol? boolean? type-spec? boolean?) (integer?) method-info?)
  ;; (printf "declare-method type: ~a name: ~a no-virt: ~a type-spec: ~a override: ~a :id ~a\n"
  ;;        (inspect type)
  ;;        method-name
  ;;        no-virtual
  ;;        (inspect ts)
  ;;        override-type
  ;;        id)

  (cond
    ;; new methods
    ((== method-name 'new)
     (when override-type
       (error "Cannot use :replace option with a new method."))
     (add-new-method this type ts))
    ;; other methods
    (else
     ;; look up the method
     (define existing-info (try-lookup-method this type method-name))

     (cond
       (override-type
        ;; Allow overriding
        (when (not existing-info)
          (set! existing-info (try-lookup-method-by-id this (type-get-parent type) id))
          (unless (and (!= id -1) existing-info)
            (error
             (format
              "Cannot use :replace on method ~a of ~a because this method was not previously declared in a parent."
              method-name (type-name type)))))

        ;; use the existing ID.
        (type-add-method type (method-info-new (method-info-id existing-info)
                                               method-name
                                               ts
                                               (type-name type)
                                               no-virtual
                                               #t)))
       ;; Do not allow overriding
       (else
        (cond
          (existing-info
           ;; make sure we aren't changing anything.
           (let* ((exist-type (method-info-type existing-info))
                  (compatible (type-spec-is-compatible-child-method exist-type ts (type-name type))))
             (when (not compatible)
               (error (format
                       (string-append
                        "The method ~a of type ~a was originally declared as ~a, but has been "
                        "redeclared as ~a. Originally declared in ~a\n")
                       method-name
                       (type-name type)
                       (inspect exist-type)
                       (inspect ts)
                       (method-info-defined-in-type existing-info))))

             (when (and
                    (or
                     (method-info-no-virtual existing-info)
                     no-virtual)
                    (!= method-info-defined-in-type existing-info)
                    (type-name type))
               (error (format
                       "Cannot define method ~a in type ~a when it was defined as no-virtual in parent type "
                       method-name (type-name type) (method-info-defined-in-type existing-info))))
             (when (!= no-virtual (method-info-no-virtual existing-info))
               (error (format
                       "The method ~a of type ~a was originally declared with no-virtual = ~a, but has been redeclared as ~a"
                       method-name (type-name type) (method-info-no-virtual existing-info) no-virtual)))
             existing-info))
          (else
           ;; add a new method!
           (when (< id 0)
             (set! id (get-next-method-id this type)))
           (type-add-method
            type
            (method-info-new id
                             method-name
                             ts
                             (type-name type)
                             no-virtual
                             false)))))))))

;; Define method for type. The type can be as the name

(define/contract (define-method type-or-name method-name ts)
  (-> (or/c type? symbol?) symbol? type-spec? method-info?)

   (if (symbol? type-or-name)
       (define-method-for-type (lookup-type (type-spec-new type-name) method-name ts))
       (lookup-type type-or-name method-name ts)))

;; Add a method, if it doesn't exist. If the method already exists (possibly in
;; a parent), checks to see if this is an identical definition. If not, it's an
;; error, and if so, nothing happens. Returns the info of either the existing or
;; newly created method.
;;
;; This is not used to override methods, but instead to create truly new
;; methods. The one exception is overriding the "new" method - the TypeSystem
;; will track that because overridden new methods may have different arguments.

(define/contract (define-method-for-type this type method-name ts)
  (-> type-system? type? symbol? type-spec? method-info?)
  (cond
    ((== method-name 'new)
     (add-new-method type ts))
    (else
     ;; look up the method
     (let ((existing-info (try-lookup-method type method-name)))
       (cond
         (existing-info
          ;; make sure we aren't changing anything.
          (let ((mtype (method-info-type existing-info)))
            (when (not (type-spec-is-compatible-child-method mtype ts (type-name type)))
              (error (format
                      "The method ~a of type ~a was originally defined as ~a but has been redefined as ~a\n"
                      method-name (type-name type)
                      (inspect mtype)
                      (inspect ts)))))
          existing-info)
         (else
          (error (format "Cannot add method ~a to type ~a because it was not declared.\n"
                         method-name (type-name type)))))))))

;; Special case to add a new method, as new methods can specialize the
;; arguments. If it turns out that other child methods can specialize arguments
;; (seems like a bad idea), this may be generalized.

(define/contract (add-new-method this type ts)
  (-> type-system? type? type-spec? method-info?)
  (define existing (type-get-my-new-method type))
  (cond
    (existing
      ;; it exists!
      (let ((mtype (method-info-type existing)))
        (when (not (type-spec-is-compatible-child-method mtype ts (type-name type)))
          (error (format
                  (string-append
                  "Cannot add new method. Type does not match declaration. The new method of ~a was "
                  "originally defined as ~a, but has been redefined as ~a\n")
                  (type-name type)  (inspect existing) (inspect ts))))
        existing))
    (else
     (type-add-new-method type (method-info-new 0 'new ts (type-name type))))))

;; ==============================================================================
;; Methid lookups
;; ==============================================================================

;; Lookup information on a method. Error if it can't be found. Will check parent
;; types if the given type doesn't specialize the method.

(define/contract (lookup-method type-name method-name)
  (-> symbol? symbol? method-info?)
  (cond
    ((== method-name 'new)
     (lookup-new-method type-name))
    (else

     ; first lookup the type
     (let :loop: ((iter-type (lookup-type type-name)))
       (let ((info (type-get-my-method method-name)))
         (cond
           (info info)
           (else
            (if (type-has-parent? iter-type)
                (:loop: (lookup-type (type-get-parent iter-type)))
                (error (format "The method ~a of type ~a could not be found.\n"
                               method-name type-name))))))))))

;; Try lookup method by the type and method name will not make an exeption

(define/contract (try-lookup-method this type-or-name method-name-or-id)
  (-> type-system? (or/c type? symbol?) (or/c symbol? integer?) (or/c #f method-info?))
  (cond
    ((and (symbol? type-or-name) (symbol? method-name-or-id))
     (try-lookup-method-by-name this type-or-name method-name-or-id))
    ((and (symbol? type-or-name) (integer? method-name-or-id))
     (try-lookup-method-by-id this type-or-name method-name-or-id))
    ((and (type? type-or-name) (symbol? method-name-or-id))
     (try-lookup-method-of-type this  type-or-name method-name-or-id))
    (else (error (format "Bad arguments\n [0] ~a\n [1] ~a\n" type-or-name method-name-or-id)))))

;; Lookup by name of type and method

(define/contract (try-lookup-method-by-name this type-name method-name)
  (-> type-system? symbol? symbol? (or/c #f method-info?))
  (define type (types-find this type-name))
  (cond
    (type
     (try-lookup-method this type method-name))
    (else
     ;; try to look up a forward declared type.
     (let ((fwd-dec-type (lookup-type-allow-partial-def this type-name)))
       (if (tc (type-spec-new 'basic)
               (type-spec-new fwd-dec-type))
           ;; only allow this for basics. It technically should be safe for structures as well.
           (try-lookup-method this fwd-dec-type method-name)
           #f)))))

;; Like lookup-method, but won't throw or print an error
;; when things go wrong.

(define/contract (try-lookup-method-by-id this type-name method-id)
  (-> type-system? symbol? integer? (or/c #f method-info?))
  ;;
  (define (type-get-any-method-by-id type method-id)
    (cond
      ((== method-id GOAL-NEW-METHOD)
       (type-get-my-new-method type))
      (else
       (type-get-my-method-by-id type method-id))))
  ;; look up the method
  (let :loop: ((iter-type (types-find this type-name)))
    (cond
      ((not iter-type) #f)
      (else
       (define m (type-get-any-method-by-id iter-type method-id))
       (cond
         ;; Case we found it
         (m m)
         ;; Did not found
         (else
          (if (type-has-parent? iter-type)
              (:loop: (lookup-type this (type-get-parent iter-type)))
              #f)))))))

;;

(define/contract (try-lookup-method-of-type this type method-name)
  (-> type-system? type? symbol? (or/c #f method-info?))
  (define (type-get-any-method type name)
    (cond
      ((== name 'new)
       (type-get-my-new-method type))
      (else
       (type-get-my-method type method-name))))
  ;; look up the method
  (let :loop: ((iter-type type))
    (cond
      ((not iter-type) #f)
      (else
       (define m (type-get-any-method iter-type method-name))
       (cond
         ;; Case we found it
         (m m)
         ;; Did not found
         (else
          (if (type-has-parent? iter-type)
              (:loop: (lookup-type this (type-get-parent iter-type)))
              #f)))))))

;; Lookup information on a method by ID number. Error if it can't be found. Will
;; check parent types if the given type doesn't specialize the method.

(define/contract (lookup-method-by-method-id this type-name method-id)
(-> type-system? symbol? integer? method-info?)
  (cond
    ((== method-id GOAL-NEW-METHOD)
     (lookup-new-method this type-name))
    (else
  ;; first lookup the type
     (let :loop: ((type-iter (lookup-type type-name)))
       (let (( m (type-get-my-method type-iter)))
         ;; look up the method
         (cond
           (m m)
           (else
            (if (type-has-parent? type-iter)
                (:loop: (lookup-type this (type-get-parent type-iter)))
                (error "Method with id ~a of type ~a could not be found."
                       method-id type-name)))))))))

;; Lookup information on a new method and get the most specialized version.

(define/contract (lookup-new-method this type-name)
  (-> type-system? symbol? method-info?)
  ;; first lookup the type
  (let :loop: ((iter-type (lookup-type this type-name)))
    ;; look up the method
    (let ((m (type-get-my-new-method iter-type)))
      (cond
        (m m)
        (else
         (if (type-has-parent? iter-type)
             (:loop: (lookup-type this (type-get-parent iter-type)))
             (error "The new method of type ~a could not be found.\n"
                    type-name)))))))

;; Makes sure a method exists at the given ID for the given type possibly
;; defined in a parent.

(define/contract (assert-method-id this type-name method-name id)
  (-> type-system? symbol? symbol? integer? void?)
  (define info (lookup-method this type-name method-name))
  (unless (== (method-info-id info) id)
      (error (format "Method ID assertion failed: type ~a, method ~a id was ~a, expected ~a\n"
                     type-name method-name (method-info-id info) id))))

;; Get the next free method ID of a type.

(define/contract (get-next-method-id this type)
  (-> type-system? type? integer?)

  ;; Original version form Open GOAL project
  ;; Does not checking parent types if the method define us defined in child.
  ;; It can be problem in some cases. Look at the example:
  ;;
  ;; (deftype foo (struct) (x y z))
  ;; (deftype bar (foo) (x w))
  ;;
  ;; Defining method `w` for `bar` will not find the method `w`
  ;; in `foo` type. So it will try to place `w` after last method
  ;; this function will find `x` as the last method and place
  ;; `w` afte -- it is overlap with `y`
#|
  (let :loop: ((it type))
    (let ((info (type-get-my-last-method it)))
      (cond
        (info ;; condition!
         (+ 1 (method-info-id info)))
        (else
         (if (type-has-parent? it)
             (:loop: (types-find this (type-get-parent it)))
             ;; nobody has defined any method yet. New is special and doesn't use this, so we return
             ;; one after new.
             1))))))
|#
  ;; Alternative version will find the last ID for all parent types
  ;; and choose max value. In the example above it will be slot
  ;; after `z`

  (define (find-method-ids type)
    (let ((info (type-get-my-last-method type)))
      (cons (if info (+ 1 (method-info-id info)) 1)
         (if (type-has-parent? type)
             (find-method-ids (types-find this (type-get-parent type)))
             null))))
  (apply max (find-method-ids type)))

;; ==============================================================================
;; Field lookup
;; ==============================================================================

;; Field TypeSpec Boool Int

(define-struct field-lookup-info (field type needs-deref array-size) #:transparent #:mutable)

;; Default constructor

(define/contract (field-lookup-info-new)
  (-> field-lookup-info?)
  (make-field-lookup-info #f #f #f -1))

;; Lookup detailed information about a field of a type by name, including type,
;; offset, and how to access it.

(define/contract (lookup-field-info type-name field-name)
(-> symbol? symbol? field-lookup-info?)
  (define info (field-lookup-info-new))
  (define field (lookup-field type-name field-name))
  (set-field-lookup-info-field! info field)

  ;; get array size, for bounds checking (when possible)
  (when (and (field-is-array field) (not (field-is-dynamic field)))
    (set-field-lookup-info-array-size! (field-array-size field)))

  (define base-type (lookup-type-allow-partial-def (field-type field)))
  (cond ;; is-ref
    ((is-reference? base-type)
     (cond ;; is-inline
       ((field-is-inline info)
        (cond ;; is-array
          ((field-is-array field)
           ;; inline array of reference types
           (set-field-lookup-info-needs-deref! info false)
           (set-field-lookup-info-type! info (make-inline-array-typespec (field-type type))))
          (else ;; is-array
           ;; inline object
           (set-field-lookup-info-needs-deref! info false)
           (set-field-lookup-info-type! info (field-type field)))))
       (else ;; !is-inline
        (cond
          ((field-is-array field)
           (set-field-lookup-info-needs-deref! info false)
           (set-field-lookup-info-type! (make-pointer-typespec (field-type type))))
          (else
           (set-field-lookup-info-needs-deref! info true)
           (set-field-lookup-info-type! info (field-type type)))))))
    (else ;; !is-ref
     (cond
       ((field-is-array? field)
        (set-field-lookup-info-needs-deref! info false)
        (set-field-lookup-info-type! info (make-pointer-typespec (field-type field))))
       (else
        ;; not array
        (set-field-lookup-info-needs-deref! info true)
        (set-field-lookup-info-type! info (field-type type))))))
  info)

;; Make sure a field is located at the specified offset.

(define/contract (:assert-field-offset type-name field-name offset)
  (-> symbol? symbol? integer? void?)
  (define field (lookup-field type-name field-name))
  (unless (== (field-offset field) offset)
    (error (format "assert-field-offset(~a, ~a, ~a) failed - got ~a\n"
                   type-name field-name offset))))


; Add a field to a type. If offset_override is -1
;; (the default), will place it automatically.

(define/contract (add-field-to-type
                  this
                  type
                  fld-name
                  fld-type
                  (is-inline #f)
                  (is-dynamic #f)
                  (array-size -1)
                  (offset-override -1)
                  (skip-in-static-decomp #f)
                  (score 0))
  (->* (type-system? struct-type? symbol? type-spec?)
       (boolean? boolean? integer? integer? boolean? real?)
       integer?)
  ;(printf "Add field ~a to type ~a\n" fld-name (type-name type))
  (when (struct-type-lookup-field type fld-name)
    (error (format "Type `~a` already has a field named `~a`\n" (type-name type) fld-name)))

  ;; first, construct the field
  (define field (field-new fld-name fld-type 0))
  (when is-inline
    (field-set-inline field))
  (when is-dynamic
     (field-set-dynamic field))
  (when (!= array-size -1)
    (field-set-array field array-size))
  ;;
  (define offset offset-override)
  (define field-alignment (get-alignment-in-type this field))
  ;; Compute offset
  (cond
    ((== offset -1)
     ;; we need to compute the offset ourself from the curent struct size
     (set! offset (align (struct-type-get-size-in-memory type) field-alignment)))
    (else
     ;; there was defined offset in the memory but mark this field as user-places
     (field-mark-as-user-placed field)
     ;; verify the offset if it was alligned
     (define aligned-offset (align offset field-alignment))
     (when (!= offset aligned-offset)
       (error (format "Tried to place field `~a` at `~a`, but it is not aligned correctly\n"
                      fld-name offset)))))
  ;;
  (set-field-offset! field offset)
  (set-field-alignment! field field-alignment)
  (when skip-in-static-decomp
    (set-field-skip-in-static-decomp! field #t))

  (set-field-field-score! field score)
  (define field-size (get-size-in-type this field))
  (define after-field (+ offset field-size))
  (when (< (struct-type-get-size-in-memory type) after-field)
    (struct-type-override-size-in-memory type after-field))
  (struct-type-add-field type field (struct-type-get-size-in-memory type))
  offset)

;; ==============================================================================
;; Lookup field
;; ==============================================================================

;; Lookup a field of a type by name

(define/contract (lookup-field type-name field-name)

  (-> symbol? symbol? field?)
  ;; next method will make exception in bad case
  (define atype (get-type-of-type struct-type? type-name))
  (define field (lookup-field atype field-name))
  (unless field
    (error (format "Type ~a has no field named ~a\n"
                   type-name field-name)))
  field)


;; ==============================================================================
;; Field alignement
;; ==============================================================================

;; Get the minimum required aligment of a field.

(define/contract (get-alignment-in-type this field)
  (-> type-system? field? integer?)

  (define fld-type (lookup-type-allow-partial-def this (field-type field)))

  (cond
    ((field-is-inline field)
     (if (field-is-array field)
         ;; THEN
         ;; TODO - is this actually correct? or do we use in-memory for the first element and
         ;; inline-array for the ones that follow?
         (get-inl-array-start-align fld-type)
         ;; ELSE
         ;; it is an inlined field, so return the alignment in memory
         ;; TODO - for inline, but not inline array, do we use structure alignment always?
         (get-inl-array-start-align fld-type)))
    (else

     (if (not (is-reference? fld-type))
         ;; it is a value type, so it's stored in full:
         (get-in-memory-alignment fld-type)
         ;; otherwise it's a reference
         POINTER-SIZE))))

(define/contract (allow-inline type)
  (-> type? boolean?)
  (define name (type-name type))
  (and (!= name 'basic) (!= name 'structure)))

;; Get the size of a field in a type. The array sizes should be consistent with
;; get-deref-info's stride.

(define/contract (get-size-in-type this field)
  (-> type-system? field? integer?)
  (define fld-type-spec (field-type field))
  (define fld-type (lookup-type-allow-partial-def this fld-type-spec))

  ;; Helper: Get size of field in case if it is array
  (define (get-size-for-array)
    (cond
    ((field-is-inline field)
      (when (not (fully-defined-type-exists this fld-type-spec))
        (error (format "Cannot use the forward-declared type ~a in an inline array.\n"
                               (inspect fld-type))))
      (when (not (allow-inline fld-type))
        (error (format
            "Attempted to use `~a` inline, this probably isn't what you wanted.\n"
            fld-type-spec)))
      (assert (is-reference? fld-type))
      (* (field-array-size field)
         (align (get-size-in-memory fld-type)
                (get-inl-array-stride-align fld-type))))
    (else
      (if (is-reference? fld-type)
          (* (field-array-size field) POINTER-SIZE)
          (* (field-array-size field)
             (align (get-size-in-memory fld-type)
                    (get-in-memory-alignment fld-type)))))))
  ;; Helper: Get size for not arrays
  (define (get-size-for-not-array)
    ;; not an array
    (cond
      ((field-is-inline field)
       (when (not (fully-defined-type-exists this fld-type-spec))
         (error (format "Cannot use the forward-declared type ~a inline.\n"
                        (inspect fld-type))))
       (when (not (allow-inline fld-type))
         (error (format
                 "Attempted to use `~a` inline, this probably isn't what you wanted. Type may not be defined fully.\n"
                 (type-name fld-type))))
       (assert (is-reference? fld-type))
       ;; return align(field-type->get-size-in-memory(), field-type->get-in-memory-alignment());
       ;; looking at dead-pool-heap we tightly pack in this case
       (get-size-in-memory fld-type))
      (else
       (if (is-reference? fld-type)
           POINTER-SIZE
           (align (get-size-in-memory fld-type)
                  (get-in-memory-alignment fld-type))))))
  ;;
  (cond
    ((field-is-dynamic field)
     0) ;; for dynamic fields zero
    ((field-is-array field)
     (get-size-for-array))
    (else
     (get-size-for-not-array))))

;; ==============================================================================
;; Type checking
;; ==============================================================================

;; Main compile-time type check!

(define/contract (tc this expected actual)
  (-> type-system? type-spec? type-spec? boolean?)
  (typecheck-and-throw this expected actual "" #f #f #f))


;; Is actual of type expected? For base types.

(define/contract (typecheck-base-types this expected actual allow_alias)
  (-> type-system? symbol? symbol? boolean? boolean?)
  ;;
  ;; The expected type should be found in the actual type tree
  ;;
  (define/contract (find-same-type this expected actual)
    (-> type-system? symbol? symbol? boolean?)
    (if (== expected actual)
        ;; types already same
        #t
        ;; find actual type and name
        (let* ((actual-type (lookup-type-allow-partial-def this actual))
               (actual-name (type-name actual-type)))
          (if (== expected actual-name)
              ;; expected and actual types are identical
              #t
              ;; repeat this function for parent of actual type
              (if (type-has-parent? actual-type)
                  (find-same-type this expected (type-parent actual-type))
                  #f)))))
  ;;
  (begin
    ;; the unit types aren't picky.
    (cond
      ((== expected 'meters)
       (set! expected 'float))
      ((== expected 'seconds)
       (set! expected 'time-frame))
      ((== expected 'degrees)
       (set!  expected 'float)))
    (when (== actual 'seconds)
      (set! actual 'time-frame))

    ;; the decompiler prefers no aliasing so it can detect casts properly
    (when allow_alias
      (when (== expected 'time-frame)
        (set! expected 'int))
      (when (== actual 'time-frame)
        (set! actual 'int)))

    ;; just to make sure it exists. will produce an errir if will not found
    (lookup-type-allow-partial-def this expected);
    ;; There are differnet expected and actual names
    (find-same-type this expected actual)))

;; Main compile-time type check!
;; @param expected - the expected type
;; @param actual - the actual type (can be more specific)
;; @param error_source_name - optional, can provide a name for where the error comes from
;; @param print_on_error - print a message explaining the type error, if there is one
;; @param throw_on_error - throw a std::runtime_error on failure if set.
;; @return if the type check passes

(define/contract (typecheck-and-throw this
                                      expect
                                      actual
                                      error-source-name
                                      print-on-error
                                      throw-on-error
                                      allow-type-alias)
  (-> type-system? type-spec? type-spec? string? boolean? boolean? boolean? void)

  ;; first, typecheck the base types:
  (and (typecheck-base-types this
                             (base-type expect)
                             (base-type actual)
                             allow-type-alias)
       ;; check the arguments
       (let ((exp-args-count (type-spec-args-count expect))
             (act-args-count (type-spec-args-count actual)))
         ;; next argument checks:
         (if (== exp-args-count act-args-count)
             (let :loop: ((i 0))
               (if (>= i exp-args-count)
                   #t
                   ;; don't print/throw because the error would be confusing. Better to fail only the
                   ;; outer most check and print a single error message.
                   (if (tc (type-spec-args-ref expect i) (type-spec-args-ref actual i))
                       (:loop: (+ 1 i))
                       #f)))
             ;; different sizes of arguments.
             (if (== exp-args-count 0)
                      ;; we expect zero arguments, but got some. The actual type is more specific, so this is fine.
                      #t
                      ;; different sizes, and we expected arguments. No good!
                      #f)))))

;; ==============================================================================
;; Enum
;; ==============================================================================

;; Find enum

(define/contract (try-enum-lookup this type-or-name)
  (-> type-system? (or/c type-spec? symbol?) (or/c #f enum-type?))
  (when (type-spec? type-or-name)
    (set! type-or-name (base-type type-or-name)))
  (let ((type (types-find this type-or-name)))
    ;; the type is found
    (cond
      ((and type (enum-type? type)) type)
      (else #f))))

;; ==============================================================================
;; The path of a type
;; ==============================================================================

;; Get a path from type to object.

(define/contract (get-path-up-tree this name)
  (-> type-system? symbol? (listof symbol?))
  (if (== name 'object)
      (list 'object)
      (cons name
            (let :loop: ((type (lookup-type-allow-partial-def this name)))
              (if (type-has-parent? type)
                  (cons (type-name type) (:loop: (lookup-type-allow-partial-def this (type-name type))))
                  (cons (type-name type) null))))))

;; Type tree test ------------------------------------------------------

(module+ test

  (require rackunit)
  (let ((this (type-system-new)))
    (forward-declare-type-as-type this 'foo)
    (check-equal? (get-path-up-tree this 'object) '(object))
    (check-equal? (get-path-up-tree this 'foo) '(foo object))))

;; ==============================================================================
;; Common accessor
;; ==============================================================================

;; Lowest common ancestor of two base types.

(define/contract (lca-base this typea typeb)
  (-> type-system? symbol? symbol? (or/c #f symbol?))
  (cond
    ((== typea typeb)
     typea)
    ((and (== 'none typea) (== 'none typeb))
     'none)
    (else
     ;; the list of classes will be
     (let ((a-up (get-path-up-tree this typea))
           (b-up (get-path-up-tree this typeb)))
       (let :loop: ((ai (1- (length a-up)))
                    (bi (1- (length b-up))))
         (if (and (>= ai 0) (>= bi 0))
             (let ((a (list-ref a-up ai))
                   (b (list-ref b-up bi)))
               (if (equal? a b)
                   a
                   (:loop: (1- ai) (1- bi))))
             #f))))))


;; Lowest common ancestor of two typespecs. Will recursively apply to arguments,
;; if compatible.
;;
;; Otherwise arguments are stripped off.
;; In a situation like lca("(a b)", "(c d)"), the result will be
;; (lca(a, b) lca(b, d)).

(define/contract (lowest-common-ancestor this a b)
  (-> type-system? type-spec? type-spec? type-spec?)
  (define result (type-spec-new (lca-base this (base-type a) (base-type b))))
  (cond
    ((and (== result (type-spec-new 'function))
          (== (type-spec-args-count a) 2)
          (== (type-spec-args-count b) 2)
          (or (== (type-spec-args-ref a 0) (type-spec-new '-varargs-))
              (== (type-spec-args-ref b 0) (type-spec-new '-varargs-))))
     (type-spec-new 'function))
    (else
     (when (and (not (type-spec-empty? a))
                (not (type-spec-empty? b))
                (== (type-spec-args-count a) (type-spec-args-count b)))
       ;; recursively add arguments
       (for ((i (in-range (type-spec-args-count a))))
         (type-spec-args-add result (lowest-common-ancestor (type-spec-args-ref a i) (type-spec-args-ref b i)))))))
  result)

;; Lowest common ancestor of multiple (or at least one) type.

(define/contract (lowest-common-ancestor-in-vector this types)
  (-> type-system? (vectorof type-spec?) type-spec?)
  (assert (!= 0 (vector-length types)))
  (cond
    ((== 1 (vector-length types))
     (vector-ref types 0))
    (else
     (let ((result (lowest-common-ancestor this (vector-at-0 types)
                                           (vector-at-1 types))))
       (for ((i (in-range 2 (vector-length types))))
         (set! result (lowest-common-ancestor this result (vector-ref types i))))
       result))))

;;
(define/contract (lowest-common-ancestor-reg a b)
  (-> type-spec? type-spec? type-spec?)
  (coerce-to-reg-type (lowest-common-ancestor a b)))

;; Converts a type in memory to the type you'll get in a register after loading it.

(define/contract (coerce-to-reg-type in)
  (-> type-spec? type-spec?)
  (define bt (base-type in))
  (when (== (type-spec-args-count in) 0)

    (when (or ((== bt 'int8) (== bt 'int16) (== bt 'int32) (== bt 'int64) (== bt 'integer)))
      (type-spec-new 'int))

    (when (or (== bt 'uint8) (== bt 'uint16) (== bt 'uint32) (== bt 'uint64) (== bt 'uinteger))
      (type-spec-new 'uint)))
  in)

;; ==============================================================================
;; Bitfield
;; ==============================================================================

;; Field TypeSpec Boool Int

(define-struct bitfield-lookup-info (result-type offset size sign-extend) #:transparent #:mutable)

;; Default constructor

(define/contract (bitfield-lookup-info-new)
  (-> field-lookup-info?)
  (make-bitfield-lookup-info #f -1 -1 #f))

;; Is the given type a bitfield type?

(define/contract (is-bitfield-type this type-name)
(-> type-system? symbol? boolean?)
  (get-type-of-type bitfield-type? type-name))

;; Get information about a field within a bitfield type.

(define/contract (lookup-bitfield-info this type-name field-name)
  (-> type-system? symbol? symbol? bitfield-lookup-info?)
  (define type (get-type-of-type this bitfield-type? type-name))

  (define f (struct-type-lookup-field type field-name))
  (when (not f)
    (error (format "Type ~a has no bitfield named ~a\n" type-name field-name)))

  (make-bitfield-lookup-info
   (sbitfield-type f) ; resutl type
   (sbitfield-offset f)
   (get-load-signed (lookup-type this (field-type f)))
   (sbitfield-size f)))

;; Add a new field to a bitfield type. Set the field size to -1 if you want to
;; just use the size of the type and not clip it.

(define/contract (add-field-to-bitfield this type field-name field-type offset field-size skip-in-decomp)
  (-> type-system? bitfield-type? symbol? type-spec? integer? integer? boolean? void)
  ;; in bits
  (define load-size (* 8 (get-load-size (lookup-type this field-type))))

  (when (== field-size -1)
    (set! field-size load-size))

  (when (> field-size load-size)
    (error (format
        "Type ~a's bitfield ~a's set size is ~a which is larger than the actual type: ~a\n"
        (type-name type) field-name field-size load-size)))

  (when (> (+ field-size offset) (* (get-load-size type) 8))
    (error (format
        "Type ~a's bitfield ~a will run off the end of the type (ends at ~a bits type is ~a bits)\n"
        (type-name type) field-name (+ field-size offset) (* (get-load-size type) 8))))

  ;; 128-bit bitfields have the limitation that fields cannot cross the 64-bit boundary.
  (when (and (< offset 64)
             (> (+ offset field-size) 64))
    (error (format
            "Type ~a's bitfield ~a will cross bit 64 which is not permitted. Range [~a ~a)"
            (type-name type) field-name offset (+ offset field-size))))

  (define field (make-sbitfield field-type field-name offset field-size skip-in-decomp))
  (gvector-add! (bitfield-type-fields type) field))



;; ==============================================================================
;; Virtual methods helpers
;; ==============================================================================

(define/contract (should-use-virtual-methods type method-id)
  (-> (or/c type? type-spec?) integer? boolean?)
  (if (type? type)
      (and
       (basic-type? type)
       (not (basic-type-is-final type))
       (not (method-info-no-virtual (lookup-method (type-name type) method-id))))
      (should-use-virtual-methods-ts type method-id)))

(define/contract (should-use-virtual-methods-ts ts method-id)
  (-> type-spec? integer? boolean?)
  (define it (types-find (base-type ts)))
  (cond
    ((it
      ;; it's a fully defined type
      (should-use-virtual-methods it method-id)))
    (else
     ;; it's a partially defined type.
     ;; for now we will prohibit calling a method on something that's defined only as a structure
     ;; because we don't know if it's actually a basic and should use virtual methods.
     (let ((fwd-dec-type (lookup-type-allow-partial-def ts)))
       (when (== (type-name fwd-dec-type) 'structure)
         (error (format
                 "Type ~a was forward declared as structure and it is not safe to call a method."
                 (inspect ts))))
       (should-use-virtual-methods fwd-dec-type method-id)))))

;; ==============================================================================
;; Generators
;; ==============================================================================

;; Generate the part of a deftype for the flag asserts and methods.
;; Doesn't include the final close paren of the deftype
;; This should work for both structure/bitfield definitions.

(define/contract (generate-deftype-footer this type)
  (-> type-system? type? string?)

  (define result "")
  (define (result-append str) (set! result (string-append result str)))

  (when (struct-type? type)
    (when (struct-type-pack type)
      (result-append "  :pack-me\n"))
    (when (struct-type-allow-misalign type)
      (result-append "  :allow-misaligned\n"))
    (when (struct-type-always-stack-singleton type)
      (result-append "  :always-stack-singleton\n")))

  (when (!= 0 (type-heap-base type))
    (result-append (format "  :heap-base #x(:x)\n" (type-heap-base type))))

  (define method-count (get-next-method-id type))
  (result-append (format "  :method-count-assert ~a\n" (get-next-method-id type)))
  (result-append (format "  :size-assert         #x(:~a)\n" (get-size-in-memory type)))
  (define flags (type-flags-new))
  (set-type-flags-heap-base! flags (type-heap-base type))
  (set-type-flags-size! (get-size-in-memory type))
  (set-type-flags-methods!  method-count)

  (result-append (format "  :flag-assert         #x~a\n  " (integer->hex (type-flags-flag flags))))
  (unless (type-generate-inspect type)
    (result-append ":no-inspect\n  "))

  (define methods-string "")
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Check and print the NEW method
  (define new-info (type-get-my-new-method type))
  (when new-info
    (define new-info-type (method-info-type new-info))
    (define new-info-argc (type-spec-args-count new-info-type))
    (string-append! methods-string "(new (")
    (for ((i (in-range 0 (1- new-info-argc))))
      (string-append! methods-string (type-spec-args-ref new-info-type i))
      (when (!= i (2- new-info-argc))
        (string-append! methods-string " ")))
    (string-append! methods-string (format ") ~a " (inspect (type-spec-args-last new-info))))

    (define behavior (type-spec-try-get-tag new-info-type 'behavior))
    (when behavior
      (string-append! methods-string (format ":behavior ~a " behavior)))
    (string-append! methods-string "0)\n    "))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Print all other methods
  (for-each-in-list
   (type-methods type)
   (lambda (info)
     (string-append! methods-string (format "(~a (" (method-info-name info)))
     (define info-type (method-info-type info))
     (define info-argc (type-spec-args-count info-type))
     (for ((i (in-range 0 (1- info-argc))))
       (string-append! methods-string (inspect (type-spec-args-ref info-type i)))
       (when (!= i (2- info-argc))
         (string-append! methods-string " ")))
     (string-append! methods-string (format ") ~a " (inspect (type-spec-args-last info))))

     (when (method-info-no-virtual info)
       (string-append! methods-string ":no-virtual "))

     (when (method-info-override info)
       (string-append! methods-string ":replace "))

     (define behavior (type-spec-try-get-tag info 'behavior))
     (when behavior
       (string-append! methods-string (format ":behavior ~a " behavior)))

     (when (== (base-type info-type) 'state)
       (string-append! methods-string ":state "))

     (string-append! methods-string (format "~a)\n    " (method-info-id info)))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (unless (== "" methods-string)
    (result-append "(:methods\n    ")
    (result-append methods-string)
    (result-append ")\n  "))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Make states string
  (define states-string "")
  (hash-for-each
   (type-states type)
   (lambda (name info)
     (define info-argc (type-spec-args-count info))
     (cond
       ((> info-argc 1)
        (string-append! states-string (format "    (~a" (type-spec-args-first info)))
        (for ((i (in-range 0 (1- info-argc))))
          (string-append! states-string " ")
          (string-append! states-string (inspect (type-spec-args-ref info i))))

        (string-append! states-string ")\n"))
       (else
        (string-append! states-string (format "    ~a\n" name))))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (unless (== "" states-string)
    (result-append "(:states\n")
    (result-append states-string)
    (result-append "    )\n  "))

  (result-append ")\n")
  result)

(define/contract (generate-deftype-for-structure ts st)
  (-> type-system? struct-type? string?)
  (define result "")
  (define (result-append str) (set! result (string-append result str)))
  (string-append! result (format "(deftype ~a (~a)\n  (" (type-name st) (type-parent st)))

  (define longest-field-name 0)
  (define longest-type-name 0)
  (define longest-mods 0)

  (define inline-string ":inline")
  (define dynamic-string ":dynamic")
  (define user-offset-string ":offset xxx")
  (define has-offset-assert false)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Check lenght names to make nice format in the next step
  (for ((i (in-range (struct-type-idx-of-first-unique-field st) (struct-type-fields-count st))))
    (define field (struct-type-fields-ref st i))

    (set! longest-field-name (max longest-field-name (length (field-name field))))
    (set! longest-type-name (max longest-type-name (length (inspect (field-type)))))

    (define mods 0)
    ;; mods are array size :inline :dynamic
    (when (and (field-is-array field)
               (not (field-is-dynamic field)))
      (set! mods (+ mods (length (~a (field-array-size field))))))

    (when (field-is-inline field)
      (when mods (+1! mods)) ; space
      (+! mods (length inline-string)))

    (when (field-is-dynamic field)
      (when mods (+1! mods))  ;; space
      (+! mods (length dynamic-string)))

    (unless (field-is-user-placed field)
      (set! has-offset-assert true))
    (set! longest-mods (max longest-mods mods)))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (for ((i (in-range (struct-type-idx-of-first-unique-field st)
                     (struct-type-fields-count st))))
    (define field (struct-type-fields-ref st i))
    (string-append! result "(")
    (string-append! result (field-name field))
    (string-append! result (make-string (1+ (- longest-field-name (length (field-name field)))) #\space))
    (string-append! result (inspect (field-type field)))
    (string-append! result (make-string (1+ (- longest-type-name (length (inspect (field-type field))))) #\space))

    (define mods "")
    (when (and (field-is-array field) (not (field-is-dynamic field)))
      (string-append! mods (~a (field-array-size field)))
      (string-append! mods " "))

    (when (field-is-inline field)
      (string-append! mods inline-string)
      (string-append! mods " "))

    (when (field-is-dynamic field)
      (string-append! mods dynamic-string)
      (string-append! mods " "))

    (result-append mods)
    (result-append (make-string (- longest-mods (1- (length (mods)))) #\space))

    (cond
      ((not (field-is-user-placed field))
       (result-append ":offset-assert "))
      (else
       (if has-offset-assert
           (result-append ":offset        ")
           (result-append ":offset "))))

    (result-append (~a (field-offset field) #:width 3))
    (result-append ")\n   "))

  (result-append ")\n")
  (result-append (generate-deftype-footer st))

  result)

(define/contract (generate-deftype-for-bitfield this type)
  (-> type-system? bitfield-type? string?)

  (define result "")
  (define (result-append str) (set! result (string-append result str)))

  (result-append (format "(deftype ~a (~a)\n  (" (type-name type) (type-parent type)))

  (define longest-field-name 0)
  (define longest-type-name 0)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Check lenght names to make nice format in the next step
  (for-each-in-list
   (bitfield-type-fields type)
   (lambda (field)
     (set! longest-field-name (max longest-field-name (length (field-name field))))
     (set! longest-type-name (max longest-type-name (length (field-type field))))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (for-each-in-list
   (bitfield-type-fields type)
   (lambda (field)
     (result-append "(")
     (result-append (field-name field))
     (result-append (make-string (1+ (- longest-field-name (length (field-name field)))) #\space))
     (result-append (inspect (field-type field)))
     (result-append (make-string (1+ (- longest-type-name (length (field-type field)))) #\space))

     (result-append (format ":offset (~a) :size (~a)"
                            (~a (sbitfield-offset field) #:width 3)
                            (~a (sbitfield-size field)  #:width 3)))
     (result-append ")\n   ")))

  (result-append ")\n")
  (result-append (generate-deftype-footer this type))
  result)
