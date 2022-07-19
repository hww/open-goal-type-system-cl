(in-package :type-system)

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

;; ==============================================================================
;; Globals
;; ==============================================================================

(defstruct type-system
  ;; The table of all known types -- contains the types
  (types (make-hash) :type hash-table)
  ;; The table of all forward declared types - contains only the symbols
  ;; aka type names
  (forward-declared-types (make-hash) :type hash-table)
  ;; The table of all forward declared types - contains quantiry of
  ;; methods for types
  (forward-declared-method-counts (make-hash) :type hash-table)
  ;; Keep the types after redefinition
  (old-types (arr-new) :type array)
  ;; Allow the type redefinition
  (allow-redefinition t :type boolean)
  )

(defun type-system-new ()
  ;;(-> type-system?)
  (let ((this (make-type-system :types (make-hash)
				:forward-declared-types	(make-hash)
				:forward-declared-method-counts (make-hash)
				:old-types (arr-new :capacity 256)
				:allow-redefinition t)))
    ;; TODO
;;    (define-initial this)
    this))


;; ------------------------------------------------------------------------------
;; Helpers for a globals
;; ------------------------------------------------------------------------------

;; Find the type
(defun types-find (this name)
  ;;(-> type-system? symbol? (or/c #f type?))
  (hash-ref (type-system-types this) name nil))

(defun types-set! (this name val)
  ;;(-> type-system? symbol? type? void?)
  (hash-set! (type-system-types this) name val))

;; Find the type
(defun forward-declared-types-find (this name)
  ;;(-> type-system? symbol? (or/c nil symbol?))
  (hash-ref (type-system-forward-declared-types this) name nil))

(defun forward-declared-types-set! (this name val)
  ;;(-> type-system? symbol? symbol? void?)
  (hash-set! (type-system-forward-declared-types this) name val))

;; Find the type
(defun forward-declared-method-counts-find (this name)
  ;;(-> type-system? symbol? (or/c nil symbol?))
  (hash-ref (type-system-forward-declared-method-counts this) name nil))

(defun forward-declared-method-counts-set! (this name val)
  ;;(-> type-system? symbol? integer? void?)
  (hash-set! (type-system-forward-declared-method-counts this) name val))

;; Store old type

(defun old-types-push (this item)
  ;;(-> type-system? any/c void?)
  (arr-push (type-system-old-types this) item))

;; Debugging function to print out all types, and their methods and fields.

(defun inspect-all-type-information (this)
  ;;;(-> type-system? string?)
  (string-append
   #?"\nALL TYPES ===================\n"
   (string-join (hash-map (type-system-types this)
			  (lambda (k v) (assert k) (inspect v))) #?"\n")
   #?"\nEND OF ALL TYPES ============\n"))

;; Clear types
(defun types-clear (this)
  ;;(-> type-system? void)
  (hash-clear! (type-system-types this))
  (hash-clear! (type-system-forward-declared-types this))
  (hash-clear! (type-system-forward-declared-method-counts this))
  (setf (type-system-old-types this) (arr-new :capacity 256))
  ;; TODO
;;  (define-initial this)
  )

(defun define-initial (this)
  ;;(-> type-system? void?)
  ;; the "none" and "_type_" types are included by default.
  (add-type this 'none      (null-type-new 'none))
  (add-type this '_type_    (null-type-new '_type_))
  (add-type this '_varargs_ (null-type-new '_varargs_))
  ;; OBJECT
  (add-type this 'object    (value-type-new 'object 'object nil 4 nil))
  )

;; ==============================================================================

(defun base-type (o)
  ;;(-> (or/c type? type-spec?) symbol?)
  (cond
    ((typespec-p o) (typespec-basetype o))
    ((gtype-p o) (gtype-base-type o))
    (else (assert false))))

;; ==============================================================================
;; How to define a type
;; ==============================================================================

;; Add a new type. If the type exists, and this new type is different,
;; it is an error if throw_on_redefine is set. The type should be
;; fully set up (fields, etc) before running this.

(defun add-type (this name type)
  ;;(-> type-system? symbol? type? (or/c nil type?))
  (define method-kv (forward-declared-method-counts-find this name))
  (when method-kv
    (let ((method-count (get-next-method-id type)))
      (when (!= method-count method-kv)
        (error
         (format nil "Type ~a was defined with ~a methods but was forward declared with ~a\n"
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
                (error (format nil "Inconsistent type definition. Type ~a was originally\n~a\nand is redefined as\n~a\nDiff:\n~a\n"
                               (type-name kv) (inspect kv) (inspect type) (diff kv type)))))))
      (else
       ;; newly defined!
       ;; none/object get to skip these checks because they are roots.
       (when (and (!= name 'object)
                  (!= name 'none)
                  (!= name '-type-)
                  (!= name '-varargs-))
         (when (forward-declared-types-find this (type-get-parent type))
           (error (format nil "Cannot create new type `~a`. The parent type `~a` is not fully defined.\n"
                          (type-name type) (type-get-parent type)))

           (unless (types-find this (type-get-parent type))
             (error (format nil "Cannot create new type `~a`. The parent type `~a` is not defined.\n"
                            (type-name type) (type-get-parent type))))))

       (types-set! this name type)
       (define fwd-it (forward-declared-types-find this name))
       (when fwd-it
         ;; need to check parent is correct.
         (when (not (tc this (type-spec-new fwd-it) (type-spec-new name)))
           (error (format nil "Type ~a was original declared as a child of ~a but is not.\n" name fwd-it))))
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

(defun forward-declare-type-as-type (this name)
  ;;(-> type-system? symbol? void)
  (log-debug "forward-declare: ~a\n" name)
  (unless (types-find this name)
    ;; only if not defined
    (let ((it (forward-declared-types-find this name )))
      (if it
          (error (format nil "Tried to forward declare ~a as a type multiple times.  Previous: ~a Current: object" name it))
          (forward-declared-types-set! this name 'object)))))

;; Inform the type system that there will eventually be a type named "name"
;; and that it's a basic. This allows the type to be used in a few specific
;; places. For instance a basic can have a field where an element is the
;; same type.

(defun forward-declare-type-as (this new-type parent-type)
  ;;(-> type-system? symbol? symbol? void)
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
                             nil ;; dummy value
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

(defun forward-declare-type-method-count (this name num-methods)
  ;;(-> type-system? symbol? integer? void)
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

;; ==============================================================================
;; Get method count
;; ==============================================================================

(defun get-type-method-count (name)
  ;;(-> symbol? integer?)
  (define result (try-get-type-method-count(name)))
  (if result
      result;
      (error (format nil "Tried to find the number of methods on type ~a but it is not defined."
                     name))))

;; Get method count for fully declared or for partialy declared

(defun try-get-type-method-count (name)
  ;;(-> symbol? (or/c nil integer?))
  (let ((type-it (types-find name)))
    (cond
      (type-it
       (get-next-method-id type-it))
      (else
       (forward-declared-method-counts-find name)))))

;; Get the runtime type (as a name string) of a TypeSpec.
;; Gets the runtime type of the primary type of the TypeSpec.

(defun get-runtime-type (ts)
 ;;(-> type-spec? symbol?)
  (type-runtime-name (lookup-type ts)))

;; ==============================================================================
;; Deref
;; ==============================================================================
(defstruct deref-info
   (can-deref nil :type boolean)
   (mem-deref nil :type boolean)
   (sign-extend nil :type boolean)
   (stride -1 :type integer)
   (load-size -1 :type integer)
   (result-type nil :type typespec)
   )


(defun get-deref-info (type)
  ;;(-> type-spec? deref-info?)
  (error "TODO")
  (make-deref-info))

;; ==============================================================================
;; Few predicates
;; ==============================================================================

(defun fully-defined-type-exists (this type-or-name)
  ;;(-> type-system? (or/c symbol? type-spec?) any/c)
      (if (symbolp type-or-name)
          (types-find this type-or-name)
          (fully-defined-type-exists this (base-type type-or-name))))

(defun partially-defined-type-exists (this name)
  ;;(-> type-system? symbol? (or/c symbol? nil))
  (forward-declared-types-find this name))

;; ==============================================================================
;; The type specification builder
;; ==============================================================================

;; Make the type specification for the type
;
(defun make-typespec (this name)
  ;;(-> type-system? symbol? type-spec?)
  (if (or (types-find this name)
          (forward-declared-types-find this name))
        (type-spec-new name)
        (error (format nil "Type `~a` is unknown" name))))


(defun make-array-typespec (element-type)
  ;;(-> type-spec? type-spec?)
  (type-spec-new 'array (element-type)))

;; Create a typespec for a function.  If the function doesn't return
;; anything use "none" as the return type.

(defun make-function-typespec (this arg-types return-type)
  ;;(-> type-system? (listof symbol?) symbol? type-spec?)
  (define ts (make-typespec this 'function))
  (for ((it (in-list arg-types)))
    (type-spec-args-add ts (make-typespec this it)))
  (type-spec-args-add ts (make-typespec this return-type))
  ts)

;; Create a TypeSpec for a pointer to a type.

(defun make-pointer-typespec (this type-or-name)
  ;;(-> type-system? (or/c symbol? type-spec?) type-spec?)
  (if (symbolp type-or-name)
      (type-spec-new this 'pointer (make-typespec this type-or-name))
      (type-spec-new this 'pointer type)))

;; Create a TypeSpec for an inline-array of type

(defun make-inline-array-typespec (this type)
  ;;(-> type-system? (or/c symbolp type-spec?) type-spec?)
  (if (symbolp type)
      (type-spec-new this 'inline-array (make-typespec this type))
      (type-spec-new this 'inline-array type)))

;; ==============================================================================
;; Lookup types
;; ==============================================================================

;; Get full type information. Throws if the type doesn't exist. If the given
;; type is redefined after a call to lookup_type, the Type* will still be valid,
;; but will point to the old data. Whenever possible, don't store a Type* and
;; store a TypeSpec instead. The TypeSpec can then be used with lookup_type to
;; find the most up-to-date type information.

(defun lookup-type-by-name (this name)
  ;;(-> type-system? symbolp type?)
  (let ((type (types-find this name)))
    (cond  (type type)
           ((forward-declared-types-find this name)
            (error (format nil "Type ~a is not fully defined." name)))
           (else
            (error (format nil "Type ~a is not defined." name))))))

;; Get full type information. Throws if the type doesn't exist. If the given
;; type is redefined after a call to lookup_type, the Type* will still be valid,
;; but will point to the old data. Whenever possible, don't store a Type* and
;; store a TypeSpec instead. The TypeSpec can then be used with lookup_type to
;; find the most up-to-date type information.

(defun lookup-type (this ts-or-name)
  ;;(-> type-system? (or/c type-spec? symbolp) type?)
  (if (symbolp ts-or-name)
      (lookup-type-by-name this ts-or-name)
      (lookup-type this (base-type ts-or-name))))

;; Get type info. If the type is not fully defined (ie, we are parsing its
;; deftype now) and its forward defined as a basic or structure, just get
;; basic/structure.

(defun lookup-type-allow-partial-def (this ts-or-name)
  ;;(-> type-system? (or/c symbolp type-spec?) type?)
  (if (symbolp ts-or-name)
      (lookup-type-allow-partial-def-by-name this ts-or-name)
      (lookup-type-allow-partial-def this (base-type ts-or-name))))

;; Get type info. If the type is not fully defined (ie, we are parsing its
;; deftype now) and its forward defined as a basic or structure, just get
;; basic/structure.

(defun lookup-type-allow-partial-def-by-name (this name)
  ;;(-> type-system? symbolp type?)
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
                  (error (format nil "The type ~a is unknown." name))
                  (error (format nil "When looking up forward defined type ~a, could not find a type ~a."
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
                        (error (format nil "The type ~a reffer to byself by forward declaration but there is no type structure for it"
                                       frw-type))
                        (:loop: frw-type))))))))))))

;; Get a type by name and cast to a child class of Type*. Must succeed.

(defun get-type-of-type (this type-predicate type-name)
  ;;(-> type-system? procedure? symbolp any/c)
  (let ((type (types-find this type-name)))
    ;; the type is found
    (cond
      ((and type (type-predicate type)) type)
      (else
       (error (format nil "Failed to get ~a as the right type ~a, found ~a"
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

(defun get-load-size-allow-partial-def (this ts)
  ;;(-> type-system? type-spec? integer?)
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

(defun declare-method (this
		       type-or-name
		       method-name
		       no-virtual
		       ts
		       override-type)
  ;;(-> type-system? (or/c symbolp type?) symbolp boolean? type-spec? boolean? method-info?)

  (if (symbolp type-or-name)
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

(defun declare-method-for-type (this
				type
				method-name
				no-virtual
				ts
				override-type
				(id -1))
  ;;(->* (type-system? type? symbolp boolean? type-spec? boolean?) (integer?) method-info?)
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
                                               t)))
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

(defun define-method (type-or-name method-name ts)
  ;;(-> (or/c type? symbolp) symbolp type-spec? method-info?)

   (if (symbolp type-or-name)
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

(defun define-method-for-type (this type method-name ts)
  ;;(-> type-system? type? symbolp type-spec? method-info?)
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
          (error (format nil "Cannot add method ~a to type ~a because it was not declared.\n"
                         method-name (type-name type)))))))))

;; Special case to add a new method, as new methods can specialize the
;; arguments. If it turns out that other child methods can specialize arguments
;; (seems like a bad idea), this may be generalized.

(defun add-new-method (this type ts)
  ;;(-> type-system? type? type-spec? method-info?)
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

(defun lookup-method (type-name method-name)
  ;;(-> symbolp symbolp method-info?)
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
                (error (format nil "The method ~a of type ~a could not be found.\n"
                               method-name type-name))))))))))

;; Try lookup method by the type and method name will not make an exeption

(defun try-lookup-method (this type-or-name method-name-or-id)
  ;;(-> type-system? (or/c type? symbolp) (or/c symbolp integer?) (or/c nil method-info?))
  (cond
    ((and (symbolp type-or-name) (symbolp method-name-or-id))
     (try-lookup-method-by-name this type-or-name method-name-or-id))
    ((and (symbolp type-or-name) (integer? method-name-or-id))
     (try-lookup-method-by-id this type-or-name method-name-or-id))
    ((and (type? type-or-name) (symbolp method-name-or-id))
     (try-lookup-method-of-type this  type-or-name method-name-or-id))
    (else (error (format nil "Bad arguments\n [0] ~a\n [1] ~a\n" type-or-name method-name-or-id)))))

;; Lookup by name of type and method

(defun try-lookup-method-by-name (this type-name method-name)
  ;;(-> type-system? symbolp symbolp (or/c nil method-info?))
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
           nil)))))

;; Like lookup-method, but won't throw or print an error
;; when things go wrong.

(defun try-lookup-method-by-id (this type-name method-id)
  ;;(-> type-system? symbolp integer? (or/c nil method-info?))
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
      ((not iter-type) nil)
      (else
       (define m (type-get-any-method-by-id iter-type method-id))
       (cond
         ;; Case we found it
         (m m)
         ;; Did not found
         (else
          (if (type-has-parent? iter-type)
              (:loop: (lookup-type this (type-get-parent iter-type)))
              nil)))))))

;;

(defun try-lookup-method-of-type (this type method-name)
  ;;(-> type-system? type? symbolp (or/c nil method-info?))
  (define (type-get-any-method type name)
    (cond
      ((== name 'new)
       (type-get-my-new-method type))
      (else
       (type-get-my-method type method-name))))
  ;; look up the method
  (let :loop: ((iter-type type))
    (cond
      ((not iter-type) nil)
      (else
       (define m (type-get-any-method iter-type method-name))
       (cond
         ;; Case we found it
         (m m)
         ;; Did not found
         (else
          (if (type-has-parent? iter-type)
              (:loop: (lookup-type this (type-get-parent iter-type)))
              nil)))))))

;; Lookup information on a method by ID number. Error if it can't be found. Will
;; check parent types if the given type doesn't specialize the method.

(defun lookup-method-by-method-id (this type-name method-id)
;;(-> type-system? symbolp integer? method-info?)
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

(defun lookup-new-method (this type-name)
  ;;(-> type-system? symbolp method-info?)
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

(defun assert-method-id (this type-name method-name id)
  ;;(-> type-system? symbolp symbolp integer? void?)
  (define info (lookup-method this type-name method-name))
  (unless (== (method-info-id info) id)
      (error (format nil "Method ID assertion failed: type ~a, method ~a id was ~a, expected ~a\n"
                     type-name method-name (method-info-id info) id))))

;; Get the next free method ID of a type.

(defun get-next-method-id (this type)
  ;;(-> type-system? type? integer?)

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

  (defun find-method-ids (type)
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

(defstruct field-lookup-info
(field nil :type field)
(type nil :type gtype)
(needs-deref nil :type boolean)
(array-size -1 :type integer))

;; Default constructor

(defun field-lookup-info-new ()
  ;;(-> field-lookup-info?)
  (make-field-lookup-info nil nil nil -1))

;; Lookup detailed information about a field of a type by name, including type,
;; offset, and how to access it.

(defun lookup-field-info (type-name field-name)
;;(-> symbolp symbolp field-lookup-info?)
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

(defun assert-field-offset (type-name field-name offset)
  ;;(-> symbolp symbolp integer? void?)
  (define field (lookup-field type-name field-name))
  (unless (== (field-offset field) offset)
    (error (format nil "assert-field-offset(~a, ~a, ~a) failed - got ~a\n"
                   type-name field-name offset))))


; Add a field to a type. If offset_override is -1
;; (the default), will place it automatically.

(defun add-field-to-type (this
			  type
			  fld-name
			  fld-type
			  (is-inline nil)
			  (is-dynamic nil)
			  (array-size -1)
			  (offset-override -1)
			  (skip-in-static-decomp nil)
			  (score 0))
  ;;(->* (type-system? struct-type? symbolp type-spec?)
  ;;     (boolean? boolean? integer? integer? boolean? real?)
  ;;     integer?)
  ;;(printf "Add field ~a to type ~a\n" fld-name (type-name type))
  (when (struct-type-lookup-field type fld-name)
    (error (format nil "Type `~a` already has a field named `~a`\n" (type-name type) fld-name)))

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
       (error (format nil "Tried to place field `~a` at `~a`, but it is not aligned correctly\n"
                      fld-name offset)))))
  ;;
  (set-field-offset! field offset)
  (set-field-alignment! field field-alignment)
  (when skip-in-static-decomp
    (set-field-skip-in-static-decomp! field t))

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

(defun lookup-field (type-name field-name)

  ;;(-> symbolp symbolp field?)
  ;; next method will make exception in bad case
  (define atype (get-type-of-type struct-type? type-name))
  (define field (lookup-field atype field-name))
  (unless field
    (error (format nil "Type ~a has no field named ~a\n"
                   type-name field-name)))
  field)


;; ==============================================================================
;; Field alignement
;; ==============================================================================

;; Get the minimum required aligment of a field.

(defun get-alignment-in-type (this field)
  ;;(-> type-system? field? integer?)

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

(defun allow-inline (type)
  ;;(-> type? boolean?)
  (define name (type-name type))
  (and (!= name 'basic) (!= name 'structure)))

;; Get the size of a field in a type. The array sizes should be consistent with
;; get-deref-info's stride.

(defun get-size-in-type (this field)
  ;;(-> type-system? field? integer?)
  (define fld-type-spec (field-type field))
  (define fld-type (lookup-type-allow-partial-def this fld-type-spec))

  ;; Helper: Get size of field in case if it is array
  (define (get-size-for-array)
    (cond
    ((field-is-inline field)
      (when (not (fully-defined-type-exists this fld-type-spec))
        (error (format nil "Cannot use the forward-declared type ~a in an inline array.\n"
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
         (error (format nil "Cannot use the forward-declared type ~a inline.\n"
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

(defun tc (this expected actual)
  ;;(-> type-system? type-spec? type-spec? boolean?)
  (typecheck-and-throw this expected actual "" nil nil nil))


;; Is actual of type expected? For base types.

(defun typecheck-base-types (this expected actual allow_alias)
  ;;(-> type-system? symbolp symbolp boolean? boolean?)
  ;;
  ;; The expected type should be found in the actual type tree
  ;;
  (defun find-same-type this expected actual)
    ;;(-> type-system? symbolp symbolp boolean?)
    (if (== expected actual)
        ;; types already same
        t
        ;; find actual type and name
        (let* ((actual-type (lookup-type-allow-partial-def this actual))
               (actual-name (type-name actual-type)))
          (if (== expected actual-name)
              ;; expected and actual types are identical
              t
              ;; repeat this function for parent of actual type
              (if (type-has-parent? actual-type)
                  (find-same-type this expected (type-parent actual-type))
                  nil)))))
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

(defun typecheck-and-throw this
                                      expect
                                      actual
                                      error-source-name
                                      print-on-error
                                      throw-on-error
                                      allow-type-alias)
  ;;(-> type-system? type-spec? type-spec? string? boolean? boolean? boolean? void)

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
                   t
                   ;; don't print/throw because the error would be confusing. Better to fail only the
                   ;; outer most check and print a single error message.
                   (if (tc (type-spec-args-ref expect i) (type-spec-args-ref actual i))
                       (:loop: (+ 1 i))
                       nil)))
             ;; different sizes of arguments.
             (if (== exp-args-count 0)
                      ;; we expect zero arguments, but got some. The actual type is more specific, so this is fine.
                      t
                      ;; different sizes, and we expected arguments. No good!
                      nil)))))

;; ==============================================================================
;; Enum
;; ==============================================================================

;; Find enum

(defun try-enum-lookup (this type-or-name)
  ;;(-> type-system? (or/c type-spec? symbolp) (or/c nil enum-type?))
  (when (type-spec? type-or-name)
    (set! type-or-name (base-type type-or-name)))
  (let ((type (types-find this type-or-name)))
    ;; the type is found
    (cond
      ((and type (enum-type? type)) type)
      (else nil))))

;; ==============================================================================
;; The path of a type
;; ==============================================================================

;; Get a path from type to object.

(defun get-path-up-tree (this name)
  ;;(-> type-system? symbolp (listof symbolp))
  (if (== name 'object)
      (list 'object)
      (cons name
            (let :loop: ((type (lookup-type-allow-partial-def this name)))
              (if (type-has-parent? type)
                  (cons (type-name type) (:loop: (lookup-type-allow-partial-def this (type-name type))))
                  (cons (type-name type) null))))))

;; ==============================================================================
;; Common accessor
;; ==============================================================================

;; Lowest common ancestor of two base types.

(defun lca-base (this typea typeb)
  ;;(-> type-system? symbolp symbolp (or/c nil symbolp))
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
             nil))))))


;; Lowest common ancestor of two typespecs. Will recursively apply to arguments,
;; if compatible.
;;
;; Otherwise arguments are stripped off.
;; In a situation like lca("(a b)", "(c d)"), the result will be
;; (lca(a, b) lca(b, d)).

(defun lowest-common-ancestor (this a b)
  ;;(-> type-system? type-spec? type-spec? type-spec?)
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

(defun lowest-common-ancestor-in-vector (this types)
  ;;(-> type-system? (vectorof type-spec?) type-spec?)
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
(defun lowest-common-ancestor-reg (a b)
  ;;(-> type-spec? type-spec? type-spec?)
  (coerce-to-reg-type (lowest-common-ancestor a b)))

;; Converts a type in memory to the type you'll get in a register after loading it.

(defun coerce-to-reg-type (in)
  ;;(-> type-spec? type-spec?)
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

(defstruct bitfield-lookup-info
(result-type nil :type gtype)
(offset -1 :type integer)
(size -1 :type integer)
(sign-extend nil :type boolean)
)

;; Default constructor

(defun bitfield-lookup-info-new ()
  ;;(-> field-lookup-info?)
  (make-bitfield-lookup-info nil -1 -1 nil))

;; Is the given type a bitfield type?

(defun is-bitfield-type (this type-name)
;;(-> type-system? symbolp boolean?)
  (get-type-of-type bitfield-type? type-name))

;; Get information about a field within a bitfield type.

(defun lookup-bitfield-info (this type-name field-name)
  ;;(-> type-system? symbolp symbolp bitfield-lookup-info?)
  (define type (get-type-of-type this bitfield-type? type-name))

  (define f (struct-type-lookup-field type field-name))
  (when (not f)
    (error (format nil "Type ~a has no bitfield named ~a\n" type-name field-name)))

  (make-bitfield-lookup-info
   (sbitfield-type f) ; resutl type
   (sbitfield-offset f)
   (get-load-signed (lookup-type this (field-type f)))
   (sbitfield-size f)))

;; Add a new field to a bitfield type. Set the field size to -1 if you want to
;; just use the size of the type and not clip it.

(defun add-field-to-bitfield (this type field-name field-type offset field-size skip-in-decomp)
  ;;(-> type-system? bitfield-type? symbol? type-spec? integer? integer? boolean? void)
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
  (arr-push (bitfield-type-fields type) field))



;; ==============================================================================
;; Virtual methods helpers
;; ==============================================================================

(defun should-use-virtual-methods (type method-id)
  ;;(-> (or/c type? type-spec?) integer? boolean?)
  (if (type? type)
      (and
       (basic-type? type)
       (not (basic-type-is-final type))
       (not (method-info-no-virtual (lookup-method (type-name type) method-id))))
      (should-use-virtual-methods-ts type method-id)))

(defun should-use-virtual-methods-ts (ts method-id)
  ;;(-> type-spec? integer? boolean?)
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

(defun generate-deftype-footer this type)
  ;;(-> type-system? type? string?)

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
    (result-append (format nil "  :heap-base #x(:x)\n" (type-heap-base type))))

  (define method-count (get-next-method-id type))
  (result-append (format nil "  :method-count-assert ~a\n" (get-next-method-id type)))
  (result-append (format nil "  :size-assert         #x(:~a)\n" (get-size-in-memory type)))
  (define flags (type-flags-new))
  (set-type-flags-heap-base! flags (type-heap-base type))
  (set-type-flags-size! (get-size-in-memory type))
  (set-type-flags-methods!  method-count)

  (result-append (format nil "  :flag-assert         #x~a\n  " (integer->hex (type-flags-flag flags))))
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
    (string-append! methods-string (format nil ") ~a " (inspect (type-spec-args-last new-info))))

    (define behavior (type-spec-try-get-tag new-info-type 'behavior))
    (when behavior
      (string-append! methods-string (format nil ":behavior ~a " behavior)))
    (string-append! methods-string "0)\n    "))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Print all other methods
  (for-each-in-list
   (type-methods type)
   (lambda (info)
     (string-append! methods-string (format nil "(~a (" (method-info-name info)))
     (define info-type (method-info-type info))
     (define info-argc (type-spec-args-count info-type))
     (for ((i (in-range 0 (1- info-argc))))
       (string-append! methods-string (inspect (type-spec-args-ref info-type i)))
       (when (!= i (2- info-argc))
         (string-append! methods-string " ")))
     (string-append! methods-string (format nil ") ~a " (inspect (type-spec-args-last info))))

     (when (method-info-no-virtual info)
       (string-append! methods-string ":no-virtual "))

     (when (method-info-override info)
       (string-append! methods-string ":replace "))

     (define behavior (type-spec-try-get-tag info 'behavior))
     (when behavior
       (string-append! methods-string (format nil ":behavior ~a " behavior)))

     (when (== (base-type info-type) 'state)
       (string-append! methods-string ":state "))

     (string-append! methods-string (format nil "~a)\n    " (method-info-id info)))))
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
        (string-append! states-string (format nil "    (~a" (type-spec-args-first info)))
        (for ((i (in-range 0 (1- info-argc))))
          (string-append! states-string " ")
          (string-append! states-string (inspect (type-spec-args-ref info i))))

        (string-append! states-string ")\n"))
       (else
        (string-append! states-string (format nil "    ~a\n" name))))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (unless (== "" states-string)
    (result-append "(:states\n")
    (result-append states-string)
    (result-append "    )\n  "))

  (result-append ")\n")
  result)

(defun generate-deftype-for-structure ts st)
  ;;(-> type-system? struct-type? string?)
  (define result "")
  (define (result-append str) (set! result (string-append result str)))
  (string-append! result (format nil "(deftype ~a (~a)\n  (" (type-name st) (type-parent st)))

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
    (result-append #?")\n   "))

  (result-append #?")\n")
  (result-append (generate-deftype-footer st))

  result)

(defun generate-deftype-for-bitfield this type)
  ;;(-> type-system? bitfield-type? string?)

  (define result "")
  (define (result-append str) (set! result (string-append result str)))

  (result-append (format nil "(deftype ~a (~a)~%  (" (type-name type) (type-parent type)))

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

     (result-append (format nil ":offset (~a) :size (~a)"
                            (~a (sbitfield-offset field) #:width 3)
                            (~a (sbitfield-size field)  #:width 3)))
     (result-append %?")\n   ")))

  (result-append #?")\n")
  (result-append (generate-deftype-footer this type))
  result)
