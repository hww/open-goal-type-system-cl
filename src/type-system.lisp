(in-package :type-system)
(use-package :type-system/interfaces)
(use-package :type-system/typespec)
(use-package :type-system/type)

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
  (let ((this (make-type-system
               :types (make-hash)
               :forward-declared-types (make-hash)
               :forward-declared-method-counts (make-hash)
               :old-types (arr-new :capacity 256)
               :allow-redefinition t)))
    (define-initial this)
    this))


;; ------------------------------------------------------------------------------
;; Helpers for a globals
;; ------------------------------------------------------------------------------

;; Find the type
(defmethod types-find ((this type-system) (name string))
  ;;(-> type-system? symbol? (or/c #f type?))
  (hash-ref (type-system-types this) name nil))

(defmethod types-set! ((this type-system) (name string) val)
  ;;(-> type-system? symbol? type? void?)
  (hash-set! (type-system-types this) name val))

;; Find the type
(defmethod forward-declared-types-find ((this type-system) (name string))
  ;;(-> type-system? symbol? (or/c nil symbol?))
  (hash-ref (type-system-forward-declared-types this) name nil))

(defmethod forward-declared-types-set! ((this type-system) (name string) (val string))
  ;;(-> type-system? symbol? symbol? void?)
  (hash-set! (type-system-forward-declared-types this) name val))

;; Find the type
(defmethod forward-declared-method-counts-find ((this type-system) (name string))
  ;;(-> type-system? symbol? (or/c nil symbol?))
  (hash-ref (type-system-forward-declared-method-counts this) name nil))

(defmethod forward-declared-method-counts-set! ((this type-system) (name string) (val integer))
  ;;(-> type-system? symbol? integer? void?)
  (hash-set! (type-system-forward-declared-method-counts this) name val))

;; Store old type

(defmethod old-types-push ((this type-system) (item gtype))
  ;;(-> type-system? any/c void?)
  (arr-push (type-system-old-types this) item))

;; Debugging function to print out all types, and their methods and fields.

(defmethod inspect-all-type-information ((this type-system))
  ;;;(-> type-system? string?)
  (string-append
   (format nil "~%ALL TYPES ===================~%")
   (string-join
    (hash-map
     (type-system-types this)
     (lambda (k v) (assert k) (to-str v)))
    #\newline)
   (format nil "~%END OF ALL TYPES ============~%"))

;; Clear types
(defmethod types-clear ((this type-system))
  ;;(-> type-system? void)
  (hash-clear! (type-system-types this))
  (hash-clear! (type-system-forward-declared-types this))
  (hash-clear! (type-system-forward-declared-method-counts this))
  (setf (type-system-old-types this) (arr-new :capacity 256))
  ;; TODO
  (define-initial this)
  )

(defmethod define-initial ((this type-system))
  ;;(-> type-system? void?)
  ;; the "none" and "_type_" types are included by default.
  (add-type this "none"      (null-type-new "none"))
  (add-type this "_type_"    (null-type-new "_type_"))
  (add-type this "_varargs_" (null-type-new "_varargs_"))
  ;; OBJECT
  (add-type this "object"    (value-type-new "object" "object" nil 4 nil))
  )

;; ==============================================================================

(defun base-type (o)
  ;;(-> (or/c type? typespec?) symbol?)
  (cond
    ((typespec-p o) (typespec-basetype o))
    ((gtype-p o) (gtype-base-type o))
    (else (assert false))))

;; ==============================================================================
;; How to define a type
;; ==============================================================================
(defmacro log-warning (fmt &rest args) `(format t ,fmt ,@args))
(defmacro log-debug (fmt &rest args) `(format t ,fmt ,@args))

;; Add a new type. If the type exists, and this new type is different,
;; it is an error if throw_on_redefine is set. The type should be
;; fully set up (fields, etc) before running this.

(defun add-type (this name type)
  ;;(-> type-system? symbol? type? (or/c nil type?))
  (let ((method-kv (forward-declared-method-counts-find this name)))
    (when method-kv
      (let ((method-count (get-next-method-id this type)))
        (when (!= method-count method-kv)
          (error
           (format nil "Type ~a was defined with ~a methods but was forward declared with ~a~%"
                   name  method-count method-kv)))))
    (let ((kv (types-find this name)))
      (cond
        (kv
         ;; exists already
         (cond
           ((compare kv type)
            ;; exists and we are trying to change it!
            (when (type-system-allow-redefinition this)
              (log-warning "[TypeSystem] Type ~a was originally~%~a~%and is redefined as~%~a~%"
                           (gtype-name kv) (to-str kv) (to-str type))
              ;; extra dangerous we have allowed type redefinition!
              ;; keep the unique-ptr around just in case somebody references this old type pointer.
              (old-types-push this kv)
              ;; update the type
              (types-set! this name type)))
           (else (error (format nil "Inconsistent type definition. Type ~a was originally~%~a~%and is redefined as~%~a~%Diff:~%~a~%"
                                (gtype-name kv) (to-str kv) (to-str type) (diff kv type))))))
        (else
         ;; newly defined!
         ;; none/object get to skip these checks because they are roots.
         (when (and (!= name "object")
                    (!= name "none")
                    (!= name "_type_")
                    (!= name "_varargs_"))
           (when (forward-declared-types-find this (gtype-get-parent type))
             (error (format nil "Cannot create new type `~a`. The parent type `~a` is not fully defined.~%"
                            (gtype-name type) (gtype-get-parent type))))

           (unless (types-find this (gtype-get-parent type))
             (error (format
                     nil
                     "Cannot create new type `~a`. The parent type `~a` is not defined.~%"
                     (gtype-name type) (gtype-get-parent type)))))

         (types-set! this name type)
         (let ((fwd-it (forward-declared-types-find this name)))
           (when fwd-it
             ;; need to check parent is correct.
             (when (not (tc this (typespec-new fwd-it) (typespec-new name)))
               (error (format nil "Type ~a was original declared as a child of ~a but is not.~%" name fwd-it)))))
         (hash-remove! (type-system-forward-declared-types this) name))))
    (let ((res (types-find this name)))
      (if res
          (log-debug  "Defined         ~a~%" (to-str res))
          (log-warning "Was not defined ~a~%" name))
      res)))

;; ==============================================================================
;; Forward declare
;; ==============================================================================

;; Inform the type system that there will eventually be a type named "name".
;; This will allow the type system to generate TypeSpecs for this type, but
;; not access detailed information, or know the exact size.

(defun forward-declare-type-as-type (this name)
  ;;(-> type-system? symbol? void)
  (log-debug "forward-declare: ~a~%" name)
  (unless (types-find this name)
    ;; only if not defined
    (let ((it (forward-declared-types-find this name )))
      (if it
          (error (format nil "Tried to forward declare ~a as a type multiple times.  Previous: ~a Current: object" name it))
          (forward-declared-types-set! this name "object")))))

;; Inform the type system that there will eventually be a type named "name"
;; and that it's a basic. This allows the type to be used in a few specific
;; places. For instance a basic can have a field where an element is the
;; same type.

(defun forward-declare-type-as (this new-type parent-type)
  ;;(-> type-system? symbol? symbol? void)
  (log-debug "forward-declare: ~a as: ~a~%" new-type parent-type)
  (let ((type-it (types-find this new-type)))
    (if type-it
        ;; the type is already defined
        (progn
          (let ((parent-it (types-find this parent-type)))
            (when (not parent-it)
              (error
               "Got a forward declaration for known type ~a where the parent ~a is unknown"
               new-type parent-type)))
          (when (not (tc this (typespec-new parent-type) (typespec-new new-type)))
            (error
             "Got a forward definition that type ~a is a ~a which disagrees with existing fully-defined types."
             new-type parent-type))
          ;; ignore forward declaration
          )
        ;; the new type is not found
        (progn
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
                         (old-ts (typespec-new fwd-it)))
                     (cond
                       ((and old-parent-it new-parent-it)
                        ;; old and new types are found
                        ;; check the posibility of casting
                        (let ((new-ts (typespec-new (gtype-name new-parent-it))))
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
  (let ((existing-fwd (forward-declared-method-counts-find this name)))

    (when (and existing-fwd (!= existing-fwd num-methods))
      (error (format
              nil
              "Type ~a was originally forward declared with ~a methods and is now being forward declared with ~a methods"
              name existing-fwd num-methods)))

    (let ((existing-type (types-find this name)))
      (when existing-type
        (let ((existing-count (get-next-method-id this existing-type)))
          (when (!= existing-count num-methods)
            (error (format nil "Type (~a) was defined with ~a methods and is now being forward declared with ~a methods"
                    name existing-count num-methods))))))
    (forward-declared-method-counts-set! this name num-methods)))

;; ==============================================================================
;; Get method count
;; ==============================================================================

(defun get-type-method-count (this name)
  ;;(-> symbol? integer?)
  (let ((result (try-get-type-method-count this name)))
    (if result
        result
        (error (format nil "Tried to find the number of methods on type ~a but it is not defined."
                       name)))))

;; Get method count for fully declared or for partialy declared

(defun try-get-type-method-count (this name)
  ;;(-> symbol? (or/c nil integer?))
  (let ((type-it (types-find this name)))
    (cond
      (type-it
       (get-next-method-id this type-it))
      (else
       (forward-declared-method-counts-find this name)))))

;; Get the runtime type (as a name string) of a TypeSpec.
;; Gets the runtime type of the primary type of the TypeSpec.

(defun get-runtime-type (this ts)
 ;;(-> typespec? symbol?)
  (gtype-runtime-name (lookup-type this ts)))

;; ==============================================================================
;; Deref
;; ==============================================================================

(defstruct deref-info
   (can-deref nil :type boolean)
   (mem-deref nil :type boolean)
   (sign-extend nil :type boolean)
   (stride -1 :type integer)
   (load-size -1 :type integer)
   (result-type nil :type (or null typespec))
   (reg REG-CLASS-INVALID :type integer)
   )


(defun get-deref-info (this ts)
  ;;(-> type-system? typespec? deref-info?)
  (let ((info (make-deref-info)))
    (unless (typespec-has-single-arg ts)
      ;; not enough info.
      (setf (deref-info-can-deref this) nil)
      (return-from get-deref-info info))
    ;; default to GPR

    (setf (deref-info-reg info) REG-CLASS-GPR-64)
    (setf (deref-info-mem-deref info) true)

    (when (tc this (typespec-new "float") ts)
      (setf (deref-info-reg info) REG-CLASS-FLOAT))

    (cond
      ((== (base-type ts) "inline-array")
       (let* ((result-type (lookup-type this (typespec-get-single-arg ts))))
         (when (or (not (struct-type-p result-type))
                   (struct-type-dynamic result-type))
           (setf (deref-info-can-deref info) nil)
           (return-from get-deref-info info))

         ;; it's an inline array of structures. We can "dereference". But really we don't do a memory
         ;; dereference, we just add stride*idx to the pointer.
         (setf (deref-info-can-deref info) true)                   ;; deref operators should work...
         (setf (deref-info-mem-deref info) false)                  ;; but don't actually dereference a pointer
         (setf (deref-info-result-type info) (typespec-get-single-arg ts))  ;; what we're an inline-array of
         (setf (deref-info-sign-extend info) false)                ;; not applicable anyway

         (if (is-reference? result-type)
             ;; then
             (setf (deref-info-stride info) (align-n
                                             (get-size-in-memory result-type)
                                             (get-inl-array-stride-align result-type)))
             ;; else - can't have an inline array of value types!
             (assert false))))
      ((== (base-type ts) "pointer")
       (setf (deref-info-can-deref info) true)
       (setf (deref-info-result-type info) (typespec-get-single-arg ts))
       (let ((result-type (lookup-type-allow-partial-def this (deref-info-result-type info))))
         (if (is-reference? result-type)
             ;; in memory, an array of pointers
             (progn
               (setf (deref-info-stride info) POINTER-SIZE)
               (setf (deref-info-sign-extend info) false)
               (setf (deref-info-load-size info) POINTER-SIZE))
             (progn
               ;; an array of values, which should be loaded in the correct way to the correct register
               (setf (deref-info-stride info) (get-size-in-memory result-type))
               (setf (deref-info-sign-extend info) (get-load-signed result-type))
               (setf (deref-info-reg info) (get-preferred-reg-class result-type))
               (setf (deref-info-load-size info) (get-load-size result-type))
               (assert (== (get-size-in-memory result-type)
                           (get-load-size result-type)))))))
      (else
       (setf (deref-info-can-deref info) false)))
    info))

;; ==============================================================================
;; Few predicates
;; ==============================================================================

(defun fully-defined-type-exists (this type-or-name)
  ;;(-> type-system? (or/c symbol? typespec?) any/c)
      (if (stringp type-or-name)
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
(defmethod make-a-typespec ((this type-system) (name string))
  ;;(-> type-system? symbol? typespec?)
  (if (or (types-find this name)
          (forward-declared-types-find this name))
      (typespec-new name)
      (progn
        (error (format nil "Can't make a typespec because type `~a` is unknown" name)))))


(defun make-array-typespec (element-type)
  ;;(-> typespec? typespec?)
  (typespec-new "array" (list element-type)))

;; Create a typespec for a function.  If the function doesn't return
;; anything use "none" as the return type.

(defun make-function-typespec (this arg-types return-type)
  ;;(-> type-system? (listof symbol?) symbol? typespec?)
  (let ((ts (make-a-typespec this "function")))
    (loop for it in arg-types do
         (typespec-args-add ts (make-a-typespec this it)))
    (typespec-args-add ts (make-a-typespec this return-type))
    ts))

;; Create a TypeSpec for a pointer to a type.

(defun make-pointer-typespec (this type-or-name)
  ;;(-> type-system? (or/c symbol? typespec?) typespec?)
  (if (stringp type-or-name)
      (typespec-new this "pointer" (make-a-typespec this type-or-name))
      (typespec-new this "pointer" type-or-name)))

;; Create a TypeSpec for an inline-array of type

(defun make-inline-array-typespec (this type)
  ;;(-> type-system? (or/c symbolp typespec?) typespec?)
  (if (stringp type)
      (typespec-new this "inline-array" (make-a-typespec this type))
      (typespec-new this "inline-array" type)))

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
  ;;(-> type-system? (or/c typespec? symbolp) type?)
  (if (stringp ts-or-name)
      (lookup-type-by-name this ts-or-name)
      (lookup-type this (base-type ts-or-name))))

;; Get type info. If the type is not fully defined (ie, we are parsing its
;; deftype now) and its forward defined as a basic or structure, just get
;; basic/structure.

(defun lookup-type-allow-partial-def (this ts-or-name)
  ;;(-> type-system? (or/c symbolp typespec?) type?)
  (if (stringp ts-or-name)
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
       (let ((cur-name name))
         (loop
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
                      (return type)
                      ;; the type is not found, try deduct fw-decl
                      ;; of cur-name
                      (if (== frw-type cur-name)
                          (error (format nil
                                         "The type ~a reffer to byself by forward declaration but there is no type structure for it"
                                         frw-type))
                          (setf cur-name frw-type)))))))))))))

;; Get a type by name and cast to a child class of Type*. Must succeed.

(defun get-type-of-type (this type-predicate type-name)
  ;;(-> type-system? procedure? symbolp any/c)
  (let ((type (types-find this type-name)))
    ;; the type is found
    (cond
      ((and type (funcall type-predicate type)) type)
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
  ;;(-> type-system? typespec? integer?)
  (let ((fully-defined-it (types-find this (base-type ts))))
    (cond
      (fully-defined-it
       (get-load-size fully-defined-it))
      (else

       (let ((partial-def (lookup-type-allow-partial-def this ts)))
         (when (not (tc this (typespec-new "structure") ts))
           (error
            (format nil
             "Cannot perform a load or store from partially defined type ~a"
             (to-str ts))))
         (assert (== (get-load-size partial-def) 4))
         (get-load-size partial-def))))))

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
  ;;(-> type-system? (or/c symbolp type?) symbolp boolean? typespec? boolean? method-info?)

  (if (stringp type-or-name)
      (declare-method-for-type this (lookup-type this (make-a-typespec this type-or-name))
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
                                &optional
                                  (id -1))
  ;;(->* (type-system? type? symbolp boolean? typespec? boolean?) (integer?) method-info?)
  ;; (printf "declare-method type: ~a name: ~a no-virt: ~a typespec: ~a override: ~a :id ~a~%"
  ;;        (to-str type)
  ;;        method-name
  ;;        no-virtual
  ;;        (to-str ts)
  ;;        override-type
  ;;        id)

  (cond
    ;; new methods
    ((== method-name "new")
     (when override-type
       (error "Cannot use :replace option with a new method."))
     (add-new-method this type ts))
    ;; other methods
    (else
     ;; look up the method
     (let ((existing-info (try-lookup-method this type method-name)))

       (cond
         (override-type
          ;; Allow overriding
          (when (not existing-info)
            (set! existing-info (try-lookup-method-by-id this (gtype-get-parent type) id))
            (unless (and (!= id -1) existing-info)
              (error
               (format
                nil
                "Cannot use :replace on method ~a of ~a because this method was not previously declared in a parent."
                method-name (gtype-name type)))))

          ;; use the existing ID.
          (gtype-add-method type (method-info-new (method-info-id existing-info)
                                                 method-name
                                                 ts
                                                 (gtype-name type)
                                                 no-virtual
                                                 t)))
         ;; Do not allow overriding
         (else
          (cond
            (existing-info
             ;; make sure we aren't changing anything.
             (let* ((exist-type (method-info-type existing-info))
                    (compatible (typespec-is-compatible-child-method exist-type ts (gtype-name type))))
               (when (not compatible)
                 (error (format
                         (string-append
                          "The method ~a of type ~a was originally declared as ~a, but has been "
                          "redeclared as ~a. Originally declared in ~a~%")
                         method-name
                         (gtype-name type)
                         (to-str exist-type)
                         (to-str ts)
                         (method-info-defined-in-type existing-info))))

               (when (and
                      (or (method-info-no-virtual existing-info) no-virtual)
                      (!= (method-info-defined-in-type existing-info) (gtype-name type)))
                 (error (format
                         nil
                         "Cannot define method ~a in type ~a when it was defined as no-virtual in parent type ~a"
                         method-name (gtype-name type) (method-info-defined-in-type existing-info))))
               (when (!= no-virtual (method-info-no-virtual existing-info))
                 (error (format
                         nil
                         "The method ~a of type ~a was originally declared with no-virtual = ~a, but has been redeclared as ~a"
                         method-name (gtype-name type) (method-info-no-virtual existing-info) no-virtual)))
               existing-info))
            (else
             ;; add a new method!
             (when (< id 0)
               (set! id (get-next-method-id this type)))
             (gtype-add-method
              type
              (method-info-new id
                               method-name
                               ts
                               (gtype-name type)
                               no-virtual
                               false))))))))))

;; Define method for type. The type can be as the name

(defun define-method (this type-or-name method-name ts)
  ;;(-> (or/c type? symbolp) symbolp typespec? method-info?)

   (if (stringp type-or-name)
       (define-method-for-type this (lookup-type this (typespec-new type-or-name)) method-name ts)
       (define-method-for-type this type-or-name method-name ts)))


;; Add a method, if it doesn't exist. If the method already exists (possibly in
;; a parent), checks to see if this is an identical definition. If not, it's an
;; error, and if so, nothing happens. Returns the info of either the existing or
;; newly created method.
;;
;; This is not used to override methods, but instead to create truly new
;; methods. The one exception is overriding the "new" method - the TypeSystem
;; will track that because overridden new methods may have different arguments.

(defun define-method-for-type (this type method-name ts)
  ;;(-> type-system? type? symbolp typespec? method-info?)
  (cond
    ((== method-name "new")
     (add-new-method this type ts))
    (else
     ;; look up the method
     (let ((existing-info (try-lookup-method this type method-name)))
       (cond
         (existing-info
          ;; make sure we aren't changing anything.
          (let ((mtype (method-info-type existing-info)))
            (when (not (typespec-is-compatible-child-method mtype ts (gtype-name type)))
              (error (format
                      nil
                      "The method ~a of type ~a was originally defined as ~a but has been redefined as ~a~%"
                      method-name (gtype-name type)
                      (to-str mtype)
                      (to-str ts)))))
          existing-info)
         (else
          (error (format nil "Cannot add method ~a to type ~a because it was not declared.~%"
                         method-name (gtype-name type)))))))))

;; Special case to add a new method, as new methods can specialize the
;; arguments. If it turns out that other child methods can specialize arguments
;; (seems like a bad idea), this may be generalized.

(defun add-new-method (this type ts)
  ;;(-> type-system? type? typespec? method-info?)
  (assert this)
  (let ((existing (gtype-get-my-new-method type)))
    (cond
      (existing
       ;; it exists!
       (let ((mtype (method-info-type existing)))
         (when (not (typespec-is-compatible-child-method mtype ts (gtype-name type)))
           (error (format
                   nil
                   (string-append
                    "Cannot add new method. Type does not match declaration. The new method of ~a was "
                    "originally defined as ~a, but has been redefined as ~a~%")
                   (gtype-name type)  (to-str existing) (to-str ts))))
         existing))
      (else
       (gtype-add-new-method type (method-info-new 0 :new ts (gtype-name type)))))))

;; ==============================================================================
;; Methid lookups
;; ==============================================================================

;; Lookup information on a method. Error if it can't be found. Will check parent
;; types if the given type doesn't specialize the method.

(defun lookup-method (type-name this method-name)
  ;;(-> symbolp symbolp method-info?)
  (cond
    ((== method-name "new")
     (lookup-new-method this type-name))
    (else

     ; first lookup the type
     (let ((iter-type (lookup-type this type-name)))
       (loop
         (let ((info (gtype-get-my-method iter-type method-name)))
           (cond
             (info (return info))
             (else
              (if (gtype-has-parent? iter-type)
                  (setf iter-type (lookup-type this (gtype-get-parent iter-type)))
                  (error (format
                          nil
                          "The method ~a of type ~a could not be found.~%"
                          method-name type-name)))))))))))

;; Try lookup method by the type and method name will not make an exeption

(defun try-lookup-method (this type-or-name method-name-or-id)
  ;;(-> type-system? (or/c type? symbolp) (or/c symbolp integer?) (or/c nil method-info?))
  (cond
    ((and (stringp type-or-name) (stringp method-name-or-id))
     (try-lookup-method-by-name this type-or-name method-name-or-id))
    ((and (stringp type-or-name) (integer? method-name-or-id))
     (try-lookup-method-by-id this type-or-name method-name-or-id))
    ((and (gtype-p type-or-name) (stringp method-name-or-id))
     (try-lookup-method-of-type this  type-or-name method-name-or-id))
    (else (error (format nil "Bad arguments~% [0] ~a~% [1] ~a~%" type-or-name method-name-or-id)))))

;; Lookup by name of type and method

(defun try-lookup-method-by-name (this type-name method-name)
  ;;(-> type-system? symbolp symbolp (or/c nil method-info?))
  (let ((type (types-find this type-name)))
    (cond
      (type
       (try-lookup-method this type method-name))
      (else
       ;; try to look up a forward declared type.
       (let ((fwd-dec-type (lookup-type-allow-partial-def this type-name)))
         (if (tc this
                 (typespec-new "basic")
                 (typespec-new fwd-dec-type))
             ;; only allow this for basics. It technically should be safe for structures as well.
             (try-lookup-method this fwd-dec-type method-name)
             nil))))))

;; Like lookup-method, but won't throw or print an error
;; when things go wrong.

(defun try-lookup-method-by-id (this type-name method-id)
  ;;(-> type-system? symbolp integer? (or/c nil method-info?))
  ;;
  (defun type-get-any-method-by-id (type method-id)
    (cond
      ((== method-id GOAL-NEW-METHOD)
       (gtype-get-my-new-method type))
      (else
       (gtype-get-my-method-by-id type method-id))))
  ;; look up the method
  (let ((iter-type (types-find this type-name)))
    (loop
      (cond
        ((not iter-type) nil)
        (else
         (let ((m (type-get-any-method-by-id iter-type method-id)))
           (cond
             ;; Case we found it
             (m (return m))
             ;; Did not found
             (else
              (if (gtype-has-parent? iter-type)
                  (setf iter-type (lookup-type this (gtype-get-parent iter-type)))
                  (return nil))))))))))

;;

(defun try-lookup-method-of-type (this type method-name)
  ;;(-> type-system? type? symbolp (or/c nil method-info?))
  (defun type-get-any-method (type name)
    (cond
      ((== name "new")
       (gtype-get-my-new-method type))
      (else
       (gtype-get-my-method type method-name))))
  ;; look up the method
  (let ((iter-type type))
    (loop
      (cond
        ((not iter-type) nil)
        (else
         (let ((m (type-get-any-method iter-type method-name)))
           (cond
             ;; Case we found it
             (m (return m))
             ;; Did not found
             (else
              (if (gtype-has-parent? iter-type)
                  (setf iter-type (lookup-type this (gtype-get-parent iter-type)))
                  nil)))))))))

;; Lookup information on a method by ID number. Error if it can't be found. Will
;; check parent types if the given type doesn't specialize the method.

(defun lookup-method-by-method-id (this type-name method-id)
  ;;(-> type-system? symbolp integer? method-info?)
  (cond
    ((== method-id GOAL-NEW-METHOD)
     (lookup-new-method this type-name))
    (else
     ;; first lookup the type
     (let ((type-iter (lookup-type this type-name)))
       (loop
         (let ((m (gtype-get-my-method type-iter method-id)))
           ;; look up the method
           (cond
             (m (return m))
             (else
              (if (gtype-has-parent? type-iter)
                  (setf type-iter (lookup-type this (gtype-get-parent type-iter)))
                  (error "Method with id ~a of type ~a could not be found."
                         method-id type-name))))))))))

;; Lookup information on a new method and get the most specialized version.

(defun lookup-new-method (this type-name)
  ;;(-> type-system? symbolp method-info?)
  ;; first lookup the type
  (let ((iter-type (lookup-type this type-name)))
    (loop
      ;; look up the method
      (let ((m (gtype-get-my-new-method iter-type)))
        (cond
          (m m)
          (else
           (if (gtype-has-parent? iter-type)
               (setf iter-type (lookup-type this (gtype-get-parent iter-type)))
               (error "The new method of type ~a could not be found.~%"
                      type-name))))))))

;; Makes sure a method exists at the given ID for the given type possibly
;; defined in a parent.

(defun assert-method-id (this type-name method-name id)
  ;;(-> type-system? symbolp symbolp integer? void?)
  (let ((info (lookup-method this type-name method-name)))
    (unless (== (method-info-id info) id)
      (error (format nil "Method ID assertion failed: type ~a, method ~a id was ~a, expected ~a~%"
                     type-name method-name (method-info-id info) id)))))

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
    (let ((info (gtype-get-my-last-method type)))
      (cons (if info (+ 1 (method-info-id info)) 1)
         (if (gtype-has-parent? type)
             (find-method-ids (types-find this (gtype-get-parent type)))
             nil))))
  (apply #'max (find-method-ids type)))

;; ==============================================================================
;; Field lookup
;; ==============================================================================

;; Field TypeSpec Boool Int

(defstruct field-lookup-info
  (field nil :type (or null field))
  (type nil :type (or null gtype))
  (needs-deref nil :type boolean)
  (array-size -1 :type integer))

;; Default constructor

(defun field-lookup-info-new ()
  ;;(-> field-lookup-info?)
  (make-field-lookup-info))

;; Lookup detailed information about a field of a type by name, including type,
;; offset, and how to access it.

(defun lookup-field-info (this type-name field-name)
;;(-> symbolp symbolp field-lookup-info?)
  (let* ((info (field-lookup-info-new))
         (field (lookup-field this type-name field-name)))
    (setf (field-lookup-info-field info) field)

    ;; get array size, for bounds checking (when possible)
    (when (and (field-is-array field) (not (field-is-dynamic field)))
      (setf (field-lookup-info-array-size info) (field-array-size field)))

    (let ((base-type (lookup-type-allow-partial-def this (field-type field)))
          (a-field-type (field-type field)))
      (cond ;; is-ref
        ((is-reference? base-type)
         (cond ;; is-inline
           ((field-is-inline? info)
            (cond ;; is-array
              ((field-is-array? field)
               ;; inline array of reference types
               (setf (field-lookup-info-needs-deref info) false)
               (setf (field-lookup-info-type info) (make-inline-array-typespec this a-field-type)))
              (else ;; is-array
               ;; inline object
               (setf (field-lookup-info-needs-deref info) false)
               (setf (field-lookup-info-type info) a-field-type))))
           (else ;; !is-inline
            (cond
              ((field-is-array field)
               (setf (field-lookup-info-needs-deref info) false)
               (setf (field-lookup-info-type info) (make-pointer-typespec this a-field-type)))
              (else
               (setf (field-lookup-info-needs-deref info) true)
               (setf (field-lookup-info-type info) a-field-type))))))
        (else ;; !is-ref
         (cond
           ((field-is-array? field)
            (setf (field-lookup-info-needs-deref info) false)
            (setf (field-lookup-info-type info) (make-pointer-typespec this a-field-type)))
           (else
            ;; not array
            (setf (field-lookup-info-needs-deref info) true)
            (setf (field-lookup-info-type info) a-field-type))))))
    info))

;; Make sure a field is located at the specified offset.

(defun assert-field-offset (this type-name field-name offset)
  ;;(-> symbolp symbolp integer? void?)
  (let ((field (lookup-field type-name this field-name)))
    (unless (== (field-offset field) offset)
      (error (format nil "assert-field-offset(~a, ~a, ~a) failed - got ~a~%"
                     type-name field-name offset (field-offset field))))))


; Add a field to a type. If offset_override is -1
;; (the default), will place it automatically.

(defun add-field-to-type (this
                          type
                          fld-name
                          fld-type
                         &optional
                           (is-inline nil)
                           (is-dynamic nil)
                           (array-size -1)
                           (offset-override -1)
                           (skip-in-static-decomp nil)
                           (score 0))
  ;;(->* (type-system? struct-type? symbolp typespec?)
  ;;     (boolean? boolean? integer? integer? boolean? real?)
  ;;     integer?)
  ;;(printf "Add field ~a to type ~a~%" fld-name (type-name type))
  (when (struct-type-lookup-field type fld-name)
    (error (format nil "Type `~a` already has a field named `~a`~%" (gtype-name type) fld-name)))

  ;; first, construct the field
  (let ((field (field-new fld-name fld-type 0)))
    (when is-inline
      (field-set-inline field))
    (when is-dynamic
      (field-set-dynamic field))
    (when (!= array-size -1)
      (field-set-array field array-size))
    ;;
    (let ((offset offset-override)
          (field-alignment (get-alignment-in-type this field)))
      ;; Compute offset
      (cond
        ((== offset -1)
         ;; we need to compute the offset ourself from the curent struct size
         (set! offset (align-n (struct-type-get-size-in-memory type) field-alignment)))
        (else
         ;; there was defined offset in the memory but mark this field as user-places
         (field-mark-as-user-placed field)
         ;; verify the offset if it was alligned
         (let ((aligned-offset (align-n offset field-alignment)))
           (when (!= offset aligned-offset)
             (error (format nil "Tried to place field `~a` at `~a`, but it is not aligned correctly~%"
                            fld-name offset))))))
      ;;
      (setf (field-offset field) offset)
      (setf (field-alignment field) field-alignment)
      (when skip-in-static-decomp
        (setf (field-skip-in-static-decomp field) t))

      (setf (field-field-score field) score)
      (let* ((field-size (get-size-in-type this field))
             (after-field (+ offset field-size)))
        (when (< (struct-type-get-size-in-memory type) after-field)
          (struct-type-override-size-in-memory type after-field))
        (struct-type-add-field type field (struct-type-get-size-in-memory type))
        offset))))

;; ==============================================================================
;; Lookup field
;; ==============================================================================

;; Lookup a field of a type by name

(defun lookup-field (this type-name field-name)
  ;;(-> symbolp symbolp field?)
  ;; next method will make exception in bad case
  (let* ((atype (get-type-of-type this #'struct-type-p type-name))
         (field (lookup-field this atype field-name)))
    (unless field
      (error (format nil "Type ~a has no field named ~a~%"
                     type-name field-name)))
    field))


;; ==============================================================================
;; Field alignement
;; ==============================================================================

;; Get the minimum required aligment of a field.

(defun get-alignment-in-type (this field)
  ;;(-> type-system? field? integer?)

  (let ((fld-type (lookup-type-allow-partial-def this (field-type field))))

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
           POINTER-SIZE)))))

(defun allow-inline (type)
  ;;(-> type? boolean?)
  (let ((name (gtype-name type)))
    (and (!= name "basic") (!= name "structure"))))

;; Get the size of a field in a type. The array sizes should be consistent with
;; get-deref-info's stride.

(defmethod get-size-in-type ((this type-system) (afield field))
  ;;(-> type-system? field? integer?)
  (let* ((fld-typespec (field-type afield))
         (fld-type (lookup-type-allow-partial-def this fld-typespec)))

    ;; Helper: Get size of field in case if it is array
    (defun get-size-for-array ()
      (cond
        ((field-is-inline afield)
         (when (not (fully-defined-type-exists this fld-typespec))
           (error (format nil "Cannot use the forward-declared type ~a in an inline array.~%"
                          (to-str fld-type))))
         (when (not (allow-inline fld-type))
           (error (format
                   nil
                   "Attempted to use `~a` inline, this probably isn't what you wanted.~%"
                   fld-typespec)))
         (assert (is-reference? fld-type))
         (* (field-array-size afield)
            (align-n (get-size-in-memory fld-type)
                     (get-inl-array-stride-align fld-type))))
        (else
         (if (is-reference? fld-type)
             (* (field-array-size afield) POINTER-SIZE)
             (* (field-array-size afield)
                (align-n (get-size-in-memory fld-type)
                       (get-in-memory-alignment fld-type)))))))
    ;; Helper: Get size for not arrays
    (defun get-size-for-not-array ()
      ;; not an array
      (cond
        ((field-is-inline afield)
         (when (not (fully-defined-type-exists this fld-typespec))
           (error (format nil "Cannot use the forward-declared type ~a inline.~%"
                          (to-str fld-type))))
         (when (not (allow-inline fld-type))
           (error (format
                   nil
                   "Attempted to use `~a` inline, this probably isn't what you wanted. Type may not be defined fully.~%"
                   (gtype-name fld-type))))
         (assert (is-reference? fld-type))
         ;; return align(field-type->get-size-in-memory(), field-type->get-in-memory-alignment());
         ;; looking at dead-pool-heap we tightly pack in this case
         (get-size-in-memory fld-type))
        (else
         (if (is-reference? fld-type)
             POINTER-SIZE
             (align-n (get-size-in-memory fld-type)
                      (get-in-memory-alignment fld-type))))))
    ;;
    (cond
      ((field-is-dynamic afield)
       0) ;; for dynamic fields zero
      ((field-is-array afield)
       (get-size-for-array))
      (else
       (get-size-for-not-array)))))

;; ==============================================================================
;; Type checking
;; ==============================================================================

;; Main compile-time type check!

(defun tc (this expected actual)
  ;;(-> type-system? typespec? typespec? boolean?)
  (typecheck-and-throw this expected actual "" nil nil nil))


;; Is actual of type expected? For base types.

(defun typecheck-base-types (this expected actual allow_alias)
  ;;(-> type-system? symbolp symbolp boolean? boolean?)
  ;;
  ;; The expected type should be found in the actual type tree
  ;;
  (defun find-same-type (this expected actual)
    ;;(-> type-system? symbolp symbolp boolean?)
    (if (== expected actual)
        ;; types already same
        t
        ;; find actual type and name
        (let* ((actual-type (lookup-type-allow-partial-def this actual))
               (actual-name (gtype-name actual-type)))
          (if (== expected actual-name)
              ;; expected and actual types are identical
              t
              ;; repeat this function for parent of actual type
              (if (gtype-has-parent? actual-type)
                  (find-same-type this expected (gtype-parent actual-type))
                  nil)))))
  ;;
  (progn
    ;; the unit types aren't picky.
    (cond
      ((== expected "meters")
       (set! expected "float"))
      ((== expected "seconds")
       (set! expected "time-frame"))
      ((== expected "degrees")
       (set!  expected "float")))
    (when (== actual "seconds")
      (set! actual "time-frame"))

    ;; the decompiler prefers no aliasing so it can detect casts properly
    (when allow_alias
      (when (== expected "time-frame")
        (set! expected "int"))
      (when (== actual "time-frame")
        (set! actual "int")))

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

(defun typecheck-and-throw (this
                            expect
                            actual
                            error-source-name
                            print-on-error
                            throw-on-error
                            allow-type-alias)
  ;;(-> type-system? typespec? typespec? string? boolean? boolean? boolean? void)
  (let ((success true))
    ;; first, typecheck the base types:
    (unless (typecheck-base-types this
                                  (base-type expect)
                                  (base-type actual)
                                  allow-type-alias)
      (setf success nil)

      ;; check the arguments
      (let ((exp-args-count (typespec-args-count expect))
            (act-args-count (typespec-args-count actual)))
        ;; next argument checks:
        (if (== exp-args-count act-args-count)
            ;; equal arguments count
            (let ((i 0))
              (loop
                (if (>= i exp-args-count)
                    (return t)
                    ;; don't print/throw because the error would be confusing. Better to fail only the
                    ;; outer most check and print a single error message.
                    (if (tc this (typespec-args-ref expect i) (typespec-args-ref actual i))
                        (setf i (+ 1 i))
                        (progn
                          (setf success nil)
                          (return))))))
            ;; different sizes of arguments.
            (if (== exp-args-count 0)
                ;; we expect zero arguments, but got some. The actual type is more specific, so this is fine.
                t
                ;; different sizes, and we expected arguments. No good!
                (setf success nil)))))

      ;; next, tag checks. It's fine to throw away tags, but the child must
      ;; match all parent tags
    (loop for tag in (typespec-tags expect)
          do
             (if (== (type-tag-name tag) "behavior")
                 ;; then
                 (let ((got (typespec-try-get-tag actual (type-tag-name tag))))
                   (if (not got)
                       ;; then
                       (setf success nil)
                       ;; else
                       (if (!= got (type-tag-value tag))
                           (setf success nil))))
                 ;; else
                 (error (format nil "Unknown tag ~a" (type-tag-name tag)))))

    (unless success
      (when print-on-error
        (if (== "" error-source-name)
            ;; then
            (format t "[TypeSystem] Got type \"~a\" when expecting \"~a\"~%"
                    (to-str actual) (to-str expect))
            ;; else
            (format t "[TypeSystem] For ~a, got type \"~a\" when expecting \"~a\"~%"
                    error-source-name (to-str actual) (to-str expect))))

      (when throw-on-error
        (error "typecheck failed")))
    success

    ))

;; ==============================================================================
;; Enum
;; ==============================================================================

;; Find enum

(defun try-enum-lookup (this type-or-name)
  ;;(-> type-system? (or/c typespec? symbolp) (or/c nil enum-type?))
  (when (typespec-p type-or-name)
    (set! type-or-name (base-type type-or-name)))
  (let ((type (types-find this type-or-name)))
    ;; the type is found
    (cond
      ((and type (enum-type-p type)) type)
      (else nil))))

;; ==============================================================================
;; The path of a type
;; ==============================================================================

;; Get a path from type to object.

(defmethod get-path-up-tree ((this type-system) (name string))
  ;;(-> type-system? symbolp (listof symbolp))
  (if (== name "object")
      (list "object")
      (let ((type (lookup-type-allow-partial-def this name)))
        (cons name (get-path-up-tree this (gtype-name type))))))

;; ==============================================================================
;; Common accessor
;; ==============================================================================

;; Lowest common ancestor of two base types.

(defun lca-base (this typea typeb)
  ;;(-> type-system? symbolp symbolp (or/c nil symbolp))
  (cond
    ((== typea typeb)
     typea)
    ((and (== "none" typea) (== "none" typeb))
     'none)
    (else
     ;; the list of classes will be
     (let ((a-up (get-path-up-tree this typea))
           (b-up (get-path-up-tree this typeb)))
       (let ((ai (1- (length a-up)))
             (bi (1- (length b-up))))
         (loop
           (if (and (>= ai 0) (>= bi 0))
               (let ((a (list-ref a-up ai))
                     (b (list-ref b-up bi)))
                 (if (equal? a b)
                     (return a)
                     (progn (setf ai (1- ai)) (setf bi (1- bi)))))
               (return nil))))))))


;; Lowest common ancestor of two typespecs. Will recursively apply to arguments,
;; if compatible.
;;
;; Otherwise arguments are stripped off.
;; In a situation like lca("(a b)", "(c d)"), the result will be
;; (lca(a, b) lca(b, d)).

(defun lowest-common-ancestor (this a b)
  ;;(-> type-system? typespec? typespec? typespec?)
  (let ((result (typespec-new (lca-base this (base-type a) (base-type b)))))
    (cond
      ((and (== result (typespec-new "function"))
            (== (typespec-args-count a) 2)
            (== (typespec-args-count b) 2)
            (or (== (typespec-args-ref a 0) (typespec-new "_varargs_"))
                (== (typespec-args-ref b 0) (typespec-new "_varargs_"))))
       (typespec-new "function"))
      (else
       (when (and (not (typespec-empty? a))
                  (not (typespec-empty? b))
                  (== (typespec-args-count a) (typespec-args-count b)))
         ;; recursively add arguments
         (dotimes (i (typespec-args-count a))
              (typespec-args-add result (lowest-common-ancestor this (typespec-args-ref a i) (typespec-args-ref b i)))))))
    result))

;; Lowest common ancestor of multiple (or at least one) type.

(defun lowest-common-ancestor-in-vector (this types)
  ;;(-> type-system? (vectorof typespec?) typespec?)
  (assert (!= 0 (arr-count types)))
  (cond
    ((== 1 (arr-count types))
     (arr-ref types 0))
    (else
     (let ((result (lowest-common-ancestor this
                                           (arr-ref types 0)
                                           (arr-ref types 1))))
       (loop for i from 2 below (arr-count types) by 1 do
         (setf result (lowest-common-ancestor this result (arr-ref types i))))
       result))))

;;
(defun lowest-common-ancestor-reg (this a b)
  ;;(-> typespec? typespec? typespec?)
  (coerce-to-reg-type (lowest-common-ancestor this a b)))

;; Converts a type in memory to the type you'll get in a register after loading it.

(defun coerce-to-reg-type (in)
  ;;(-> typespec? typespec?)
  (let ((bt (base-type in)))
    (when (== (typespec-args-count in) 0)
      (when (or (== bt "int8") (== bt "int16") (== bt "int32") (== bt "int64") (== bt "integer"))
        (typespec-new "int"))

      (when (or (== bt "uint8") (== bt "uint16") (== bt "uint32") (== bt "uint64") (== bt "uinteger"))
        (typespec-new "uint")))
    in))

;; ==============================================================================
;; Bitfield
;; ==============================================================================

;; Field TypeSpec Boool Int

(defstruct bitfield-lookup-info
  (result-type nil :type (or null gtype))
  (offset -1 :type integer)
  (size -1 :type integer)
  (sign-extend nil :type boolean)
  )

;; Default constructor

(defun bitfield-lookup-info-new ()
  ;;(-> field-lookup-info?)
  (make-bitfield-lookup-info))

;; Is the given type a bitfield type?

(defun is-bitfield-type (this type-name)
;;(-> type-system? symbolp boolean?)
  (get-type-of-type this #'bitfield-type-p type-name))

;; Get information about a field within a bitfield type.

(defun lookup-bitfield-info (this type-name field-name)
  ;;(-> type-system? symbolp symbolp bitfield-lookup-info?)
  (let* ((type (get-type-of-type this #'bitfield-type-p type-name))
         (fld (struct-type-lookup-field type field-name)))
    ;; (unless fld
    ;;   (error (format nil "Type ~a has no bitfield named ~a~%" type-name field-name)))
    (let* ((bf-type (sbitfield-type fld))
           (bf-base (lookup-type this (sbitfield-type fld))))

      (make-bitfield-lookup-info
       :result-type bf-type ; resutl type
       :offset (sbitfield-offset fld)
       :size (get-load-signed bf-base)
       :sign-extend (sbitfield-size fld)))))

;; Add a new field to a bitfield type. Set the field size to -1 if you want to
;; just use the size of the type and not clip it.

(defun add-field-to-bitfield (this type field-name field-type offset field-size skip-in-decomp)
  ;;(-> type-system? bitfield-type? symbol? typespec? integer? integer? boolean? void)
  ;; in bits
  (let ((load-size (* 8 (get-load-size (lookup-type this field-type)))))

    (when (== field-size -1)
      (set! field-size load-size))

    (when (> field-size load-size)
      (error (format
              nil
              "Type ~a's bitfield ~a's set size is ~a which is larger than the actual type: ~a~%"
              (gtype-name type) field-name field-size load-size)))

    (when (> (+ field-size offset) (* (get-load-size type) 8))
      (error (format
              nil
              "Type ~a's bitfield ~a will run off the end of the type (ends at ~a bits type is ~a bits)~%"
              (gtype-name type) field-name (+ field-size offset) (* (get-load-size type) 8))))

    ;; 128-bit bitfields have the limitation that fields cannot cross the 64-bit boundary.
    (when (and (< offset 64)
               (> (+ offset field-size) 64))
      (error (format
              nil
              "Type ~a's bitfield ~a will cross bit 64 which is not permitted. Range [~a ~a)"
              (gtype-name type) field-name offset (+ offset field-size))))

    (let ((field (sbitfield-new field-type field-name offset field-size skip-in-decomp)))
      (arr-push (bitfield-type-fields type) field))))



;; ==============================================================================
;; Virtual methods helpers
;; ==============================================================================

(defun should-use-virtual-methods (this type method-id)
  ;;(-> (or/c type? typespec?) integer? boolean?)
  (if (gtype-p type)
      (and
       (basic-type-p type)
       (not (basic-type-is-final type))
       (not (method-info-no-virtual (lookup-method this (gtype-name type) method-id))))
      (should-use-virtual-methods-ts this type method-id)))

(defun should-use-virtual-methods-ts (this ts method-id)
  ;;(-> typespec? integer? boolean?)
  (let ((it (types-find this (base-type ts))))
    (cond
      (it
        ;; it's a fully defined type
        (should-use-virtual-methods this it method-id))
      (else
       ;; it's a partially defined type.
       ;; for now we will prohibit calling a method on something that's defined only as a structure
       ;; because we don't know if it's actually a basic and should use virtual methods.
       (let ((fwd-dec-type (lookup-type-allow-partial-def this ts)))
         (when (== (gtype-name fwd-dec-type) "structure")
           (error (format
                   nil
                   "Type ~a was forward declared as structure and it is not safe to call a method."
                   (to-str ts))))
         (should-use-virtual-methods this fwd-dec-type method-id))))))

;; ==============================================================================
;; Generators
;; ==============================================================================

;; Generate the part of a deftype for the flag asserts and methods.
;; Doesn't include the final close paren of the deftype
;; This should work for both structure/bitfield definitions.

(defun generate-deftype-footer (this type)
  ;;(-> type-system? type? string?)
  (let ((result "")
        (method-count (get-next-method-id this type))
        (flags (type-flags-new))
        (methods-string "")
        (states-string ""))
    (defun result-append (str)
      (set! result (string-append result str)))

    (when (struct-type-p type)
      (when (struct-type-pack type)
        (result-append (format nil "  :pack-me~%")))
      (when (struct-type-allow-misalign type)
        (result-append (format nil "  :allow-misaligned~%")))
      (when (struct-type-always-stack-singleton type)
        (result-append (format nil "  :always-stack-singleton~%"))))

    (when (!= 0 (gtype-heap-base type))
      (result-append (format nil "  :heap-base #x(~x)~%" (gtype-heap-base type))))

    (result-append (format nil "  :method-count-assert ~a~%" (get-next-method-id this type)))
    (result-append (format nil "  :size-assert         #x(~x)~%" (get-size-in-memory type)))

    (setf (type-flags-heap-base flags) (gtype-heap-base type))
    (setf (type-flags-size flags)      (get-size-in-memory type))
    (setf (type-flags-methods flags)    method-count)

    (result-append (format nil "  :flag-assert         #x~x~%  " (type-flags-flag flags)))
    (unless (gtype-generate-inspect type)
      (result-append (format nil ":no-to-str~%  ")))

    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ;; Check and print the NEW method
    (let ((new-info (gtype-get-my-new-method type)))
      (when new-info
        (let* ((new-info-type (method-info-type new-info))
               (new-info-argc (typespec-args-count new-info-type)))
          (string-append! methods-string "(new (")
          (dotimes (i (1- new-info-argc))
               (string-append! methods-string (typespec-args-ref new-info-type i))
               (when (!= i (- new-info-argc 1))
                 (string-append! methods-string " ")))
          (string-append! methods-string (format nil ") ~a " (to-str (typespec-args-last new-info))))

          (let ((behavior (typespec-try-get-tag new-info-type "behavior")))
            (when behavior
              (string-append! methods-string (format nil ":behavior ~a " behavior)))
            (string-append! methods-string (format nil "0)~%    "))))))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ;; Print all other methods
    (loop for info across (gtype-methods type) do
      (string-append! methods-string (format nil "(~a (" (method-info-name info)))
      (let* ((info-type (method-info-type info))
             (info-argc (typespec-args-count info-type)))
        (dotimes (i (1- info-argc))
                (string-append! methods-string (to-str (typespec-args-ref info-type i)))
                (when (!= i (- info-argc 2))
                  (string-append! methods-string " ")))
        (string-append! methods-string (format nil ") ~a " (to-str (typespec-args-last info))))

        (when (method-info-no-virtual info)
          (string-append! methods-string ":no-virtual "))

        (when (method-info-override info)
          (string-append! methods-string ":replace "))

        (let ((behavior (typespec-try-get-tag info "behavior")))
          (when behavior
            (string-append! methods-string (format nil ":behavior ~a " behavior))))

        (when (== (base-type info-type) "state")
          (string-append! methods-string ":state "))

        (string-append! methods-string (format nil "~a)~%    " (method-info-id info)))))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (unless (== "" methods-string)
      (result-append "(:methods~%    ")
      (result-append methods-string)
      (result-append ")~%  "))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ;; Make states string
    (loop for name being the hash-keys in (gtype-states type) using (hash-value info)
          do
             (let ((info-argc (typespec-args-count info)))
               (cond
                 ((> info-argc 1)
                  (string-append! states-string (format nil "    (~a" (typespec-args-first info)))
                  (dotimes (i (1- info-argc))
                    (string-append! states-string " ")
                    (string-append! states-string (to-str (typespec-args-ref info i))))

                  (string-append! states-string ")~%"))
                 (else
                  (string-append! states-string (format nil "    ~a~%" name))))))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (unless (== "" states-string)
      (result-append "(:states~%")
      (result-append states-string)
      (result-append "    )~%  "))

    (result-append ")~%")
    result))

(defun generate-deftype-for-structure (this st)
  ;;(-> type-system? struct-type? string?)
  (assert this)
  (let ((result ""))
    (defun result-append (str)
      (set! result (string-append result str)))
    ;; - - - - - - - -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    (string-append! result
                    (format nil "(deftype ~a (~a)~%  ("
                            (gtype-name st) (gtype-parent st)))
    (let ((longest-field-name 0)
          (longest-type-name 0)
          (longest-mods 0)
          (inline-string ":inline")
          (dynamic-string ":dynamic")
          ;(user-offset-string ":offset")
          (has-offset-assert false))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ;; Check lenght names to make nice format in the next step
      (loop for i
              from (struct-type-idx-of-first-unique-field st)
                below (struct-type-fields-count st)
                  by 1
            do
               (let ((field (struct-type-fields-ref st i)))

                 (setf longest-field-name (max longest-field-name (length (field-name field))))
                 (setf longest-type-name (max longest-type-name (length (to-str (field-type field)))))

                 (let ((mods 0))
                   ;; mods are array size :inline :dynamic
                   (when (and (field-is-array field)
                              (not (field-is-dynamic field)))
                     (set! mods (+ mods (length (format nil "~a" (field-array-size field))))))

                   (when (field-is-inline field)
                     (when mods (1+! mods)) ; space
                     (+! mods (length inline-string)))

                   (when (field-is-dynamic field)
                     (when mods (1+! mods))  ;; space
                     (+! mods (length dynamic-string)))

                   (unless (field-is-user-placed field)
                     (set! has-offset-assert true))
                   (set! longest-mods (max longest-mods mods)))))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      (loop for i from (struct-type-idx-of-first-unique-field st)
              below (struct-type-fields-count st)
                by 1
            do
               (let ((field (struct-type-fields-ref st i)))
                 (string-append! result "(")
                 (string-append! result (field-name field))
                 (string-append! result (make-string (1+ (- longest-field-name (length (field-name field)))) :initial-element #\space))
                 (string-append! result (to-str (field-type field)))
                 (string-append! result (make-string (1+ (- longest-type-name (length (to-str (field-type field))))) :initial-element #\space))

                 (let ((mods ""))
                   (when (and (field-is-array field) (not (field-is-dynamic field)))
                     (string-append! mods (format nil "~a" (field-array-size field)))
                     (string-append! mods " "))

                   (when (field-is-inline field)
                     (string-append! mods inline-string)
                     (string-append! mods " "))

                   (when (field-is-dynamic field)
                     (string-append! mods dynamic-string)
                     (string-append! mods " "))

                   (result-append mods)
                   (result-append (make-string (- longest-mods (1- (length mods))) :initial-element #\space))

                   (cond
                     ((not (field-is-user-placed field))
                      (result-append ":offset-assert "))
                     (else
                      (if has-offset-assert
                          (result-append ":offset        ")
                          (result-append ":offset "))))

                   (result-append (format nil "~3a" (field-offset field)))
                   (result-append (format nil ")~%   ")))))

      (result-append (format nil ")~%"))
      (result-append (generate-deftype-footer this st))

      result)))

(defmethod generate-deftype-for-bitfield ((this type-system) (atype bitfield-type))
  ;;(-> type-system? bitfield-type? string?)

  (let* ((result ""))
    (defun result-append (str)
      (set! result (string-append result str)))

    (result-append (format nil "(deftype ~a (~a)~%  ("
                           (gtype-name atype) (gtype-parent atype)))
    (let ((longest-field-name 0)
          (longest-type-name 0))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ;; Check lenght names to make nice format in the next step
      (loop for field across (bitfield-type-fields atype)
            do
               (setf longest-field-name (max longest-field-name (length (field-name field))))
               (setf longest-type-name (max longest-type-name (length (field-type field)))))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      (loop for field across (bitfield-type-fields atype)
            do
               (result-append "(")
               (result-append (sbitfield-name field))
               (result-append (make-string (1+ (- longest-field-name (length (sbitfield-name field)))) :initial-element #\space))
               (result-append (to-str (sbitfield-type field)))
               (result-append (make-string (1+ (- longest-type-name (length (sbitfield-type field)))) :initial-element #\space))

               (result-append (format nil ":offset (~3a) :size (~3a)"
                                      (sbitfield-offset field)
                                      (sbitfield-size field)))
               (result-append (format ")~%   "))))

    (result-append (format nil ")~%"))
    (result-append (generate-deftype-footer this atype))
    result))
