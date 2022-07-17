#lang errortrace racket/base
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

(require syntax/parse
         racket/pretty
         racket/contract
         racket/list
         racket/lazy-require)

(require "vmc-lib.rkt")
(require "interfaces.rkt" "type.rkt" "basic-types.rkt" "type-system.rkt" "type-spec.rkt" "builting-types.rkt" "type-spec.rkt")

(lazy-require
 ["defenum.rkt"
  (parse-defenum)])

(provide parse-type-spec parse-deftype parse-declare-method)

;; ==============================================================================
;; Structures
;; ==============================================================================

;; The typedefinition result
;; (type-flags? type-spec? type? boolean?)
(define-struct deftype-res ( flags type type-info create-runtime-type ) #:mutable #:transparent)
(define (deftype-res-new)
  (deftype-res (type-flags-new) EMPTY-SYMBOL #f #t))
(define-with-struct-methods deftype-res- (type-flags size heap-base methods pad))

;; The structure definition result
(define-struct defstruct-res (flags generate-runtime-type pack-me allow-misaligned final always-stack-singleton)
  #:mutable #:transparent)
(define (defstruct-res-new)
  (defstruct-res (type-flags-new) #t #f #f #f #f))
(define-with-struct-methods defstruct-res- (type-flags size heap-base methods pad))

;; The bitfield definition result
(define-struct defbitfield-res (flags generate-runtime-type)
  #:mutable #:transparent)
(define (defbitfield-res-new)
  (defbitfield-res (type-flags-new) #t))
(define-with-struct-methods defbitfield-res- (type-flags size heap-base methods pad))

;; ==============================================================================
;; Helpers
;; ==============================================================================

(define/contract (get-int stx)
  (-> syntax? integer?)
  (define e (syntax-e stx))
  (if (integer? e) e (error (format "Expected integer value, found ~a" e))))

(define/contract (get-float stx)
  (-> syntax? integer?)
  (define e (syntax-e stx))
  (if (number? e) e (error (format "Expected floating point value, found ~a" e))))

(define/contract (get-symbol stx)
  (-> syntax? symbol?)
  (define e (syntax-e stx))
  (if (symbol? e) e (error (format "Expected a symbol value, found ~a" e))))

(define/contract (get-list stx)
  (-> syntax? list?)
  (define e (syntax-e stx))
  (if (list? e) e (error (format "Expected a list value, found ~a" e))))

(define/contract (is-type expected actual ts)
 (-> symbol? type-spec? type-system? boolean?)
  (tc ts (make-typespec ts expected) actual))

(define/contract (is-pair? stx)
 (-> syntax? boolean?)
  (pair? (syntax-e stx)))


;; ==============================================================================

;; Parse parent class list which is actualy have only one item

(define/contract (deftype-parent-list list)
  (-> (listof syntax?) symbol?)
  (unless (pair? list)
    (error (format "invalid parent list in deftype: ~a"  list)))
  (define parent (car list))
  (define rest (cdr list))
  (unless (null? rest)
    (error "invalid parent list in deftype - can only have one parent"))
  (unless (identifier? parent)
    (error "invalid parent in deftype parent list"))
  (syntax-e parent))

;; ==============================================================================
;; Add fiels
;; ==============================================================================

(define/contract (add-field structure ts def constants)
  (-> struct-type? type-system? (listof syntax?) hash? void?)
  ;(printf "Add field ~a\n" def)
  (define rest def)

  (define name (get-symbol (car rest)))
  (set! rest (cdr rest))

  (define type (parse-type-spec ts (car rest)))
  (set! rest (cdr rest))

  (define array-size -1)
  (define is-inline false)
  (define is-dynamic false)
  (define offset-override -1)
  (define offset-assert -1)
  (define score 0)
  (define skip-in-decomp false)

  (unless (null? rest)

    (cond
      ((integer? (syntax-e (car rest)))
       (set! array-size (get-int (car rest)))
       (set! rest (cdr rest)))
      ((symbol? (syntax-e (car rest)))
       (let ((key (get-symbol (car rest))))
         (when (hash-has-key? constants key)
           (set! array-size (hash-ref constants key))
           (set! rest (cdr rest))))))

    (let :loop: ()
      (unless (empty? rest)
        (define opt-name (get-symbol (car rest)))
        (set! rest (cdr rest))

        (cond
          ((== opt-name ':inline)
           (set! is-inline true))
          ((== opt-name ':dynamic)
           (set! is-dynamic true))
          ((== opt-name ':offset)
           (set! offset-override (get-int (car rest)))
           (set! rest (cdr rest)))
          ((== opt-name ':overlay-at)
           (define field-name (get-symbol (car rest)))
           (define overlay-field (struct-type-lookup-field structure field-name))
           (unless overlay-field
             (error
              (format "Field ~a not found to overlay for ~a" field-name name)))
           (set! offset-override (field-offset overlay-field))
           (set! rest (cdr rest)))
          ((== opt-name ':score)
           (set! score (get-float (car rest)))
           (set! rest (cdr rest)))
          ((== opt-name ':offset-assert)
           (set! offset-assert (get-int (car rest)))
           (when (== offset-assert -1)
             (error "Cannot use -1 as offset-assert"))
           (set! rest (cdr rest)))
          ((== opt-name ':do-not-decompile)
           (set! skip-in-decomp true))
          (else
           (error (format "Invalid option in field specification: ~a" opt-name))))
        (:loop:))))

  (define actual-offset (add-field-to-type ts
                                           structure name type is-inline is-dynamic
                                           array-size offset-override skip-in-decomp score))
  ;(printf "Added field to structure ~a\n" (inspect structure))
  (when (and (!= offset-assert -1)
             (!= actual-offset offset-assert))
    (error (format "Field ~a  was placed at ~a but offset-assert was set to ~a"
                   name  actual-offset offset-assert))))


;; ==============================================================================
;; Add Bitfield Type
;; ==============================================================================

(define/contract (add-bitfield this bitfield-type ts def)
  (-> type-system? bitfield-type? type-system? (listof syntax?) void?)
  (log-debug "add-bitfield\n~a\n\n" def)
  (define rest def)
  (define name (get-symbol (car rest)))
  (set! rest (cdr rest))

  (define type (parse-type-spec ts (car rest)))
  (set! rest (cdr rest))

  (define offset-override -1)
  (define size-override -1)
  (define skip-in-decomp false)

  (let :loop: ()
    (unless (empty? rest)
      (define opt-name (get-symbol (car rest)))
      (set! rest (cdr rest))
      (cond
        ((== opt-name ':offset)
         (set! offset-override (get-int (car rest)))
         (set! rest (cdr rest)))
        ((== opt-name ':size)
         (set! size-override (get-int (car rest)))
         (set! rest (cdr rest)))
        ((== opt-name ':do-not-decompile)
         (set! skip-in-decomp true))
        (else
         (error (format "Invalid option in field specification: ~a" opt-name))))
      (:loop:)))

  (when (== offset-override -1)
    (error "Bitfield type must manually specify offsets always"))

  ;; it's fine if the size is -1, that means it'll just use the type's size.
  (add-field-to-bitfield this bitfield-type name type offset-override size-override skip-in-decomp))

;; ==============================================================================
;; Declare Method
;;
;; (defmethod square is-same-shape ((obj1 square) (obj2 square))
;;  (= (-> obj1 side-length) (-> obj2 side-length))
;;  )
;; ==============================================================================

(define/contract (parse-declare-method type ts def)
  (-> type? type-system? (listof syntax?) void?)
  ;; - -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
  ;; helper function to check if the object is a tag name
  (define (is-tag it tag)
    (cond
      ((null? it) #f)
      ((symbol? it) (== tag it))
      ((pair? it) (is-tag (car it) tag))
      ((syntax? it) (is-tag (syntax-e it) tag))
      (else #f)))
  (define (is-integer? it)
    (cond
      ((null? it) #f)
      ((integer? it) #t)
      ((syntax? it) (is-integer? (syntax->datum it)))
      ((pair? it) (is-integer? (car it)))
      (else #f)))
  ;; - -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
  (log-debug "parse-declare-method\n~a\n\n" def)
  (for ((obj-stx (in-list def)))
    (define obj (syntax-e obj-stx))

      ;; name args return-type [:no-virtual] [:replace] [:state] [id])
      (define method-name (get-symbol (car obj)))
      (set! obj (cdr obj))
      (define args (car obj))
      (set! obj (cdr obj))
      (define return-type (car obj))
      (set! obj (cdr obj))

      (define no-virtual false)
      (define replace-method false)
      (define function-typespec (type-spec-new 'function))

      (when (is-tag obj ':no-virtual)
        (set! obj (cdr obj))
        (set! no-virtual true))

      (when (is-tag  obj ':replace)
        (set! obj (cdr obj))
        (set! replace-method true))

      (when (is-tag obj ':state)
        (set! obj (cdr obj))
        (set! function-typespec (type-spec-new 'state)))

      (when (is-tag obj ':behavior)
        (set! obj (cdr obj))
        (type-spec-add-new-tag function-typespec 'behavior (get-symbol (car obj)))
        (set! obj (cdr obj)))

      (define id -1)
      (when (is-integer? obj)
        (set! id (get-int (car obj)))
        (set! obj (cdr obj)))

      (unless (null? obj)
        (error (format "too many things in method definition ~a, was unexpeced: ~a" def obj)))

      (for ((o (in-list (syntax-e args))))
        (type-spec-args-add function-typespec
                            (parse-type-spec ts o)))
      (type-spec-args-add function-typespec
                          (parse-type-spec ts return-type))

      (define info (declare-method-for-type ts
                                            type
                                            method-name
                                            no-virtual
                                            function-typespec
                                            replace-method
                                            id))
      ;; check the method assert
      (when (!= id -1)
        ;; method id assert!
        (when (!= id (method-info-id info))
          (error
           (format "Method ID failed -  method ~a of type ~a (wanted ~a got ~a)\n"
                   method-name (type-name type) id (method-info-id info)))))))


;; ==============================================================================
;; Declare State
;; ==============================================================================

(define/contract (declare-state type type-system def)
  (-> type? type-system? (listof syntax?) void?)

  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (define (declare-item obj)
    (cond
      ((list? obj)
       ;; (name ,@args)
       (define state-name (get-symbol (car obj)))
       (define args (cdr obj))

       (define state-typespec (type-spec-new 'state))

       (for-each-in-list args (lambda (o)
                                (type-spec-args-add state-typespec (parse-type-spec type-system o))))

       (type-spec-args-add state-typespec (type-spec-new (type-name type)))

       (type-add-state type state-name state-typespec))
      (else
       ;; name
       (define state-name (if (symbol? obj) obj (get-symbol obj)))

       (define state-typespec (type-spec-new 'state))
       (type-spec-args-add state-typespec (type-spec-new (type-name type)))

       (type-add-state type state-name state-typespec))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (for-each-in-list def (lambda (it) (declare-item (syntax-e it)))))

;; ==============================================================================
;; Parse Structur
;; ==============================================================================

(define/contract (parse-structure-def type ts fields options constants)
(-> struct-type? type-system? (listof syntax?) (listof syntax?) hash? defstruct-res?)

  (define result (defstruct-res-new))
  (for-each-in-list fields (lambda (o) (add-field type ts (syntax-e o) constants)))
  (define flags (type-flags-new))
  (set-type-flags-size! flags (get-size-in-memory type))

  (define size-assert  -1)
  (define method-count-assert -1)
  (define flag-assert 0)
  (define flag-assert-set false)
  (define set-heapbase false)

  (let :loop: ((rest options))
    (unless (empty? rest)
      (cond
        ((pair? (syntax-e (car rest)))
         (define opt-list (syntax-e (car rest)))
         (define first (car opt-list))
         (set! opt-list (cdr opt-list))
         (define list-name (get-symbol first))
         (cond ((== list-name ':methods)
                (parse-declare-method type ts opt-list))
               ((== list-name ':states)
                (declare-state type ts opt-list))
               (else
                (error (format "Invalid option list in field specification: ~a" (car rest)))))
         (set! rest (cdr rest)))
        (else
         (define opt-name (get-symbol (car rest)))
         (set! rest (cdr rest))
         (cond
           ((== opt-name ':size-assert)
            (set! size-assert (get-int (car rest)))
            (when (== size-assert -1)
              (error "Cannot use -1 as size-assert"))
            (set! rest (cdr rest)))
           ((== opt-name ':method-count-assert)
            (set! method-count-assert (get-int (car rest)))
            (when (== method-count-assert -1)
              (error "Cannot use -1 as method-count-assert"))
            (set! rest (cdr rest)))
           ((== opt-name ':flag-assert)
            (set! flag-assert (get-int (car rest)))
            (set! flag-assert-set true)
            (set! rest (cdr rest)))
           ((== opt-name ':no-runtime-type)
            (set-defstruct-res-generate-runtime-type! result false))
           ((== opt-name ':no-inspect)
            (set-type-generate-inspect! type false))
           ((== opt-name ':pack-me)
            (set-defstruct-res-pack-me! result  true))
           ((== opt-name ':heap-base)
            (define hb (get-int (car rest)))
            (when (!= (% hb #x10) 0)
              (error "heap-base is not 16-byte aligned"))
            (set! rest (cdr rest))
            (set-type-flags-heap-base! flags hb)
            (set! set-heapbase true))
           ((== opt-name ':allow-misaligned)
            (set-defstruct-res-allow-misaligned! result true))
           ((== opt-name ':final)
            (set-defstruct-res-final! result true))
           ((== opt-name ':always-stack-singleton)
            (set-defstruct-res-always-stack-singleton! result true))
           (else
            (error (format "Invalid option in field specification: ~a" opt-name))))))
      (:loop: rest)))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (when (and (fully-defined-type-exists ts (type-spec-new 'process))
             (tc ts (type-spec-new 'process) (type-spec-new (type-parent type))))
    ;; check heap-base if this is a child of process.
    (define process-type (get-type-of-type ts basic-type? 'process))
    (define auto-hb (align (- (type-flags-size flags) (struct-type-size-in-mem  process-type))
                           16))

    (cond
      ((not set-heapbase)
       ;; wasnt set manually so set (definematically.
       (set-type-flags-heap-base! flags auto-hb))
      ((< (type-flags-heap-base flags) auto-hb)
       ;; was set manually so verify if that's correct.
       (error
        (format "Process heap underflow in type ~a heap-base is ~a vs. define-detected ~a"
                (type-name type) (type-flags-heap-base flags) auto-hb)))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (when (and (!= size-assert -1)
             (!= (type-flags-size flags) (bitwise-and #xFFFF size-assert)))
    (error (format "Type ~a came out to size ~a but size-assert was set to ~a"
           (type-name type) (type-flags-size flags) size-assert)))

  (set-type-flags-methods! flags (get-next-method-id ts type))

  (when (and (!= method-count-assert -1)
             (!= (type-flags-methods flags) (bitwise-and #xFFFF method-count-assert)))
    (error
     (format
      "Type ~a has ~a  methods, but method-count-assert was set to ~a"
      (type-name type) (type-flags-methods flags) method-count-assert)))

  (when (and flag-assert-set (!= (type-flags-flag flags) flag-assert))
    (error
     (format "Type ~a has flag 0x~a but flag-assert was set to 0x~a)"
             (type-name type)
             (integer->hex (type-flags-flag flags))
             (integer->hex flag-assert))))

  (set-defstruct-res-flags! result flags)
  result)

;; ==============================================================================

;; ==============================================================================


(define/contract (parse-bitfield-type-def type ts fields options)
  (-> bitfield-type? type-system? (listof syntax?) (listof syntax?) defbitfield-res?)
  (log-debug "parse-bitfield:\n~a\n\n" fields)
  (define result (defbitfield-res-new))

  (for-each-in-list fields (lambda (o) (add-bitfield ts type ts (syntax-e o))))
  (define flags (type-flags-new))
  (set-type-flags-size! flags (get-size-in-memory type))

  (define size-assert -1)
  (define method-count-assert -1)
  (define flag-assert 0)
  (define flag-assert-set false)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (let :loop: ((rest options))
    (unless (null? rest)
      (cond
        ((is-pair? (car rest))
         (define opt-list (syntax-e (car rest)))
         (define first (car opt-list))
         (set! opt-list (cdr opt-list))
         (if (== (get-symbol first) ':methods)
             (parse-declare-method type ts opt-list)
             (error (format "Invalid option list in field specification: ~a" (car rest))))
         (set! rest (cdr rest)))
        (else 
         (define opt-name (get-symbol (car rest)))
         (set! rest (cdr rest))
         (cond
           ((== opt-name ':size-assert)
            (set! size-assert (get-int (car rest)))
            (when (== size-assert -1)
              (error "Cannot use -1 as size-assert"))
            (set! rest (cdr rest)))
           ((== opt-name ':method-count-assert)
            (set! method-count-assert (get-int (car rest)))
            (when (== method-count-assert -1)
              (error "Cannot use -1 as method-count-assert"))
            (set! rest (cdr rest)))
           ((== opt-name ':flag-assert)
            (set! flag-assert (get-int (car rest)))
            (set! flag-assert-set true)
            (set! rest (cdr rest)))
           ((== opt-name ':no-runtime-type)
             (set-defbitfield-res-generate-runtime-type! result false))
           ((== opt-name ':no-inspect)
            (set-type-generate-inspect! type false))
           (else
            (error (format "Invalid option in field specification: ~a" opt-name))))))
      (:loop: rest)))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (when (and (!= size-assert -1)
             (!= (type-flags-size flags) size-assert))
    (error (format "Type ~a came out to size ~a but size-assert was set to ~a"
                   (type-name type) (type-flags-size flags) size-assert)))

  (set-type-flags-methods! flags (get-next-method-id ts type))

  (when (and (!= method-count-assert -1)
             (!= (type-flags-methods flags) method-count-assert))
    (error
     "Type ~a has ~a methods, but method-count-assert was set to ~a"
     (type-name type) (type-flags-methods flags) method-count-assert))

  (when (and flag-assert-set
             (!= (type-flags-flag flags) flag-assert))
    (error 'deftype
     (format "Type ~a has flag 0x~a but flag-assert was set to 0x~a"
             (type-name type)
             (integer->hex (type-flags-flag flags))
             (integer->hex flag-assert))))

  (set-defbitfield-res-flags! result flags)
  result)



;; ==============================================================================
;; Parse the type spec
;; ==============================================================================

;; The typespec is a list of items
;; (type1 type2 (type3 type4) :tag1 val1 tag2 val2)
(define/contract (parse-type-spec type-sys stx)
  (-> type-system? syntax? type-spec?)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Check in the expression is a tag's key by the first character
  (define (is-tag? stx)
    (let ((s (syntax->datum stx)))
      (and (symbol? s) (equal? #\: (string-ref (symbol->string s) 0)))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;;
  (let ((e (syntax-e stx)))
    (cond
      ((symbol? e)
       ;; parse -only the type name
       (make-typespec type-sys e))
      ((list? e)
       ;; parse - the list if elements as children types or tags
       ;; make a type
       (define ts (make-typespec type-sys (syntax->datum (car e))))
       (define tag-name #f)
       (define tag-val #f)
       ;; parse rest of items
       (let :loop: ((rest (cdr e)))
         (unless (null? rest)
           (let ((it (car rest)))
             (cond
               ((is-tag? it)
                (set! tag-name (get-symbol it))
                (set! rest (cdr rest))

                (when (null? rest)
                  (error "TypeSpec missing tag value"))
                (set! tag-val (car rest))

                (cond
                  ((== tag-name ':behavior)
                   (define val (get-symbol tag-val))
                   (when (and (not (fully-defined-type-exists type-sys val))
                              (not (partially-defined-type-exists type-sys val)))
                     (error (format "Behavior tag uses an unknown type ~a" val)))
                   (type-spec-add-new-tag ts tag-name val))
                  (else
                   (error (format "Type tag ~a is unknown" tag-name)))))
               (else
                ;; normal argument.
                (type-spec-args-add ts (parse-type-spec type-sys it))))))
         (unless (null? rest) (:loop: (cdr rest))))
       ts)
      (else
       (error (format "invalid typespec: ~a" stx))))))

;; Test for parse the type spec -------------------------------------------------

(module+ test
  (require rackunit)
  (let ((tsys (type-system-new)))
    (add-builtin-types tsys)
    (let ((ts (parse-type-spec tsys #'int)))
      (check-equal? (inspect ts) "int"))

    (let ((ts (parse-type-spec tsys #'(int (int int) :behavior int))))
      (display (inspect ts))
      (check-equal? (inspect ts) "(int (int int) :behavior int)"))))

;; ==============================================================================
;; Deftype
;; ==============================================================================

;; The constant list is the hash table
(define/contract (parse-deftype deftype ts constants)
  (-> (listof syntax?) type-system? (or/c #f hash?) deftype-res?)

  (define no-consts (make-hash))
  (define constants-to-use no-consts)
  (when constants
    (set! constants-to-use constants))

  (define iter deftype)

  (define type-name-obj (car iter))
  (set! iter (cdr iter))
  (define parent-list-obj (car iter))
  (set! iter (cdr iter))
  (define field-list-obj (syntax-e (car iter)))
  (set! iter (cdr iter))
  (define options-obj iter)

  (unless (identifier? type-name-obj)
    (error "deftype must be given a symbol as the type name"))

  (define name (get-symbol type-name-obj))
  (define parent-type-name (deftype-parent-list (get-list parent-list-obj)))
  (define parent-type (make-typespec ts parent-type-name))
  (define result (deftype-res-new))

  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (define (parse-basic-internal)
    (define new-type (basic-type-new parent-type-name name false 0))
    (define pto (lookup-type ts parent-type))
    (assert pto);
    (when (basic-type-is-final pto)
      (error
       (format "[TypeSystem] Cannot make a child type ~a of final basic type ~a"
               name parent-type-name)))
    (struct-type-inherit new-type pto)
    (forward-declare-type-as ts name 'basic)
    ;; returns StructureDefResult
    (define sr (parse-structure-def new-type ts field-list-obj options-obj constants-to-use))
    (define-with-struct sr. (defstruct-res flags
                              generate-runtime-type
                              pack-me
                              allow-misaligned
                              final
                              always-stack-singleton) sr)

    (set-deftype-res-flags! result sr.flags)
    (set-deftype-res-create-runtime-type! result sr.generate-runtime-type)
    (when sr.pack-me
      (set-struct-type-pack! new-type true))
    (when sr.allow-misaligned
      (error (format "[TypeSystem] invalid pack option on basic : :allow-misaligned was set on ~a, which is a basic and cannot be misaligned\n"
                     name)))
    (when sr.always-stack-singleton
      (error (format "[TypeSystem] invalid stack singleton option on basic : :always-stack-singleton was set on ~a, which is a basic and cannot be a stack singleton\n"                     name)))

    (set-type-heap-base! new-type (type-flags-heap-base (deftype-res-flags result)))
    (when sr.final
      (set-basic-type-is-final! new-type true))
    (add-type ts name new-type))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (define (parse-struct-internal)
    (define new-type (struct-type-new parent-type-name name false false false 0))
    (define pto (lookup-type ts parent-type))
    (assert pto)
    (struct-type-inherit new-type pto)
    (forward-declare-type-as ts name 'structure)
    (define sr (parse-structure-def new-type ts field-list-obj options-obj constants-to-use))
    (define-with-struct sr. (defstruct-res
                              flags
                              generate-runtime-type
                              pack-me
                              allow-misaligned
                              final
                              always-stack-singleton) sr)
    (set-deftype-res-flags! result sr.flags)
    (set-deftype-res-create-runtime-type! result sr.generate-runtime-type)
    (when sr.pack-me
      (set-struct-type-pack! new-type true))

    (when sr.allow-misaligned
      (set-struct-type-allow-misalign! new-type true))

    (when sr.always-stack-singleton
      (set-struct-type-always-stack-singleton! new-type true))

    (when sr.final
      (error (format "[TypeSystem] :final option cannot be used on structure type ~a" name)))

    (set-type-heap-base! new-type (type-flags-heap-base sr.flags))
    (add-type ts name new-type))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (define (parse-integer-internal)
    (define pto (lookup-type ts parent-type))
    (assert pto);
    (define new-type (bitfield-type-new parent-type-name name (get-size-in-memory pto) (get-load-signed pto)))
    (assert (value-type? pto))
    (value-type-inherit new-type pto)
    (set-type-runtime-name! new-type (type-runtime-name pto))
    (define sr (parse-bitfield-type-def new-type ts field-list-obj options-obj))
    (set-deftype-res-flags! result (defbitfield-res-flags sr))
    (set-deftype-res-create-runtime-type! result (defbitfield-res-generate-runtime-type sr))
    (add-type ts name new-type))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (cond
    ((is-type 'basic parent-type ts)
     (parse-basic-internal))
    ((is-type 'structure parent-type ts)
     (parse-struct-internal))
    ((is-type 'integer parent-type ts)
     (parse-integer-internal))
    (else
     (error (format "Creating a child type from ~a is not allowed or not supported yet."
                    (inspect parent-type)) deftype #f (list #'loc))))
  (set-deftype-res-type! result (make-typespec ts name))
  (set-deftype-res-type-info! result (lookup-type ts (deftype-res-type result)))
  result)

;; Used only for tests! Becayse passing the test 'goalc-all-types.gc'
;; requires to declare type

(define/contract (parse-declare-type rest ts)
  (-> (listof syntax?) type-system? void)
  (define type-name (get-symbol (car rest)))
  (define kind (get-symbol (cadr rest)))
  (forward-declare-type-as ts type-name kind))

;; Test for parse the type spec -------------------------------------------------

(define (my-error-handler e)
  (raise (exn:fail:syntax
          (exn-message e)
          (exn-continuation-marks e)
          (list #'here))))

(module+ test
  (require rackunit)
  (let ((tsys (type-system-new)))
    (add-builtin-types tsys)
    ;; Large test
    (printf "\n[START] Deftype parsing test\n")
    (printf "Reading file...\n")
    (define expr (for-file-parse "goalc-all-types.gc"))
    (printf "Parsign deftypes...\n")
    (define deftype-cnt 0)
    (define defenum-cnt 0)
    (define constants (make-hash))
    (for-each-in-list
     expr
     (lambda (e)
       (define fe (syntax-e e))
       (when (list? fe)
         (define first (syntax-e (car fe)))
         (with-handlers
           ([exn:fail?
             (lambda (ex)
               (raise (exn:fail:syntax
                       (exn-message ex)
                       (exn-continuation-marks ex)
                       (list #'loc e))))])
           (cond
             ((== first 'declare-type)
              (printf "Parse ~a...\n" (syntax->datum (cadr fe)))
              (parse-declare-type (cdr fe) tsys)
              (+1! deftype-cnt))
             ((== first 'deftype)
              (printf "Parse ~a...\n" (syntax->datum (cadr fe)))
              (parse-deftype (cdr fe) tsys constants)
              (+1! deftype-cnt))
             ((== first 'defenum)
              (printf "Parse ~a...\n" (syntax->datum (cadr fe)))
              (parse-defenum (cdr fe) tsys)
              (+1! defenum-cnt)))))))
    (printf "[END] ~a deftypes ~a defenum parsed\n" deftype-cnt defenum-cnt)))
