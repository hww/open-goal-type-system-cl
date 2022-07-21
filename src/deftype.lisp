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
;; Structures
;; ==============================================================================

;; ----------------------------------------------------------------------------
;; Utility struct used by the parser
;; ----------------------------------------------------------------------------

;; Flags for any definition
;; All fields are integers
(defstruct type-flags
  (size      0 :type integer)
  (heap-base 0 :type integer)
  (methods   0 :type integer)
  (pad       0 :type integer))

(defun type-flags-flag (flags)
  "Collect all flags to a single 64 bits word"
  (my/with-slots nil (type-flags size heap-base methods pad) flags
    (+ (logand size      #xFFFF)
       (ash (logand heap-base #xFFFF) 16)
       (ash (logand methods   #xFFFF) 32)
       (ash (logand pad       #xFFFF) 48))))

(defun type-flags-copy (dst src)
  (setf (type-flags-size dst) (type-flags-size src))
  (setf (type-flags-heap-base dst) (type-flags-heap-base src))
  (setf (type-flags-methods dst) (type-flags-methods src))
  (setf (type-flags-pad dst) (type-flags-pad src)))


;; The typedefinition result
;; (type-flags? type-spec? type? boolean?)
(defstruct (deftype-res (:include type-flags))
           (type nil :type (or null string))
           (type-info nil :type (or null typespec))
           (create-runtime-type t :type boolean))

;; The structure definition result
(defstruct (defstruct-res (:include type-flags))
           (generate-runtime-type t :type boolean)
           (pack-me nil :type boolean)
           (allow-misaligned nil :type boolean)
           (final nil :type boolean)
           (always-stack-singleton nil :type boolean))

;; The bitfield definition result
(defstruct (defbitfield-res (:include type-flags))
           (generate-runtime-type t :type boolean))

;; ==============================================================================
;; Helpers
;; ==============================================================================

(defun get-int (e)
  ;;(-> syntax? integer?)
  (if (integer? e) e (error (format nil "Expected integer value, found ~a" e))))

(defun get-float (e)
  ;;(-> syntax? integer?)
  (if (number? e) e (error (format nil "Expected floating point value, found ~a" e))))

(defun get-symbol (e)
  ;;(-> syntax? symbol?)
  (cond ((string? e) e)
        ((symbol? e) (symbol-name e))
        (else (error (format nil "Expected a symbol value, found ~a" e)))))

(defun get-list (e)
  ;;(-> syntax? list?)
  (if (list? e) e (error (format nil "Expected a list value, found ~a" e))))

(defun is-type (this expected actual)
 ;;(-> symbol? type-spec? type-system? boolean?)
  (tc this (make-a-typespec this expected) actual))

;; ==============================================================================

;; Parse parent class list which is actualy have only one item

(defun deftype-parent-list (list)
  "Parsing a parrent type list"
  ;;(-> (listof syntax?) symbol?)
  (unless (list? list)
    (error (format nil "invalid parent list in deftype: ~a"  list)))
  (let ((name (car list))
        (rest (cdr list)))
    (unless (null? rest)
      (error "invalid parent list in deftype - can only have one parent"))
    (cond
      ((symbol? name) (symbol-name name))
      ((string? name) name)
      (else
       (error "invalid parent in deftype parent list")))))

;; ==============================================================================
;; Add fiels
;; ==============================================================================

(defun add-field (structure ts def constants)
  ;;(-> struct-type? type-system? (listof syntax?) hash? void?)
                                        ;(printf "Add field ~a~%" def)

  (let* ((name (get-symbol (car def)))
         (type (parse-typespec ts (cadr def)))
         (rest (cddr def))
         (array-size -1)
         (is-inline false)
         (is-dynamic false)
         (offset-override -1)
         (offset-assert -1)
         (score 0)
         (skip-in-decomp false))

    (unless (null? rest)

      (cond
        ((integer? (car rest))
         (setf array-size (get-int (car rest)))
         (setf rest (cdr rest)))
        ((symbol? (car rest))
         (let ((key (get-symbol (car rest))))
           (when (hash-has-key? constants key)
             (setf array-size (hash-ref constants key))
             (setf rest (cdr rest))))))

      (loop
        (when (null? rest)
          (return))
        (let ((opt-name (get-symbol (car rest))))
          (setf rest (cdr rest))

          (cond
            ((== opt-name ":inline")
             (setf is-inline true))
            ((== opt-name ":dynamic")
             (setf is-dynamic true))
            ((== opt-name ":offset")
             (setf offset-override (get-int (car rest)))
             (setf rest (cdr rest)))
            ((== opt-name ":overlay-at")
             (let* ((field-name (get-symbol (car rest)))
                    (overlay-field (struct-type-lookup-field structure field-name)))
               (unless overlay-field
                 (error
                  (format nil "Field ~a not found to overlay for ~a" field-name name)))
               (setf offset-override (field-offset overlay-field))
               (setf rest (cdr rest))))
            ((== opt-name ":score")
             (setf score (get-float (car rest)))
             (setf rest (cdr rest)))
            ((== opt-name ":offset-assert")
             (setf offset-assert (get-int (car rest)))
             (when (== offset-assert -1)
               (error "Cannot use -1 as offset-assert"))
             (setf rest (cdr rest)))
            ((== opt-name ':do-not-decompile)
             (setf skip-in-decomp true))
            (else
             (error (format nil "Invalid option in field specification: ~a" opt-name))))))

      (let ((actual-offset (add-field-to-type ts
                                              structure name type is-inline is-dynamic
                                              array-size offset-override skip-in-decomp score)))
                                        ;(printf "Added field to structure ~a~%" (inspect structure))
        (when (and (!= offset-assert -1)
                   (!= actual-offset offset-assert))
          (error (format nil "Field ~a  was placed at ~a but offset-assert was set to ~a"
                         name  actual-offset offset-assert)))))))


;; ==============================================================================
;; Add Bitfield Type
;; ==============================================================================

(defun add-bitfield (this bitfield-type def)
  ;;(-> type-system? bitfield-type?  (listof syntax?) void?)
  (log-debug "add-bitfield~%~a~%~%" def)

  (let ((name (get-symbol (car def)))
        (type (parse-typespec this (cadr def)))
        (rest (cddr def))
        (offset-override -1)
        (size-override -1)
        (skip-in-decomp false))

    (loop
      (when (null rest)
        (return))
      (let ((opt-name (get-symbol (car rest))))
        (set! rest (cdr rest))
        (cond
          ((== opt-name ":offset")
           (setf offset-override (get-int (car rest)))
           (setf rest (cdr rest)))
          ((== opt-name ":size")
           (setf size-override (get-int (car rest)))
           (setf rest (cdr rest)))
          ((== opt-name ":do-not-decompile")
           (setf skip-in-decomp true))
          (else
           (error (format nil "Invalid option in field specification: ~a" opt-name))))))

    (when (== offset-override -1)
      (error "Bitfield type must manually specify offsets always"))

    ;; it's fine if the size is -1, that means it'll just use the type's size.
    (add-field-to-bitfield this bitfield-type name type offset-override size-override skip-in-decomp)))

;; ==============================================================================
;; Declare Method
;;
;; (defmethod square is-same-shape ((obj1 square) (obj2 square))
;;  (= ;;(-> obj1 side-length) ;;(-> obj2 side-length))
;;  )
;; ==============================================================================

(defun parse-declare-method (this type def)
  ;;(-> type? type-system? (listof syntax?) void?)
  ;; - -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
  ;; helper function to check if the object is a tag name
  (defun is-tag (it tag)
    (cond
      ((null? it) nil)
      ((symbol? it) (== tag it))
      ((list? it) (is-tag (car it) tag))
      (else nil)))
  (defun is-integer? (it)
    (cond
      ((null? it) nil)
      ((integer? it) T)
      ((list? it) (is-integer? (car it)))
      (else nil)))
  ;; - -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
  (log-debug "parse-declare-method~%~a~%~%" def)
  (loop for obj in def do

    ;; name args return-type [:no-virtual] [:replace] [:state] [id])
    (let ((method-name (get-symbol (car obj)))
          (args (cadr obj))
          (return-type (caddr obj))
          (no-virtual false)
          (replace-method false)
          (function-typespec (typespec-new "function"))
          (id -1))
      ;; skip name args and return
      (set! obj (cdddr obj))

      (when (is-tag obj ":no-virtual")
        (set! obj (cdr obj))
        (set! no-virtual true))

      (when (is-tag  obj ":replace")
        (set! obj (cdr obj))
        (set! replace-method true))

      (when (is-tag obj ":state")
        (set! obj (cdr obj))
        (set! function-typespec (typespec-new 'state)))

      (when (is-tag obj ":behavior")
        (set! obj (cdr obj))
        (typespec-add-new-tag function-typespec 'behavior (get-symbol (car obj)))
        (set! obj (cdr obj)))


      (when (is-integer? obj)
        (set! id (get-int (car obj)))
        (set! obj (cdr obj)))

      (unless (null? obj)
        (error (format nil "too many things in method definition ~a, was unexpeced: ~a" def obj)))

      (loop for o in args do
        (typespec-args-add function-typespec (parse-typespec this o)))

      (typespec-args-add function-typespec (parse-typespec this return-type))

      (let ((info (declare-method-for-type this
                                           type
                                           method-name
                                           no-virtual
                                           function-typespec
                                           replace-method
                                           id)))
        ;; check the method assert
        (when (!= id -1)
          ;; method id assert!
          (when (!= id (method-info-id info))
            (error
             (format nil "Method ID failed -  method ~a of type ~a (wanted ~a got ~a)~%"
                     method-name (gtype-name type) id (method-info-id info)))))))))


;; ==============================================================================
;; Declare State
;; ==============================================================================

(defun declare-state (this type def)
  "Declare state for type"
  ;;(-> type? type-system? (listof syntax?) void?)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (defun declare-item (obj)
    (cond
      ((list? obj)
       ;; (name ,@args)
       (let ((state-name (get-symbol (car obj)))
             (args (cdr obj))
             (state-typespec (typespec-new "state")))
         (loop for o in args do
           (typespec-args-add state-typespec (parse-typespec this o)))
         (typespec-args-add state-typespec (typespec-new (gtype-name type)))
         (gtype-add-state type state-name state-typespec)))
      (else
       ;; name
       (let ((state-name (if (symbol? obj) obj (get-symbol obj)))
             (state-typespec (typespec-new 'state)))
         (typespec-args-add state-typespec (typespec-new (gtype-name type)))
         (gtype-add-state type state-name state-typespec))))
    (loop for it in def do
          (declare-item it))))

;; ==============================================================================
;; Parse Structur
;; ==============================================================================

(defun parse-structure-def (this type fields options constants)
  ;;(-> struct-type? type-system? (listof syntax?) (listof syntax?) hash? defstruct-res?)

  (let ((result (make-defstruct-res))
        (size-assert  -1)
        (method-count-assert -1)
        (flag-assert 0)
        (flag-assert-set false)
        (set-heapbase false))

    ;; parse fields
    (loop for o in fields do
      (add-field this type o constants))
    ;; change size
    (setf (type-flags-size result) (get-size-in-memory type))


    (let ((rest options))
      (loop
        (when (null rest)
          (return))
        (cond
          ((list? (car rest))
           (let* ((opt-list (car rest))
                  (first (car opt-list))
                  (list-name (get-symbol first)))
             (setf opt-list (cdr opt-list))
             (cond ((== list-name ":methods")
                    (parse-declare-method this type opt-list))
                   ((== list-name ":states")
                    (declare-state this type opt-list))
                   (else
                    (error (format nil "Invalid option list in field specification: ~a" (car rest)))))
             (setf rest (cdr rest))))
          (else
           (let ((opt-name (get-symbol (car rest))))
             (setf rest (cdr rest))
             (cond
               ((== opt-name ":size-assert")
                (setf size-assert (get-int (car rest)))
                (when (== size-assert -1)
                  (error "Cannot use -1 as size-assert"))
                (setf rest (cdr rest)))
               ((== opt-name ":method-count-assert")
                (setf method-count-assert (get-int (car rest)))
                (when (== method-count-assert -1)
                  (error "Cannot use -1 as method-count-assert"))
                (setf rest (cdr rest)))
               ((== opt-name ":flag-assert")
                (setf flag-assert (get-int (car rest)))
                (setf flag-assert-set true)
                (setf rest (cdr rest)))
               ((== opt-name ":no-runtime-type")
                (setf (defstruct-res-generate-runtime-type result) false))
               ((== opt-name ":no-inspect")
                (setf (gtype-generate-inspect type) false))
               ((== opt-name ":pack-me")
                (setf (defstruct-res-pack-me result)  true))
               ((== opt-name ":heap-base")
                (let ((hb (get-int (car rest))))
                  (when (!= (mod hb #x10) 0)
                    (error "heap-base is not 16-byte aligned"))
                  (setf rest (cdr rest))
                  (setf (type-flags-heap-base result) hb)
                  (setf set-heapbase true)))
               ((== opt-name ":allow-misaligned")
                (setf (defstruct-res-allow-misaligned result) true))
               ((== opt-name ":final")
                (setf (defstruct-res-final result) true))
               ((== opt-name ":always-stack-singleton")
                (setf (defstruct-res-always-stack-singleton result) true))
               (else
                (error (format nil "Invalid option in field specification: ~a" opt-name)))))))))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (when (and (fully-defined-type-exists this (typespec-new "process"))
               (tc this (typespec-new 'process) (typespec-new (gtype-parent type))))
      ;; check heap-base if this is a child of process.
      (let* ((process-type (get-type-of-type this #'basic-type-p "process"))
             (auto-hb (align-n (- (type-flags-size result)
                                  (struct-type-size-in-mem process-type))
                               16)))
        (cond
          ((not set-heapbase)
           ;; wasnt set manually so set (definematically.
           (setf (type-flags-heap-base result) auto-hb))
          ((< (type-flags-heap-base result) auto-hb)
           ;; was set manually so verify if that's correct.
           (error
            (format nil "Process heap underflow in type ~a heap-base is ~a vs. define-detected ~a"
                    (gtype-name type) (type-flags-heap-base result) auto-hb))))))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (when (and (!= size-assert -1)
               (!= (type-flags-size result) (logand #xFFFF size-assert)))
      (error (format nil "Type ~a came out to size ~a but size-assert was set to ~a"
                     (gtype-name type) (type-flags-size result) size-assert)))

    (setf (type-flags-methods result) (get-next-method-id this type))

    (when (and (!= method-count-assert -1)
               (!= (type-flags-methods result) (logand #xFFFF method-count-assert)))
      (error
       (format nil
               "Type ~a has ~a  methods, but method-count-assert was set to ~a"
               (gtype-name type) (type-flags-methods result) method-count-assert)))

    (when (and flag-assert-set (!= (type-flags-flag result) flag-assert))
      (error
       (format nil "Type ~a has flag 0x~X but flag-assert was set to 0x~X)"
               (gtype-name type)
               (type-flags-flag result)
               flag-assert)))
    result))

;; ==============================================================================

;; ==============================================================================


(defun parse-bitfield-type-def (this type fields options)
  ;;(-> bitfield-type? type-system? (listof syntax?) (listof syntax?) defbitfield-res?)
  (log-debug "parse-bitfield:~%~a~%~%" fields)
  (let ((result (make-defbitfield-res))
        (size-assert -1)
        (method-count-assert -1)
        (flag-assert 0)
        (flag-assert-set false))

    (loop for o in fields do
      (add-bitfield this type o))

    (setf (type-flags-size result) (get-size-in-memory type))

    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (let ((rest options))
      (loop
        (when (null rest)
          (return))
        (cond
          ((is-pair? (car rest))
           (let* ((opt-list (car rest))
                 (first (car opt-list)))
               (setf opt-list (cdr opt-list))
             (if (== (get-symbol first) ":methods")
                 (parse-declare-method this type opt-list)
                 (error (format nil "Invalid option list in field specification: ~a" (car rest))))
             (setf rest (cdr rest))))
          (else
           (let ((opt-name (get-symbol (car rest))))
             (setf rest (cdr rest))
             (cond
               ((== opt-name ":size-assert")
                (setf size-assert (get-int (car rest)))
                (when (== size-assert -1)
                  (error "Cannot use -1 as size-assert"))
                (setf rest (cdr rest)))
               ((== opt-name ":method-count-assert")
                (setf method-count-assert (get-int (car rest)))
                (when (== method-count-assert -1)
                  (error "Cannot use -1 as method-count-assert"))
                (setf rest (cdr rest)))
               ((== opt-name ":flag-assert")
                (setf flag-assert (get-int (car rest)))
                (setf flag-assert-set true)
                (setf rest (cdr rest)))
               ((== opt-name ":no-runtime-type")
                (setf (defbitfield-res-generate-runtime-type result) false))
               ((== opt-name ":no-inspect")
                (setf (gtype-generate-inspect type) false))
               (else
                (error (format nil "Invalid option in field specification: ~a" opt-name)))))))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (when (and (!= size-assert -1)
             (!= (type-flags-size result) size-assert))
    (error (format nil "Type ~a came out to size ~a but size-assert was set to ~a"
                   (gtype-name type) (type-flags-size result) size-assert)))

  (setf (type-flags-methods result) (get-next-method-id this type))

  (when (and (!= method-count-assert -1)
             (!= (type-flags-methods result) method-count-assert))
    (error
     "Type ~a has ~a methods, but method-count-assert was set to ~a"
     (gtype-name type) (type-flags-methods result) method-count-assert))

  (when (and flag-assert-set
             (!= (type-flags-flag result) flag-assert))
    (error 'deftype
     (format nil "Type ~a has flag 0x~X but flag-assert was set to 0x~X"
             (gtype-name type)
             (type-flags-flag result)
             flag-assert)))

  result))
;; ==============================================================================
;; Parse the type spec
;; ==============================================================================

;; The typespec is a list of items
;; (type1 type2 (type3 type4) :tag1 val1 tag2 val2)
(defun parse-typespec (this exp)
  ;;(-> thistem? syntax? typespec?)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Check in the expression is a tag's key by the first character
  (defun is-tag-name? (s)
    (or (and (string? s) (equal? #\: (string-ref s 0)))
        (and (symbol? s) (equal? #\: (string-ref (symbol-name s) 0)))))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;;
  (let ((e exp))
    (cond
      ((symbol? e)
       ;; parse -only the type name
       (make-typespec this e))
      ((list? e)
       ;; parse - the list if elements as children types or tags
       ;; make a type
       (let ((ts (make-typespec this (car e)))
             (tag-name nil)
             (tag-val nil))
         ;; parse rest of items
         (let ((rest (cdr e)))
           (loop
             (when (null rest)
               (return))
             (let ((it (car rest)))
               (cond
                 ((is-tag-name? it)
                  (set! tag-name (get-symbol it))
                  (set! rest (cdr rest))

                  (when (null? rest)
                    (error "TypeSpec missing tag value"))
                  (set! tag-val (car rest))

                  (cond
                    ((== tag-name ":behavior")
                     (let ((val (get-symbol tag-val)))
                       (when (and (not (fully-defined-type-exists this val))
                                  (not (partially-defined-type-exists this val)))
                         (error (format nil "Behavior tag uses an unknown type ~a" val)))
                       (typespec-add-new-tag ts tag-name val)))
                    (else
                     (error (format nil "Type tag ~a is unknown" tag-name)))))
                 (else
                  ;; normal argument.
                  (typespec-args-add ts (parse-typespec this it)))))
             (setf rest (cdr rest))))
         ts))
      (else
       (error (format nil "invalid typespec: ~a" exp))))))

;; ==============================================================================
;; Deftype
;; ==============================================================================

;; The constant list is the hash table
(defun parse-deftype (this def constants)
  ;;(-> (listof syntax?) thistem? (or/c nil hash?) deftype-res?)

  (let* ((constants-to-use (if constants constants (make-hash)))
         (iter def)
         (type-name-obj (car def))
         (parent-list-obj (cadr def))
         (field-list-obj (caddr def))
         (iter (cdddr iter))
         (options-obj iter))

    (unless (symbol? type-name-obj)
      (error "deftype must be given a symbol as the type name"))

    (let* ((name (get-symbol type-name-obj))
           (parent-type-name (deftype-parent-list (get-list parent-list-obj)))
           (parent-type (make-a-typespec this parent-type-name))
           (result (make-deftype-res)))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      (defun parse-basic-internal ()
        (let ((new-type (basic-type-new parent-type-name name false 0))
              (pto (lookup-type this parent-type)))
          (assert pto);
          (when (basic-type-is-final pto)
            (error
             (format nil "[TypeSystem] Cannot make a child type ~a of final basic type ~a"
                     name parent-type-name)))
          (struct-type-inherit new-type pto)
          (forward-declare-type-as this name 'basic)
          ;; returns StructureDefResult
          (let ((sr (parse-structure-def this new-type field-list-obj options-obj constants-to-use)))
            (my/with-slots sr-
                (defstruct-res generate-runtime-type
                  pack-me
                  allow-misaligned
                  final
                  always-stack-singleton) sr
              (type-flags-copy result sr)
              (setf (deftype-res-create-runtime-type result) sr-generate-runtime-type)
              (when sr-pack-me
                (setf (struct-type-pack new-type) true))
              (when sr-allow-misaligned
                (error (format nil "[TypeSystem] invalid pack option on basic : :allow-misaligned was set on ~a, which is a basic and cannot be misaligned~%"
                               name)))
              (when sr-always-stack-singleton
                (error (format nil "[TypeSystem] invalid stack singleton option on basic : :always-stack-singleton was set on ~a, which is a basic and cannot be a stack singleton~%"                     name)))

              (setf (gtype-heap-base new-type) (type-flags-heap-base result))
              (when sr-final
                (setf (basic-type-is-final new-type) true))
              (add-type this name new-type)))))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      (defun parse-struct-internal ()
        (let ((new-type (struct-type-new parent-type-name name false false false 0))
              (pto (lookup-type this parent-type)))
          (assert pto)
          (struct-type-inherit new-type pto)
          (forward-declare-type-as this name "structure")
          (let ((sr (parse-structure-def this new-type field-list-obj options-obj constants-to-use)))
            (my/with-slots sr- (defstruct-res
                                   generate-runtime-type
                                 pack-me
                                 allow-misaligned
                                 final
                                 always-stack-singleton) sr
              (type-flags-copy result sr)
              (setf (deftype-res-create-runtime-type result) sr-generate-runtime-type)
              (when sr-pack-me
                (setf (struct-type-pack new-type) true))

              (when sr-allow-misaligned
                (setf (struct-type-allow-misalign new-type) true))

              (when sr-always-stack-singleton
                (setf (struct-type-always-stack-singleton new-type) true))

              (when sr-final
                (error (format nil "[TypeSystem] :final option cannot be used on structure type ~a" name)))

              (setf (gtype-heap-base new-type) (type-flags-heap-base sr))
              (add-type this name new-type)))))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      (defun  parse-integer-internal ()
        (let* ((pto (lookup-type this parent-type))
               (new-type (bitfield-type-new parent-type-name name (get-size-in-memory pto) (get-load-signed pto))))
          (assert pto);
          (assert (value-type-p pto))
          (value-type-inherit new-type pto)
          (setf (gtype-runtime-name new-type) (gtype-runtime-name pto))
          (let ((sr (parse-bitfield-type-def this new-type field-list-obj options-obj)))
            (type-flags-copy result sr)
            (setf (deftype-res-create-runtime-type result) (defbitfield-res-generate-runtime-type sr))
            (add-type this name new-type))))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      (cond
        ((is-type this "basic" parent-type)
         (parse-basic-internal))
        ((is-type this "structure" parent-type)
         (parse-struct-internal))
        ((is-type this "integer" parent-type)
         (parse-integer-internal))
        (else
         (error (format nil "Creating a child type from ~a is not allowed or not supported yet."
                        parent-type))))
      (setf (deftype-res-type result) (make-a-typespec this name))
      (setf (deftype-res-type-info result) (lookup-type this (deftype-res-type result)))
      result)))

;; Used only for tests! Becayse passing the test 'goalc-all-types.gc'
;; requires to declare type

(defun parse-declare-type (this rest)
  ;;(-> (listof syntax?) type-system? void)
  (let* ((type-name (get-symbol (car rest)))
         (kind (get-symbol (cadr rest))))
    (forward-declare-type-as this type-name kind)))
