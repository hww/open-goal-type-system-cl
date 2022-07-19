(in-package :type-system/type)
(named-readtables:in-readtable :interpol-syntax)
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

;; ----------------------------------------------------------------------------
;; The field type
;; ----------------------------------------------------------------------------

(defstruct field
   (name nil :type symbol)             ; string?
   (type nil :type (or null typespec)) ; type-spec?
   (offset -1 :type integer)           ; int? -1;
   (is-inline nil :type boolean)       ; bool? false does not make sense if m-type is value, and not an array and not dynamic
   (is-dynamic nil :type boolean)      ; bool? false;
   (is-array nil :type boolean)        ; bool? false;
   (array-size 0 :type integer)        ; int? 0;
   (alignment -1 :type integer)        ; int?-1;
   (skip-in-static-decomp
    nil :type boolean)                 ; bool? = false;
   (is-user-placed nil :type boolean)  ; bool? = false;  // was this field placed manually by the user?
   (field-score 0.0 :type float)       ; double? = 0.;
)

(declaim (ftype (function (symbol typespec integer) field) field-new))
(defun field-new (name type offset)
  "Constructor"
  (make-field :name name :type type :offset offset))

(defun field-is-array? (this)
  (field-is-array this))

(defun field-set-inline (this)
  "Mark a field as inline. This has a different meaning depending on if the field is also an array"
  (setf (field-is-inline this) T))

(defun field-set-dynamic (this)
  "Mark a field as dynamic, indicating it has array semantics."
  (setf (field-is-dynamic this) T))

(defun field-set-array (this size)
  "Mark a field as a fixed (size integer) array."
  (setf (field-array-size this) size)
  (setf (field-is-array this) true))

(defun field-set-alignment (this align)
  (setf (field-alignment this) align))

(defun field-set-offset (this align)
  (setf (field-offset this) align))

(defun field-set-skip-in-static-decomp (this)
  (setf (field-skip-in-static-decomp this) T))

(defun field-mark-as-user-placed (this)
  (setf (field-is-user-placed this) T))

(defun field-set-field-score (this score)
  (setf (field-field-score this) score))

(defmethod compare ((this field) (other field))
  (equal? this other))

(defmethod diff ((this field) (other field))
  (field-diff this other))
(defmethod field-diff ((this field) (other field))  
  ;(unless (field-p other) (error (incompatible-diff 'field other)))
  (my/with-slots l- (field name type offset is-inline is-dynamic
			   is-array array-size alignment
			   skip-in-static-decomp
			   ) this
    (my/with-slots r- (field name type offset is-inline is-dynamic
			     is-array array-size alignment
			     skip-in-static-decomp
			     ) other
      (let ((result ""))
	(when (!= l-name r-name)
	  (string-append! result (format nil "name: ~a vs. ~a~%" l-name r-name)))
	(when (!= l-type r-type)
	  (string-append! result (format nil "type: ~a vs. ~a~%" (inspect l-type) (inspect r-type))))
	(when (!= l-offset r-offset)
	  (string-append! result (format nil "offset: ~a vs. ~a~%" l-offset r-offset)))
	(when (!= l-is-inline r-is-inline)
	  (string-append! result (format nil "inline: ~a vs. ~a~%" l-is-inline r-is-inline)))
	(when (!= l-is-dynamic r-is-dynamic)
	  (string-append! result (format nil "dynamic: ~a vs. ~a~%" l-is-dynamic r-is-dynamic)))
	(when (!= l-is-array r-is-array)
	  (string-append! result (format nil "array: ~a vs. ~a~%" l-is-array r-is-array)))
	(when (!= l-array-size r-array-size)
	  (string-append! result (format nil "array-size: ~a vs. ~a~%" l-array-size r-array-size)))
	(when (!= l-alignment r-alignment)
	  (string-append! result (format nil "alignment: ~a vs. ~a~%" l-alignment r-alignment)))
	(when (!= l-skip-in-static-decomp r-skip-in-static-decomp)
	  (string-append! result (format nil "skip-in-static-decomp: ~a vs. ~a~%"
					 l-skip-in-static-decomp
					 r-skip-in-static-decomp)))
	result))))

(defmethod to-str ((this field))
  (format nil
	  "[Field] (~a type: ~a offset: ~a) inline ~5a dynamic: ~5a array: ~5a array-size: ~2a align: ~2a skip: ~a"
          (field-name this)
          (to-str (field-type this))
          (field-offset this)
	  (field-is-inline this)
	  (field-is-dynamic this)
	  (field-is-array this) 
	  (field-array-size this)
	  (field-alignment this) 
	  (field-skip-in-static-decomp this)))

;; ----------------------------------------------------------------------------
;; The null type
;;
;; Used only for "none" - this is a type that the compiler can use for "this
;; has no value". Attempting to do anything with a NoneType is an error.
;; ----------------------------------------------------------------------------

(defstruct (null-type (:include gtype)))

;; Constructor

;(declaim (ftype (function (symbol) null-type) null-type-new))
(defun null-type-new (name)
  "Constructor"
  (let* ((base (gtype-new EMPTY-SYMBOL name nil 0))
	 (new (copy-parent-struct-func :type-system/type (make-null-type) base)))
    new))

(defmethod is-reference? ((this null-type))
    (error "is_reference called on NullType"))
(defmethod get-load-size ((this null-type))
    (error "get_load_size called on NullType"))
(defmethod get-size-in-memory ((this null-type))
    (error "get_size_in_memory called on NullType"))
(defmethod get-load-signed ((this null-type))
    (error "get_load_size called on NullType"))
(defmethod get-offset ((this null-type))
    (error "get_offset called on NoneType"))
(defmethod get-in-memory-alignment ((this null-type))
    (error "get_in_memory_alignment called on NullType"))
(defmethod get-inl-array-stride-align ((this null-type))
    (error "get_inline_array_start_alignment called on NullType"))
(defmethod get-inl-array-start-align ((this null-type))
    (error "get_inline_array_stride_alignment called on NullType"))
(defmethod get-preferred-reg-class ((this null-type))
    (error "get_preferred_reg_class called on NullType"))

(defmethod compare ((this null-type) other)
  (equalp this other))

(defmethod diff ((this null-type) (other null-type))
  ;(unless (null-type? other) (error (incompatible-diff 'null-type other)))
  (let ((result ""))
    (string-append! result (diff this other))
    (when (!= this other)
      (string-append! result "NullType error"))
    result))

(defmethod to-str ((this null-type))
  (format nil "[~a]" (gtype-name this)))


;; ----------------------------------------------------------------------------
;; A type which is treated as a value, as opposed to a reference. These types
;; fit in a register and are always passed by value in arguments/returns.
;; ----------------------------------------------------------------------------

(defstruct (value-type (:include gtype))
  (size -1 :type integer)
  (offset 0 :type integer)
  (sign-extend nil :type boolean)            
  (reg-kind REG-CLASS-INVALID :type integer))

;; Constructor

;(declaim (ftype (function (symbol symbol boolean integer boolean &optional integer) value-type) value-type-new))
(defun value-type-new (parent name is-boxed size sign-extend &optional (reg-kind REG-CLASS-INVALID))
  (let* ((base (gtype-new parent name is-boxed 0))
	 (new (copy-parent-struct-func
	       :type-system/type
	       (make-value-type) base
	       :size size
	       :offset 0
	       :sign-extend sign-extend
	       :reg-kind reg-kind)))
    new))

(defmethod is-reference? ((this value-type))
  nil)
(defmethod get-load-size ((this value-type))
  (value-type-size this))
(defmethod get-size-in-memory ((this value-type))
  (value-type-size this))
(defmethod get-load-signed ((this value-type))
  (value-type-sign-extend this))
(defmethod get-offset ((this value-type))
  (value-type-offset this))
(defmethod get-in-memory-alignment ((this value-type))
  (value-type-size this))
(defmethod get-inl-array-stride-align ((this value-type))
  (value-type-size this))
(defmethod get-inl-array-start-align ((this value-type))
  (value-type-size this))
(defmethod get-preferred-reg-class ((this value-type))
  (value-type-reg-kind this))

;; Compate objects 

(defmethod compare ((this value-type) (other value-type))
  (equal? this other))

;; Diff for this type

(defmethod diff ((this value-type) (other value-type))
  (value-type-diff this other))

(defmethod value-type-diff ((this value-type) (other value-type))
 ; (unless (value-type-p other) (error (incompatible-diff 'value-type other)))
  (my/with-slots l. (value-type size offset sign-extend reg-kind) this
    (my/with-slots r. (value-type size offset sign-extend reg-kind) other

      (let ((result ""))
	(string-append! result (gtype-diff this other))
	(when (!= l.size r.size)
	  (string-append! result (format nil "size: ~a vs. ~a~%" l.size r.size)))
	(when (!= l.offset r.offset)
	  (string-append! result (format nil "offset: ~a vs. ~a~%" l.offset r.offset)))
	(when (!= l.sign-extend r.sign-extend)
	  (string-append! result (format nil "sign-extend: ~a vs. ~a~%" l.sign-extend r.sign-extend)))
	(when (!= l.reg-kind r.reg-kind)
	  (string-append! result (format nil "reg-kind: ~a vs ~a~%" l.reg-kind r.reg-kind)))
	result))))

(defmethod to-str ((this value-type))
  (let ((result ""))
    (string-append!
     result
     (format nil "[ValueType] ~a~% parent: ~a~% boxed: ~a~% size: ~a~% sext: ~a~%"
	     (gtype-name this)
	     (gtype-parent this)
	     (gtype-is-boxed this)
	     (value-type-size this)
	     (value-type-sign-extend this)))
    (string-append! result (methods-to-str this))
    result))

(defun value-type-inherit (this parent)
  (setf (value-type-sign-extend this) (value-type-sign-extend parent))
  (setf (value-type-size this) (value-type-size parent))
  (setf (value-type-offset this) (value-type-offset parent))
  (setf (value-type-reg-kind this) (value-type-reg-kind parent)))


;; ----------------------------------------------------------------------------
;; The refference type
;;
;; ReferenceType is an abstract class that's a parent for everything that uses
;; reference semantics. This means this type behaves like a C pointer - the
;; thing that's passed around is a reference to some memory somewhere.
;; ----------------------------------------------------------------------------

(defstruct (reference-type (:include gtype)))

;; Constructor

;(declaim (ftype (function (symbol symbol boolean integer) reference-type) reference-type-new))
(defun reference-type-new (parent name is-boxed heap-base)
  (let* ((base (gtype-new parent name is-boxed heap-base))
	 (new (copy-parent-struct-func :type-system/type (make-reference-type) base)))
    new))

(defmethod is-reference? ((this reference-type))
  T)
(defmethod get-load-size ((this reference-type))
  POINTER-SIZE)
(defmethod get-size-in-memory ((this reference-type))
  POINTER-SIZE)
(defmethod get-load-signed ((this reference-type))
  nil)
(defmethod get-offset ((this reference-type))
  POINTER-SIZE)
(defmethod get-in-memory-alignment ((this reference-type))
  POINTER-SIZE)
(defmethod get-inl-array-stride-align ((this reference-type))
  POINTER-SIZE)
(defmethod get-inl-array-start-align ((this reference-type))
  POINTER-SIZE)
(defmethod get-preferred-reg-class ((this reference-type))
  REG-CLASS-GPR-64)


(defmethod to-str ((this reference-type))
  (string-append
   (format nil "[ReferenceType] ~a~% parent: ~a~% boxed: ~a~%"
	   (gtype-name this)
	   (gtype-parent this)
	   (gtype-is-boxed this))
   (methods-to-str this)))
 
(defmethod compare ((this reference-type) (other reference-type))
  (equal? this other))

(defmethod diff ((this reference-type) (other reference-type))
  (gtype-diff this other))
  
;; ----------------------------------------------------------------------------
;; The struct type
;;
;; StructureType is a ReferenceType which has fields.  It's also the parent of BasicType,
;; which is a structure with runtime typing informat nilion.
;; ----------------------------------------------------------------------------

(defstruct (struct-type (:include reference-type))
  (fields
   (arr-new :element-type :field)
   :type array)                      ; std::vector<Field>
  (dynamic nil :type boolean)        ; bool? = false;
  (size-in-mem 0 :type integer)      ; int? = 0;
  (pack nil :type boolean)           ; bool? = false;
  (allow-misalign nil :type boolean) ; bool? = false;
  (offset 0 :type integer)           ; int? = 0;
  (always-stack-singleton nil
   :type boolean)                    ; bool? = false;
  (idx-of-first-unique-field 0
   :type integer)                    ; int? = 0;
  )

(declaim (ftype (function (symbol symbol boolean boolean boolean integer) struct-type) struct-type-new))
(defun struct-type-new (parent name boxed dynamic pack heap-base)
  "Constructor"
  (let* ((base (reference-type-new parent name boxed heap-base))
	 (new (copy-parent-struct-func
	       :type-system/type
	       (make-struct-type)
	       base
	       :fields (arr-new :element-type :field)
	       :dynamic dynamic
	       :size-in-mem 0      
	       :pack pack
	       :allow-misalign nil
	       :offset 0
	       :always-stack-singleton nil
	       :idx-of-first-unique-field 0)))
    new))


(defmethod is-reference? ((this struct-type))
  T)
(defmethod get-load-size ((this struct-type))
  POINTER-SIZE)
(defmethod get-size-in-memory ((this struct-type))
  (struct-type-size-in-mem this))
(defmethod get-load-signed ((this struct-type))
  nil)
(defmethod get-offset ((this struct-type))
  (struct-type-offset this))
(defmethod get-in-memory-alignment ((this struct-type))
  STRUCTURE-ALIGNMENT)
(defmethod get-inl-array-stride-align ((this struct-type))
  (get-inl-array-stride-align-impl this))
(defmethod get-inl-array-start-align ((this struct-type))
  (get-inl-array-start-align-impl this))

;; Comparator

(defmethod compare ((this struct-type) (other struct-type))
  (equal? this other))

;; Diff for this type

(defmethod diff ((this struct-type) (other struct-type))
  (struct-type-diff this other))
(defmethod struct-type-diff ((this struct-type) (other struct-type))
  ;(unless (struct-type? other) (error (incompatible-diff 'struct-type other)))
  (my/with-slots l- (struct-type
                          fields dynamic size-in-mem pack allow-misalign offset
                          always-stack-singleton idx-of-first-unique-field) this
  (my/with-slots r- (struct-type
                          fields dynamic size-in-mem pack allow-misalign offset
                          always-stack-singleton idx-of-first-unique-field) other
    (let* ((l-fields.size (arr-count l-fields))
	   (r-fields.size (arr-count r-fields))
	   (min-fields (min l-fields.size r-fields.size))
	   (result ""))
      (string-append! result (gtype-diff this other))
      (when (!= l-fields r-fields)
	(when (!= l-fields.size r-fields.size)
	  (string-append! result (format nil "Number of fields ~a vs. ~a~%" l-fields.size r-fields.size)))

	(dotimes (i min-fields)
	  (let ((lf (arr-ref l-fields i))
		(rf (arr-ref r-fields i)))
	    (when (!= lf rf)
	      (string-append! result (format nil "field ~a (~a/~a):~%"
					     i (field-name lf) (field-name rf)))
	      (field-diff lf rf)))))

      (when (!= l-dynamic r-dynamic)
	(string-append! result (format nil "dynamic: ~a vs. ~a~%" l-dynamic r-dynamic)))
      (when (!= l-size-in-mem r-size-in-mem)
	(string-append! result (format nil "size-in-mem: ~a vs. ~a~%" l-size-in-mem r-size-in-mem)))
      (when (!= l-pack r-pack)
	(string-append! result (format nil "pack: ~a vs. ~a~%" l-pack r-pack)))
      (when (!= l-allow-misalign r-allow-misalign)
	(string-append! result (format nil "allow-misalign: ~a vs. ~a~%" l-allow-misalign r-allow-misalign)))
      (when (!= l-always-stack-singleton r-always-stack-singleton)
	(string-append! result (format nil "always-stack-singleton: ~a vs. ~a~%"
				       l-always-stack-singleton
				       r-always-stack-singleton)))
      (when (!= l-offset r-offset)
	(string-append! result (format nil "offset: ~a vs. ~a~%" l-offset r-offset)))
      (when (!= l-idx-of-first-unique-field r-idx-of-first-unique-field)
	(string-append! result (format nil "idx-of-first-unique-field: ~a vs. ~a~%"
				       l-idx-of-first-unique-field
				       r-idx-of-first-unique-field)))
      result))))

(defmethod to-str ((this struct-type))
  (struct-type-to-str this))
(defmethod struct-type-to-str ((this struct-type))
  (let ((result ""))
    (string-append! result (format nil "[StructType] ~a~% parent: ~a~% boxed: ~a~% size: ~a~% pack: ~a~%"
				   (gtype-name this)
				   (gtype-parent this)
				   (gtype-is-boxed this)
				   (struct-type-size-in-mem this)
				   (struct-type-pack this)))
    (string-append! result (format nil " misalign: ~a~% heap-base: ~a~% stack-singleton: ~a~% fields:~%~a~%"
				   (struct-type-allow-misalign this)
				   (gtype-heap-base this)
				   (struct-type-always-stack-singleton this)
				   (struct-type-inspect-fields this)))
    (string-append! result (format nil " methods:~%"))
    (string-append! result (methods-to-str this))
    (string-append! result (format nil "~%"))
    result))

;; So the GOAL compiler was weird here.
;; It seems like there were two states:
;; - don't care about alignment of both the first element and the later
;; - don't care about the alignment, but pad the stride.
;; so you end up with a misaligned array of padded structures which seems very stupid.

(defun get-inl-array-stride-align-impl (this)
  (cond
    ((struct-type-pack this)
     ;; make elements of inline array the minimum allowable alignment.
     (let ((alignment  1))
       ;; TODO - I don't know if GOAL actually did this check, maybe packed inline arrays could
       ;; violate these?
       (loop for field across (struct-type-fields this)
	     do
		(set! alignment (max alignment (field-alignment field))))
       alignment))
    (else
     ;; make elements of inline array properly aligned structures
     STRUCTURE-ALIGNMENT)))

(defun get-inl-array-start-align-impl (this)
  (cond
    ((or (struct-type-pack this) (struct-type-allow-misalign this))
     ;; make elements of inline array the minimum allowable alignment.
     (let ((alignment  1))
       ;; TODO - I don't know if GOAL actually did this check, maybe packed inline arrays could
       ;; violate these?
       (loop for field across (struct-type-fields this)
	     do
		(set! alignment (max alignment (field-alignment field))))
       alignment))
    (else
     ;; make elements of inline array properly aligned structures
     STRUCTURE-ALIGNMENT)))

;; Find the filed by name

(defun struct-type-lookup-field (this name)
;  (-> struct-type? symbol? (or/c nil field?))
  (find name (struct-type-fields this) :key #'field-name))

;; Print all fields

(defun struct-type-inspect-fields (this)
 ; (-> struct-type? string?)
  (apply 'string-append
         (map 'list
	      #'(lambda (f) (format nil "    ~a~%" (inspect f)))
              (struct-type-fields this))))

;; Find the method wih name

(defun struct-lookup-field (this name)
  ;(-> struct-type? symbol? method-info?)
  (find name (struct-type-fields this) :key #'method-info-name))

(defun struct-type-inherit (this parent)
  ;;(-> struct-type? struct-type? void)
  (setf (struct-type-fields this) (list->vector (vector->list (struct-type-fields parent))))
  (setf (struct-type-dynamic this) (struct-type-dynamic parent))
  (setf (struct-type-size-in-mem this) (struct-type-size-in-mem parent))
  (setf (struct-type-idx-of-first-unique-field this) (arr-count (struct-type-fields parent))))

(defun struct-type-add-field (this f new-size-in-mem)
  ;(-> struct-type? field? integer? void)
  (arr-push (struct-type-fields this) f)
  ;(printf "ADD FIELD (~a) NEW-SIZE ~a~%" (inspect f) new-size-in-mem)
  (setf (struct-type-size-in-mem this) new-size-in-mem))

(defun struct-type-get-size-in-memory (this)
;  (-> struct-type? integer?)
  (struct-type-size-in-mem this))

(defun struct-type-override-size-in-memory (this size)
 ; (-> struct-type? integer? void?)
  (setf (struct-type-size-in-mem this) size))

(defun struct-type-override-offset (this offset)
  ;(-> struct-type? integer? void)
  (setf (struct-type-offset this) offset))

(defun struct-type-fields-count (this)
  ;(-> struct-type? integer?)
  (arr-count (struct-type-fields this)))

(defun struct-type-fields-ref (this idx)
  ;(-> struct-type? integer? field?)
  (arr-ref (struct-type-fields this) idx))



;; ----------------------------------------------------------------------------
;; The basic type
;; ----------------------------------------------------------------------------

(defstruct (basic-type (:include struct-type)) (is-final nil :type boolean))

(defmethod to-str ((this basic-type))
  (string-append
   (struct-type-to-str this)
   (format nil " is-final:~a~%" (basic-type-is-final this))))

(defmethod compare ((this basic-type) (other basic-type))
  (equal? this other))

(defmethod diff ((this basic-type) (other basic-type))
  (basic-type-diff this other))
  
(defun basic-type-new (parent name &optional (dynamic nil) (heap-size 0))
  (let* ((base (struct-type-new parent name T dynamic nil heap-size))
	 (new  (copy-parent-struct-func
		:type-system/type (make-basic-type) base :is-final nil)))
    new))
    
    
(defmethod basic-type-diff ((this basic-type) (other basic-type))
  ;;(unless (basic-type-p other) (error (incompatible-diff 'basic-type other)))
  (my/with-slots
   l- (basic-type is-final) this
   (my/with-slots
    r- (basic-type is-final) other
    (string-append
     (flatten
      (list
       (struct-type-diff this other)
       (when (!= l-is-final r-is-final)
	 (format nil "final: ~a vs. ~a~%" l-is-final r-is-final))))))))


;; Make this type as final

(defun basic-type-set-final (this)
  (setf (basic-type-is-final this) T))

;; ----------------------------------------------------------------------------
;; Bitfield
;; ----------------------------------------------------------------------------

(defstruct sbitfield 
  (type nil :type (or null symbol))         ; type-spec?
  (name nil :type (or null symbol))         ; string?
  (offset -1 :type integer)                 ; int? -1;  in bits
  (size -1 :type integer)                   ; int = -1; in bits.
  (skip-in-static-decomp nil :type boolean) ; bool? = false;
  )

(defmethod compare ((this sbitfield) (other sbitfield))
  (equal? this other))

(defun sbitfield-new (type name offset size skip-in-static-decomp)
  (make-sbitfield :type type :name name :offset offset :size size
  :skip-in-static-decomp skip-in-static-decomp))

(defmethod to-str ((this sbitfield))
  (format nil "[~a ~a] sz ~a off ~a"
          (sbitfield-name this)
          (inspect (sbitfield-type this))
          (sbitfield-size this)
          (sbitfield-offset this)))

;; Diff
(defmethod diff ((this sbitfield) (other sbitfield))
  (sbitfield-diff this other))

(defmethod sbitfield-diff ((this sbitfield) (other sbitfield))
  ;(unless (sbitfield-p other) (error (incompatible-diff 'sbitfield other)))
  (my/with-slots l. (sbitfield type name offset size skip-in-static-decomp) this
    (my/with-slots r. (sbitfield type name offset size skip-in-static-decomp) other
      (let ((result ""))
	(when (!= l.type r.type)
	  (string-append! result (format nil "type: ~a vs. ~a~%" (inspect l.type) (inspect r.type))))
	(when (!= l.name r.name)
	  (string-append! result (format nil "name: ~a vs. ~a~%" l.name r.name)))
	(when (!= l.offset r.offset)
	  (string-append! result (format nil "offset: ~a vs. ~a~%" l.offset r.offset)))
	(when (!= l.size r.size)
	  (string-append! result (format nil "size: ~a vs. ~a~%" l.size r.size)))
	(when (!= l.skip-in-static-decomp r.skip-in-static-decomp)
	  (string-append! result (format nil "skip-in-static-decomp: ~a vs. ~a~%" l.skip-in-static-decomp
					 r.skip-in-static-decomp)))
	result))))

;; ----------------------------------------------------------------------------
;; BitfieldType
;; ----------------------------------------------------------------------------

(defstruct (bitfield-type (:include value-type))
  (fields (arr-new :element-type :sbitfield) :type vector))

(defmethod compare ((this bitfield-type) (other bitfield-type))
  (equal? this other))

(defun bitfield-type-new (parent name  size sign-extend)
  (let* ((base (value-type-new parent name nil size sign-extend))
	 (new (copy-parent-struct-func
	       :type-system/type (make-bitfield-type) base
	       :fields (arr-new))))
    new))

(defmethod to-str ((this bitfield-type))
  (let ((result ""))
    (string-append!
     result
     (format nil "Parent type: ~a~%Fields:~%" (gtype-parent this)))

    (loop for it across  (bitfield-type-fields this)
	  do
	     (string-append!
	      result
	      (format nil "~a~%" (inspect it))))
    (string-append!
     result
     (format nil "Mem size: ~a, load size: ~a, signed ~a, align ~a~%"
	     (get-size-in-memory this)
	     (get-load-size this)
	     (get-load-signed this)
	     (get-in-memory-alignment this)))
    result))

;; Diff for this type
(defmethod diff ((this bitfield-type) (other bitfield-type))
  (enum-type-diff this other))

(defmethod enum-type-diff ((this bitfield-type) (other bitfield-type))
  ;(unless (bitfield-type? other) (error (incompatible-diff 'bitfield-type other)))
  (my/with-slots l- (bitfield-type fields) this
    (my/with-slots r- (bitfield-type fields) other
      (let* ((l-fields-size (arr-count l-fields))
	     (r-fields-size (arr-count r-fields))
	     (min-fields (min l-fields-size r-fields-size))
	     (result ""))
	(string-append! result (gtype-diff this other))
	(string-append! result (value-type-diff this other))

	(when (!= l-fields r-fields)
	  (when (!= l-fields-size r-fields-size)
	    (string-append!
	     result
	     (format nil "Number of fields ~a vs. ~a~%" l-fields-size r-fields-size)))

	  (dotimes (i min-fields)
	    (let ((lf (arr-ref l-fields i))
		  (rf (arr-ref r-fields i)))
	      (when (!= lf rf)
		(string-append!
		 result
		 (format nil "field ~a (~a/~a):~%~a~%"
			 i (sbitfield-name lf) (sbitfield-name rf)
			 (sbitfield-diff lf rf)))))))
	result))))

;; Find the method wih name

(defun lookup-bitfield (this name)
  (find name (slot-value this :fields) :key #'sbitfield-name))

;; ----------------------------------------------------------------------------
;; Enum
;; ----------------------------------------------------------------------------


(defstruct (enum-type (:include value-type))
  (is-bitfield nil :type boolean)                ; bool?
  (entries (make-hash-table) :type hash-table)   ; hash-table of enum-entry?
  )


(defmethod to-str ((this enum-type))
  (format nil "[EnumType] ~a" (gtype-name this)))

(defmethod compare ((this enum-type) (other enum-type))
  (equal? this other))

;; @params entries - map of symbols to the integergs

(defun enum-type-new (parent name is-bitfield entries)
  (let ((base (value-type-new
               (gtype-name parent)
               name
               (gtype-is-boxed parent)
               (get-load-size parent)
               (get-load-signed parent))))
    (copy-parent-struct-func
     :type-system/type (make-enum-type) base :is-bitfield is-bitfield :entries entries)))

;; Find item

(defun enum-type-find (this name)
  (hash-ref (enum-type-entries this) name nil))

;; Diff for this type

(defmethod diff ((this enum-type) (other enum-type))
  ;(unless (enum-type? other) (error (incompatible-diff 'enum-type other)))
  (my/with-slots l- (enum-type is-bitfield entries) this
    (my/with-slots r- (enum-type is-bitfield entries) other
      (let* ((l-entries-size (hash-count l-entries))
	     (r-entries-size (hash-count r-entries))
	     (result ""))
       
	(string-append! result (gtype-diff this other))
	(string-append! result (value-type-diff this other))

	(when (!= l-is-bitfield r-is-bitfield)
	  (string-append! result (format nil "is_bitfield: ~a vs ~a~%" l-is-bitfield r-is-bitfield)))

	(when (!= l-entries r-entries)
	  (string-append! result (format nil "Entries are different:~%"))
	  (when (!= l-entries-size r-entries-size)
	    (string-append!
	     result
	     (format nil "Number of entries ~a vs ~a~%" l-entries-size r-entries-size)))

	  (hash-map l-entries
		    (lambda (lk lv)
		      (let ((rv (hash-ref r-entries lk)))
			(cond
			  ((not rv)
			   (string-append!
			    result
			    (format nil "  ~a is in one, but not the other-~%" lk)))
			  ((!= lv rv)
			   (string-append!
			    result
			    (format nil "  ~a is defined differently: ~a vs ~a~%"
				    lk lv rv))))))))
	result))))

