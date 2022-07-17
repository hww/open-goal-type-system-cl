(in-package :type-system/type)


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

(defconstant EMPTY-SYMBOL :||)

;; ----------------------------------------------------------------------------
;; The method info
;; ----------------------------------------------------------------------------

;; The method info structure

(defstruct method-info
  (id -1 :type integer)
  (name EMPTY-SYMBOL :type symbol)
  (type nil :type typespec)
  (defined-in-type nil :type symbol)
  (no-virtual nil :type boolean)
  (override nil :type boolean)
  )

;; Most useful constructor
(defun method-info-new (id name type &optional (defined-in-type nil) (no-virtual nil) (override nil))
  (make-method-info :id id
		    :name name
		    :type type
		    :defined-in-type defined-in-type
		    :no-virtual no-virtual
		    :override override))

;; One line inspector

(defmethod to-str ((this method-info))
  (format nil "Method ~3a: ~20a ~a"
          (method-info-id this)
          (method-info-name this)
          (to-str (method-info-typoe this))))

;; Compare types TODO Not sure it is needed

(defmethod diff ((this method-info) (other method-info))
  (my/with-slots l- (method-info id name type defined-in-type no-virtual override) this
    (my/with-slots r- (method-info id name type defined-in-type no-virtual override) other
      (let ((result ""))
	(when (!= l-id r-id)
	  (string-append! result (format nil "id: ~a vs. ~a~%" l-id r-id)))
	(when (!= l-name r-name)
	  (string-append! result (format nil "name: ~a vs. ~a~%" l-name r-name)))
	(when (!= l-type r-type)
	  (string-append! result (format nil "type: ~a vs. ~a~%" (to-str l-type) (to-str r-type))))
	(when (!= l-defined-in-type r-defined-in-type)
	  (string-append! result (format nil "defined-in-typ: ~a vs. ~a~%" l-defined-in-type r-defined-in-type)))
	(when (!= l-no-virtual r-no-virtual)
	  (string-append! result (format nil "no-virtual: ~a vs. ~a~%" l-no-virtual r-no-virtual)))
	(when (!= l-override r-override)
	  (string-append! result (format nil "overrides: ~a vs. ~a~%" l-override r-override)))
	result))))
   
;; ----------------------------------------------------------------------------
;; The constants
;; ----------------------------------------------------------------------------

;; The offset if the method from begin of methods table

(defconstant GOAL-NEW-METHOD 0)       ;; method ID of GOAL new
(defconstant GOAL-DEL-METHOD 1)       ;; method ID of GOAL delete
(defconstant GOAL-PRINT-METHOD 2)     ;; method ID of GOAL print
(defconstant GOAL-INSPECT-METHOD 3)   ;; method ID of GOAL inspect
(defconstant GOAL-LENGTH-METHOD 4)    ;; method ID of GOAL length
(defconstant GOAL-ASIZE-METHOD 5)     ;; method ID of GOAL size
(defconstant GOAL-COPY-METHOD 6)      ;; method ID of GOAL copy
(defconstant GOAL-RELOC-METHOD 7)     ;; method ID of GOAL relocate
(defconstant GOAL-MEMUSAGE-METHOD 8)  ;; method ID of GOAL mem-usage

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

(defun type-flags-new () (make-type-flags))

(defun type-flags-flag (flags)
  (my/with-slots nil (type-flags size heap-base methods pad) flags
		 (+ (logand size      #xFFFF)
		    (ash (logand heap-base #xFFFF) 16)
		    (ash (logand methods   #xFFFF) 32)
		    (ash (logand pad       #xFFFF) 48))))

;; ----------------------------------------------------------------------------
;; The type
;; ----------------------------------------------------------------------------

;; Default constructor

(defstruct gtype
   (methods (arr-new))                         ; vector<method-info>
   (states (make-hash-table))                  ; hash<string,type-spec>
   (new-method-info nil :type method-info)     ; MethodInfo
   (new-method-info-defined nil :type boolean) ; false
   (generate-inspect t :type boolean)          ; true
   (parent 'none :type symbol)                 ; string?  the parent type (is empty for none and object)
   (name 'none :type symbol)                   ; string?
   (allow-in-runtime t :type boolean)          ; bool? true;
   (runtime-name EMPTY-SYMBOL :type symbol)    ; string?
   (is-boxed nil :type boolean)                ; bool? false  // does this have runtime type information?
   (heap-base 0 :type integer)                 ; int? 0
   )

(defmethod to-str ((this gtype))
  (format nil "[Type] {~a}" (type-name this)))

(defmethod is-reference? ((this gtype))
  (error "Abstract!"))
;; when loading data of typable type into a register, how many bytes do we
;; need?
(defmethod get-load-size ((this gtype))
  (error "Abstract!"))
;; do we need to sign extend when loading?
(defmethod get-load-signed ((this gtype))
  (error "Abstract!"))
;; how much space does typable use in memory? For value types, typable is the
;; same as load size, as value type data is loaded directly into registers.
(defmethod get-size-in-memory ((this gtype))
  (error "Abstract!"))
(defmethod get-offset ((this gtype))
  (error "Abstract!"))
(defmethod get-in-memory-alignment ((this gtype))
  (error "Abstract!"))
(defmethod get-inl-array-stride-align ((this gtype))
  (error "Abstract!"))
(defmethod get-inl-array-start-align ((this gtype))
  (error "Abstract!"))
(defmethod get-preferred-reg-class ((this gtype))
  (error "Abstract!"))


;; Typical constructor

(defun gtype-new (parent name is-boxed heap-base)
  (let ((it (make-gtype)))
    (setf (gtype-parent it) parent)
    (setf (gtype-name it) name)
    (setf (gtype-is-boxed it) is-boxed)
    (setf (gtype-heap-base it) heap-base)
    (setf (gtype-runtime-name it) name)
    it))

(defun gtype-base-type (this)
  (type-parent this))

;; Dsable type for runtime

(defun gtype-disallow-in-runtime (this)
  (setf (gtype-allow-in-runtime this) false))

;; Print information for all methods defined specifically for a type.
;; Does not print inherited methods.

(defmethod to-str ((this gtype))
  (string-join
   (cons (if (gtype-new-method-info-defined this)
             (to-str (gtype-new-method-info this))
             "")
         (map (lambda (m) (to-str m))
              (vector->list (type-methods this))))
   "~%"))


;; Does this type have a parent that makes sense to use? Object and none both
;; don't have meaningful parents.

(defun gtype-has-parent? (this)
  (and (!= (type-name this) 'object)
       (!= (type-parent this) EMPTY-SYMBOL)))

;; Returns the parent of false

(defun gtype-get-parent (this)
  (if (!= (type-name this) 'object)
      (type-parent this)
      'none))

;; Get a method that is defined specifically for this type. Returns if it was
;; found or not.

(defun type-get-my-method (this name)
  (find name (type-methods this) :key #'method-info-name))

;; Get a method that is defined specifically in this type by id. Returns if it
;; was found or not.

(defun gtype-get-my-method-by-id (this id)
  (assert (> id 0))  ;; 0 is new, should use explicit new method functions instead.
  (find-item id (type-methods this) :key #'method-info-id)) 

;; Get the last method defined specifically for this type. Returns if there were
;; any methods defined specifically for this type or not.

(defun gtype-get-my-last-method (this)
  (let ((col (type-methods this)))
    (if (== 0 (arr-count coll))
	nil
	(arr-last col))))

;; Get the new method defined specifically for this type. Returns if there is a
;; new method specific to this type or not.

(defun gtype-get-my-new-method (this)
  (if (type-new-method-info-defined this)
      (type-new-method-info this)
      nil))

;; Add a method defined specifically for this type.

(defun type-add-method (this info)
  (let* ((id  (method-info-id info))
	 (col (type-methods this))
	 (len (arr-count coll)))
    (dotimes (i len)
      ;; iterate backward
      (let ((it (arr-ref col (- len i))))
        (when (not (method-info-override it))
          (assert (== id (1+ (method-info-id it))))
          (return t) ;; stop iterating
          )))
    (arr-push (type-methods this) info)
    info))

;; Add a NEW method defined specifically for this type. The name of this
;; function is confusing - this is specific to the method named NEW.

(defun type-add-new-method (this info)
  (assert (== (method-info-name info) 'new))
  (setf (type-new-method-info-defined this) t)
  (setf (type-new-method-info this) info)
  info)

(defun type-set-runtime-type (this name)
  (setf (type-runtime-name this) name))


;; ----------------------------------------------------------------------------
;; Statens
;; ----------------------------------------------------------------------------

;; Add the state to the class

(defun gtype-add-state (this name type)
  (let ((state (hash-ref (type-states this) name nil)))
	(when state
	  (error (format nil "State ~a is multiply defined" name)))
	(hash-set! (type-states this) name type)))

(defun type-find-state (this name)
  (hash-ref (type-states this) name nil))

;; ----------------------------------------------------------------------------
;; The type
;; ----------------------------------------------------------------------------

(defun incompatible-diff (expected-type-name other)
  (format nil "diff is not implemented between ~a and ~a~%"
          expected-type-name other))

(defmethod diff ((this gtype) (other gtype))
  (my/with-slots l- (type methods states new-method-info new-method-info-defined generate-inspect parent
			  name allow-in-runtime runtime-name is-boxed heap-base) this
    (my/with-slots r- (type methods states new-method-info new-method-info-defined generate-inspect parent
			    name allow-in-runtime runtime-name is-boxed heap-base) other
      (let ((result ""))
	;;; Check methods count
	(unless (== l-methods r-methods)
	  (let* ((l-methods.size (gvector-count l-methods))
		 (r-methods.size (gvector-count r-methods))
		 (min-methods-size (min l-methods.size r-methods.size)))
	    (when (!= l-methods.size r-methods.size)
	      (string-append! result (format nil "Number of additional methods ~a vs. ~a~%"
					     l-methods.size r-methods.size)))
	    ;; Check methods one to one
	    (dotimes (i (in-range min-methods-size))
	      (let ((l (gvector-ref l-methods i))
		    (r (gvector-ref r-methods i)))
		(when (!= l r)
		  (string-append! result (format nil "Method def ~a (~a/~a):~%~a~%"
						 i (method-info-name l) (method-info-name r)
						 (method-info-diff l r))))))))
	;;; Check states
	(unless (== l-states r-states)
	  "States are different:~%"
	  (hash-map
	   (type-states this)
	   (lambda (name l-state)
	     (let ((r-state (type-find-state other name)))
	       (cond
		 ((not r-state)
		  (string-append! result (format nil "  ~a is in one, but not the other-~%" name)))
		 ((notequal? l-state r-state)
		  (string-append! result (format nil "  ~a is defined differently: ~a vs ~a~%"
						 name  (to-str l-state) (to-str r-state))))))))

	  (hash-map
	   (type-states other)
	   (lambda (name l-state)
	     (let ((r-state (type-find-state this name)))
	       (when (not r-state)
		 (string-append! result (format nil "  ~a is in one, but not the other-~%" name)))))))

	;; Check other params
	(when (!= l-new-method-info r-new-method-info)
	  (string-append! result (format nil "new-method-info: ~a vs ~a~%" l-new-method-info  r-new-method-info)))
	(when (!= l-new-method-info-defined r-new-method-info-defined)
	  (string-append! result (format nil "new-method-info-defined: ~a vs. ~a~%" l-new-method-info-defined  r-new-method-info-defined)))
	(when (!= l-parent r-parent)
	  (string-append! result (format nil "parent: ~a vs. ~a~%" l-parent r-parent)))
	(when (!= l-name r-name)
	  (string-append! result (format nil "name: ~a vs. ~a~%" l-name r-name)))
	(when (!= l-allow-in-runtime r-allow-in-runtime)
	  (string-append! result (format nil "allow-in-runtime: ~a vs. ~a~%" l-allow-in-runtime r-allow-in-runtime)))
	(when (!= l-runtime-name r-runtime-name)
	  (string-append! result (format nil "runtime-name: ~a vs. ~a~%" l-runtime-name r-runtime-name)))
	(when (!= l-is-boxed r-is-boxed)
	  (string-append! result (format nil "is-boxed: ~a vs. ~a~%" l-is-boxed r-is-boxed)))
	(when (!= l-heap-base r-heap-base)
	  (string-append! result (format nil "heap-base: ~a vs. ~a~%" l-heap-base r-heap-base)))
	(when (!= l-generate-inspect r-generate-inspect)
	  (string-append! result (format "generate-inspect: ~a vs. ~a~%" l-generate-inspect r-generate-inspect)))
	
	result)
      )))
