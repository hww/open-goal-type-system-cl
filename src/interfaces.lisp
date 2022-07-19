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

(in-package :type-system/interfaces)
;(defun xxx () 1000)
(defconstant POINTER-SIZE 4)
(defconstant STRUCTURE-ALIGNMENT 16)

;; Globaly used empty symbol

(defconstant EMPTY-SYMBOL :||)

;; The register type

(defconstant REG-CLASS-GPR-64 0)
(defconstant REG-CLASS-FLOAT 1)
(defconstant REG-CLASS-INT-128 2)
(defconstant REG-CLASS-VECTOR_FLOAT 3)
(defconstant REG-CLASS-INVALID 4)

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

;; ==============================================================================
;; Inspect the object
;; ==============================================================================

(defgeneric to-str (this)
  (:method (this) (error "Anstract")))

;; ==============================================================================
;; To make a way to see a difference TODO redundant?
;; ==============================================================================

(defgeneric diff (this other)
  (:method (this other) (error "Anstract")))

(defgeneric compare (comparable othe)
  (:method (this other) (error "Anstract")))

;; ==============================================================================
;; The virtual interface of type
;; ==============================================================================

(defgeneric is-reference? (this)
  (:method (this) (error "Anstract")))

;; do we need to sign extend when loading?
(defgeneric get-load-signed (this)
  (:method (this) (error "Anstract")))
;; how much space does typable use in memory?
;; For value types, typable is the same as load size, as
;; value type data is loaded directly into registers.
(defgeneric get-size-in-memory (this)
  (:method (this) (error "Anstract")))
(defgeneric get-offset (this)
  (:method (this) (error "Anstract")))
(defgeneric get-in-memory-alignment (this)
  (:method (this) (error "Anstract")))
(defgeneric get-inl-array-stride-align (this)
  (:method (this) (error "Anstract")))
(defgeneric get-inl-array-start-align (this)
  (:method (this) (error "Anstract")))
(defgeneric get-preferred-reg-class (this)
  (:method (this) (error "Anstract")))
