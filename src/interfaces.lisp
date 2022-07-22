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
(defconstant +pointer-size+ 4)
(defconstant +structure-alignment+ 16)

;; globaly used empty symbol

(defconstant +empty-symbol+ :||)

;; the register type

(defconstant +reg-class-gpr-64+ 0)
(defconstant +reg-class-float+ 1)
(defconstant +reg-class-int-128+ 2)
(defconstant +reg-class-vector_float+ 3)
(defconstant +reg-class-invalid+ 4)

;; the offset if the method from begin of methods table

(defconstant +goal-new-method+ 0)       ;; method+ id of goal new
(defconstant +goal-del-method+ 1)       ;; method+ id of goal delete
(defconstant +goal-print-method+ 2)     ;; method+ id of goal print
(defconstant +goal-inspect-method+ 3)   ;; method+ id of goal inspect
(defconstant +goal-length-method+ 4)    ;; method+ id of goal length
(defconstant +goal-asize-method+ 5)     ;; method+ id of goal size
(defconstant +goal-copy-method+ 6)      ;; method+ id of goal copy
(defconstant +goal-reloc-method+ 7)     ;; method+ id of goal relocate
(defconstant +goal-memusage-method+ 8)  ;; method+ id of goal mem-usage

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
