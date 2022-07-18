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

(defconstant REG-CLASS-GPR-64 0)
(defconstant REG-CLASS-FLOAT 1)
(defconstant REG-CLASS-INT-128 2)
(defconstant REG-CLASS-VECTOR_FLOAT 3)
(defconstant REG-CLASS-INVALID 4)

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
