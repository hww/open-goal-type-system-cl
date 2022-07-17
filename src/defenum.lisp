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

(require racket/contract
         racket/list)

(require "vmc-lib.rkt"
         "interfaces.rkt"
         "type-spec.rkt"
         "basic-types.rkt"
         "builting-types.rkt"
         "deftype.rkt"
         "type-system.rkt"
         "type.rkt"
         "deftype.rkt")

(provide parse-defenum)


;; ==============================================================================
;; Utilities
;; ==============================================================================

(define/contract (is-key? stx)
  (-> syntax? boolean?)
  (define dat (syntax-e stx))
  (and (symbol? dat) (== ":" (string-ref (symbol->string dat) 0))))

(define/contract (get-key stx)
  (-> syntax? (or/c #f symbol?))
  (define dat (syntax-e stx))
  (if (and (symbol? dat) (== #\: (string-ref (symbol->string dat) 0)))
      dat
      #f))

;; Find tag in the tagsl list
;; list - (#'(tag . value) ...)

(define/contract (ts:get-tag list tag default (make-error #f))
  (->* ((listof syntax?) symbol? any/c) (boolean?) any)
  (let :loop: ((rest list))
    (let ((e (syntax-e (car rest))))
      (cond
        ((null? e)
         (if make-error
             (error (format "Can't find tag `~a` in the list `~a`" tag list ))
             default))
        (else
         (if (eq? tag (syntax->datum (car e)))
             (cdr e)
             (:loop: (cdr rest))))))))

;; Verify tags
;; ids - (#'id ...)

(define/contract (ts:verify-ids ids tags)
  (-> (listof syntax?) (listof symbol?) void)
  (let :loop: ((rest ids))
    (unless (null? rest)
      (let ((d (syntax->datum (car rest))))
        (if (not (member d tags))
            (error (format "Unknown option `~a` for defenum." d))
            (:loop: (cdr rest)))))))


;; Verify uniques of IDs in the list
;;
;; ids - (#'id ...)

(define/contract (ts:verify-unique ids)
  (-> (listof syntax?) void)
  (let* ((ids (map (lambda (stx) (syntax->datum stx)) ids))
         (dup (check-duplicates  ids)))
    (when dup
      (error (format "Entry `~a` appears multiple times." dup)))))

;; ==============================================================================
;;
;; Parse defenum
;;
;; (defenum gs-psm
;;     :bitfield #f
;;     :type uint8
;;     (ct32 0)
;;     (ct24 1)
;;     (ct16 2)
;;     ....)
;;
;; The option copy-entries allow to ommit the items
;; (defenum game-option-menu
;;  :type int32
;;  :copy-entries progress-screen)
;; ==============================================================================

(define/contract (parse-defenum defenum ts)
  (-> (listof syntax?) type-system? enum-type?)
  ;; helper
  (define/contract (is-type expected actual)
    (-> symbol? type-spec? boolean?)
    (tc ts (make-typespec ts expected) actual))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; default enum type will be int32.
  (define par-type (make-typespec ts 'int32))
  (define is-bitfield false)
  (define entries (make-hash))
  (define iter defenum)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (define enum-name-obj (car iter))
  (define enum-name (syntax-e enum-name-obj))
  (set! iter (cdr iter))

  (unless (symbol? enum-name)
    (error (format "defenum must be given a symbol as its name but found ~a" enum-name)))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (define other-info #f)
  (define option-name #f)
  (define option-value #f)
  (define current (car iter))
  ;; Iterate over the options
  (let :loop: ()
    (set! option-name (get-key current))
    (set! iter (cdr iter))
    (when option-name
      (set! option-value (car iter))
      (set! iter (cdr iter))
      (cond
        ((== option-name ':type)
         (set! par-type (parse-type-spec ts option-value)))
        ((== option-name ':bitfield)
         (cond
           ((== (syntax->datum option-value) #t)
            (set! is-bitfield true))
           ((== (syntax->datum option-value) #f)
            (set! is-bitfield false))
           (else
            (error (format "Invalid option `~a` to :bitfield option.\n" (inspect option-value))))))
        ((== option-name ':copy-entries)
         (set! other-info (try-enum-lookup ts (parse-type-spec ts option-value)))
         (unless other-info
           (error (format
                   "Cannot copy entries from `~a`, it is not a valid enum type" (inspect option-value))))
         (hash-for-each (enum-type-entries other-info)
                        (lambda (k v)
                               (when (hash-ref entries k #f)
                                 (error (format "Entry `~a` appears multiple times" k)))
                               (hash-set! entries k v))))
        (else
         (error (format "Unknown option `~a` for defenum.\n" option-name))))
      (when (pair? iter)
        (set! current (car iter))
        (:loop:)))) ;; End of iteration
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Find the base type
  (define type (lookup-type ts par-type))
  (define highest -1)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Iterate with items
  ;(printf "parse-defenum\n ~a\n" iter)
  (let :loop: ()
    (unless (null? iter)
      ;; If there is more items in the list
      (let* ((field (syntax-e (car iter)))
             (entry-name (syntax-e (car field)))
             (item (hash-ref entries entry-name #f)))

        (when item
          (error (format "Entry `~a` appears multiple times." entry-name)))

        (let ((rest (cdr field)))

          (cond
            ((not (null? rest))
             (let ((item-value (syntax-e (car rest))))
               (unless (integer? item-value)
                 (error (format "Expected integer for enum item-value, got `~a`\n" item-value)))
               (unless (integer-fits? item-value  (get-load-size type) (get-load-signed type))
                 (error (format "Integer `~a` does not fit inside a `~a`\n" item-value (type-name type))))
               (when (> (hash-count entries) 0)
                 (set! highest item-value))

               (set! highest (max highest item-value))

               (set! rest (cdr rest))
               (unless (null? rest)
                 (error (format "Got too many items in defenum `~a` entry `~a`\n" enum-name entry-name)))

               (hash-set! entries entry-name item-value)))
            (else
             (+1! highest)
             (hash-set! entries entry-name highest)))))

      (set! iter (cdr iter))
      (:loop:)))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (cond
    ((is-type 'integer par-type)
     (let* ((parent (get-type-of-type ts value-type? (base-type par-type)))
            (new-type (enum-type-new parent enum-name is-bitfield entries)))
       (type-set-runtime-type new-type (type-runtime-name parent))
       (add-type ts enum-name new-type)
       new-type))
    (else
     (error (format "Creating an enum with type `~a` is not allowed or not supported yet."
                     (inspect par-type))))))

;; TEST +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(module+ test
  (require rackunit)
  (let ((tsys (type-system-new)))
    (add-builtin-types tsys)
    ;; - - - - - - - - - - - - - - - - - -
    ;; Simple test
    (cond
      (#f ;; <- make true for simple test
       (define gs-psm-exp #'(defenum
                              gs-psm
                              :type uint32
                              :bitfield #f
                              (ct32 0)
                              (ct24 1)
                              (ct16 2)))
       (define gs-psm (parse-defenum (cdr (syntax-e gs-psm-exp)) tsys))
       (check-equal? (inspect gs-psm) "[EnumType] gs-psm"))
      (else
       ;; Large test
       (printf "\n[START] Defenum test\n")
       (printf "Reading file...\n")
       (define expr (for-file-parse "goalc-all-types.gc"))
       (printf "Parsing defenums...\n")
       (define counter 0)
       (for-each-in-list
        expr
        (lambda (e)
          (define fe (syntax-e e))
          (when (list? fe)
            (define first (syntax-e (car fe)))
            (when (== first 'defenum)
              (parse-defenum (cdr fe) tsys)
              (+1! counter)))))
       (printf "[END] ~a defenums parsing\n" counter)))))
