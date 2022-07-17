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

(require "type-spec.rkt" "vmc-lib.rkt" "type-system.rkt")
(require rebellion/type/enum)
(require racket/contract racket/list)
;;
;; This contains type system utilities related to state and process
;;

(define-struct state-handler (val) #:transparent)

(define StateHandler::ENTER (state-handler 'enter))
(define StateHandler::EXIT  (state-handler 'exit))
(define StateHandler::TRANS (state-handler 'trans))
(define StateHandler::POST  (state-handler 'post))
(define StateHandler::EVENT (state-handler 'event))
(define StateHandler::CODE  (state-handler 'code))

;; ==============================================================================
;; Handler helpers
;; ==============================================================================

(define/contract (handler-name-to-kind name)
  (-> symbol? state-handler?)
  (cond
    ((== name 'enter)
     StateHandler::ENTER)
    ((== name 'exit)
     StateHandler::EXIT)
    ((== name 'code)
     StateHandler::CODE)
    ((== name 'event)
     StateHandler::EVENT)
    ((== name 'trans)
     StateHandler::TRANS)
    ((== name 'post)
     StateHandler::POST)
    (else
     (assert false))))

(define/contract (handler-kind-to-name kind)
  (-> state-handler? symbol?)
  (state-handler-val kind))

(define/contract (get-state-handler-type handler state-type)
  (-> (or/c state-handler? symbol?) type-spec? type-spec?)
  (if (symbol? handler)
      (get-state-handler-type-impl (handler-name-to-kind handler) state-type)
      (get-state-handler-type-impl handler state-type)))

(define/contract (get-state-handler-type-impl kind state-type)
  (-> state-handler? type-spec? type-spec?)
  (define result #f)
  (cond
    ((== kind StateHandler::CODE)
     (set! result (state-to-go-function state-type (type-spec-new 'none))))
    ((== kind StateHandler::ENTER)
     (set! result (state-to-go-function state-type (type-spec-new 'none))))
    ((or (== kind StateHandler::TRANS)
         (== kind StateHandler::POST)
         (== kind StateHandler::EXIT))
     (set! result (type-spec-new 'function (list (type-spec-new 'none)))))
    ((== kind StateHandler::EVENT)
     (set! result (type-spec-new 'function (list (type-spec-new 'process)
                                                    (type-spec-new 'int)
                                                    (type-spec-new 'symbol)
                                                    (type-spec-new 'event-message-block)
                                                    (type-spec-new 'object)))))
    (else
     (assert false)))
  (type-spec-add-or-modify-tag result 'behavior (base-type (type-spec-args-last state-type)))
  result)

;; ==============================================================================
;; Make state
;; ==============================================================================

(define/contract (get-state-type-from-enter-and-code enter-func-type
                                                     code-func-type
                                                     proc-type)
  (-> type-spec? type-spec? type-spec? type-spec?)
  ;; For enter-func
  ;; Check of the base type is function and there are more than 0 arguments
  (define enter-real-func
    (and  (== (base-type enter-func-type) 'function)
          (> (type-spec-args-count enter-func-type) 0)))
  ;; For code-func
  ;; Check of the base type is function and there are more than 0 arguments
  (define code-real-func
    (and (== (base-type code-func-type) 'function)
         (> (type-spec-args-count code-func-type) 0)))

  (cond
    ((and enter-real-func code-real-func)
     (define i 0)
     (define result (type-spec-new 'state))
     (define ent-cnt (type-spec-args-count enter-func-type))
     (define cod-cnt (type-spec-args-count code-func-type))
     (define arg-min (1- (min ent-cnt cod-cnt)))

     (for ((i (in-range 0 arg-min)))
       (type-spec-args-add result (lowest-common-ancestor (type-spec-args-ref enter-func-type i)
                                                     (type-spec-args-ref code-func-type i))))

     (for ((i (in-range arg-min (1- ent-cnt))))
       (type-spec-args-add result (type-spec-args-ref enter-func-type i)))

     (for ((i (in-range arg-min (1- cod-cnt))))
       (type-spec-args-add result (type-spec-args-ref code-func-type i)))

     (type-spec-args-add result proc-type)
     result)
    (enter-real-func
     (func-to-state-type enter-func-type proc-type))
    (code-real-func
     (func-to-state-type code-func-type proc-type))
    (else
     (type-spec-new 'none))))

;; Convert a (state <blah> ...) to the function required to go. Must be state.

(define/contract (state-to-go-function state-type return-type)
  (-> type-spec? type-spec? type-spec?)
  (assert (== (base-type state-type) 'state))
  (define arg-types (append (drop-right (type-spec-args state-type) 1)
                            (list return-type)))
  (type-spec-new 'function arg-types 'behaviour (base-type (type-spec-args-last state-type))))

;; Make a 'state with same arguments as 'func-type and result 'proc-type

(define/contract (func-to-state-type func-type proc-type)
  (-> type-spec? type-spec? type-spec?)
  (define arg-types (append (drop-right (type-spec-args func-type) 1)
                            (list proc-type)))
  (type-spec-new 'state arg-types))


;; Make a 'state with same arguments as 'func-type and new result 'proc-type

(define/contract (get-state-type-from-func func-type proc-type)
  (-> type-spec? type-spec? type-spec?)
  ;; Check if this is a function with more than 0 arguments
  (define real-func (and (== (base-type func-type) 'function)
                         (> (type-spec-args-count func-type) 0)))
  (cond
    (real-func
       (define arg-types (append (drop-right (type-spec-args func-type) 1)
                                 (list proc-type)))
       (type-spec-new 'syaye arg-types))
    (else
     (type-spec-new 'none))))
