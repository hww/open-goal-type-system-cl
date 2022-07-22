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
;; Handler helpers
;; ==============================================================================

(defun handler-name-to-kind (name)
  ;;(-> symbol? state-handler?)
  (cond
    ((== name "enter")
     :state-handler-enter)
    ((== name "exit")
     :state-handler-exit)
    ((== name "code")
     :state-handler-code)
    ((== name "event")
     :state-handler-event)
    ((== name "trans")
     :state-handler-trans)
    ((== name "post")
     :state-handler-post)
    (else
     (assert false))))

(defun handler-kind-to-name (kind)
  ;;(-> state-handler? symbol?)
  (symbol-name kind))

(defun get-state-handler-type (handler state-type)
  ;;(-> (or/c state-handler? symbol?) type-spec? type-spec?)
  (if (symbol? handler)
      (get-state-handler-type-impl (handler-name-to-kind handler) state-type)
      (get-state-handler-type-impl handler state-type)))

(defun get-state-handler-type-impl (kind state-type)
  ;;(-> state-handler? type-spec? type-spec?)
  (let ((result nil))
    (cond
      ((== kind :state-handler-code)
       (set! result (state-to-go-function state-type (typespec-new "none"))))
      ((== kind :state-handler-enter)
       (set! result (state-to-go-function state-type (typespec-new "none"))))
      ((or (== kind :state-handler-trans)
	   (== kind :state-handler-post)
	   (== kind :state-handler-exit))
       (set! result (typespec-new 'function (list (typespec-new "none")))))
      ((== kind :state-handler-event)
       (set! result (typespec-new 'function (list (typespec-new 'process)
						  (typespec-new 'int)
						  (typespec-new 'symbol)
						  (typespec-new 'event-message-block)
						  (typespec-new 'object)))))
      (else
       (assert false)))
    (typespec-add-or-modify-tag result 'behavior (base-type (typespec-args-last state-type)))
    result))

;; ==============================================================================
;; Make state
;; ==============================================================================

(defun get-state-type-from-enter-and-code (enter-func-type
					   code-func-type
					   proc-type)
  ;;(-> type-spec? type-spec? type-spec? type-spec?)
  ;; For enter-func
  ;; Check of the base type is function and there are more than 0 arguments
  (let ((enter-real-func
	  (and  (== (base-type enter-func-type) 'function)
		(> (typespec-args-count enter-func-type) 0)))
	;; For code-func
	;; Check of the base type is function and there are more than 0 arguments
	(code-real-func
	  (and (== (base-type code-func-type) 'function)
	       (> (typespec-args-count code-func-type) 0))))

    (cond
      ((and enter-real-func code-real-func)
       (let* ((result (typespec-new 'state))
	      (ent-cnt (typespec-args-count enter-func-type))
	      (cod-cnt (typespec-args-count code-func-type))
	      (arg-min (1- (min ent-cnt cod-cnt))))

	 (dotimes (i arg-min)
	   (typespec-args-add result (lowest-common-ancestor
				      (typespec-args-ref enter-func-type i)
				      (typespec-args-ref code-func-type i))))

	 (loop for i from arg-min below (1- ent-cnt) do
	   (typespec-args-add result (typespec-args-ref enter-func-type i)))

	 (loop for i from arg-min below (1- cod-cnt) do
	   (typespec-args-add result (typespec-args-ref code-func-type i)))

	 (typespec-args-add result proc-type)
	 result))
       ;;
       (enter-real-func
	(func-to-state-type enter-func-type proc-type))
       ;;
       (code-real-func
	(func-to-state-type code-func-type proc-type))
       ;;
       (else
	(typespec-new "none")))))

;; Convert a (state <blah> ...) to the function required to go. Must be state.

(defun state-to-go-function (state-type return-type)
  ;;(-> type-spec? type-spec? type-spec?)
  (assert (== (base-type state-type) 'state))
  (let ((arg-types (append (butlast (typespec-args state-type) 1)
			   (list return-type))))
    (typespec-new "function" arg-types "behaviour" (base-type (typespec-args-last state-type)))))

;; Make a 'state with same arguments as 'func-type and result 'proc-type

(defun func-to-state-type (func-type proc-type)
  ;;(-> type-spec? type-spec? type-spec?)
  (let ((arg-types (append (butlast (typespec-args func-type) 1)
			   (list proc-type))))
    (typespec-new "state" arg-types)))


;; Make a 'state with same arguments as 'func-type and new result 'proc-type

(defun get-state-type-from-func (func-type proc-type)
  ;;(-> type-spec? type-spec? type-spec?)
  ;; Check if this is a function with more than 0 arguments
  (let ((real-func (and (== (base-type func-type) 'function)
			(> (typespec-args-count func-type) 0))))
    (cond
      (real-func
       (let ((arg-types (append (butlast (typespec-args func-type) 1)
				(list proc-type))))
	 (typespec-new "syaye" arg-types)))
      (else
       (typespec-new 'none)))))
