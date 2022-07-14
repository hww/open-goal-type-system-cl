(in-package :type-system)

;; ----------------------------------------------------------------------------
;;
;; Valeriya P.
;; https://gist.github.com/hww
;; _______ ________ ________
;; |   |   |  |  |  |  |  |  |
;; |       |  |  |  |  |  |  |
;; |___|___|________|________|
;;
;; ----------------------------------------------------------------------------


;; -----------------------------------------------------------------------------
;; The type tag
;;
;; Every type may have a list of tagged values
;; for eample :align 32. This structure represent
;; single tag-value pair
;; -----------------------------------------------------------------------------

(defstruct type-tag name value)

(defun type-tag-new (&rest it)
  "Construct single tagged value"
  (cond
    ((null it) (make-type-tag :name :none :value nil))
    ((listp it)
     (unless (symbolp (car it))
       (error (format nil "Unexpeced type-tag key ~a" it)))
     (when (null (cdr it))
       (error (format nil "Undefined type-tag value ~a" it)))
     (make-type-tag :name (car it) :value (cadr it)))
    (else
     (error (format nil "Unexpeced type-tag syntax ~a" it)))))

(defun type-tag-new* (lst)
  "Construct the list of tags"
  (if (null lst)
      nil
      (cons (type-tag-new (car lst) (cadr lst)) (type-tag-new* (cddr lst)))))

;; -----------------------------------------------------------------------------
;; The type specification
;;
;; @param (type symbol?) name - the name
;; @param (arguments list?) args - the list of typespec arguments
;; @param (arguments type-tag?) tags - the list of type-tag items
;; -----------------------------------------------------------------------------
;; There are two cases possible: with arguments and withot
;; The example of types
;; none
;; int
;; int32
;; pointer
;; (pointer int32)
;; (function int (pointer int32) (pointer int32) (pointer int32) none)
;; -----------------------------------------------------------------------------

(defstruct typespec type args tags)

;; Construct

(defun typespec-new (&optional (type :none) (args nil) &rest tags)
  (make-typespec :type type :args args :tags (type-tag-new* tags)))

;; Diff

(defun typespec-diff (this other)
  (list->string (sexp-diff this other)))

;; The inspector for type spec

(defun typespec-inspect (this)
  (if (and (typespec-args-empty? this) (typespec-tags-empty? this))
      (format nil "~a" (typespec-type this))
      (progn
	
	(apply 'string-append
	       (append
		(list (format nil "(~a" (typespec-type this)))
		(map 'list
		     (lambda (it) (string-append " " (typespec-inspect it)))
		     (typespec-args this))
		(map 'list
		     (lambda (it) (format nil " ~a ~a" (type-tag-name it) (type-tag-value it)))
		     (typespec-tags this))
		(list ")"))))))

;; alias fucntion

(defun typespec-basetype (ts)
  (typespec-type ts))

;; -----------------------------------------------------------------------------
;;
;; -----------------------------------------------------------------------------

(defun typespec-substitute-for-method-call (this method-type)
  (define result (typespec-new))
  (if (== (typespec-type this) '-type-)
      (setf (typespec-type result) method-type)
      (setf (typespec-type result) (typespec-type this)))

  (when (not (null? (typespec-args this)))
    (setf
     (typespec-args result)
     (map 'list
	  (lambda (arg) (typespec-substitute-for-method-call arg method-type))
	  (typespec-args this))))
  result)

(defun typespec-is-compatible-child-method (this  implementation child-type)
  ;; the base types should be same or if <this> ins the type -type-
  ;; and <implementation> type eqaul child type
  (defvar ok (or (== (typespec-type implementation) (typespec-type this))
                 (and (== (typespec-type this) '_type_)
                      (== (typespec-type implementation)  child-type))))
  (cond
    ((or (not ok)
	 (!= (typespec-args-count implementation)
	     (typespec-args-count this)))
     ;; if previous test failed and if args count different
     nil)
    (else
     ;; all looks fine, then check each the argument
     (loop for i from 0 to (typespec-args-count this)
	   do
	      (when (not (typespec-is-compatible-child-method (typespec-args-ref this i)
							      (typespec-args-ref implementation i)
							      child-type))
		;; the argument is not compatible
		return nil))
     ;; no mor arguments
     t)))

;; -----------------------------------------------------------------------------
;; Tags API
;; -----------------------------------------------------------------------------

;; Find the tag with the name or return null

(defun typespec-try-get-tag (this tag)
  (loop for it in (typespec-tags this)
	do
	   (when (equalp tag (type-tag-name it))
	       (return it))))

;; Make error if there is this tag

(defun typespec-add-new-tag (this tag value)
  (let ((item (typespec-try-get-tag this tag)))
    (cond
      ((not item)
       (setf (typespec-tags this) (cons (make-type-tag :name tag :value value)
                                       (typespec-tags this))))
      (else
       (error (format nil "Attempted to add a duplicate tag ~a to typespec." tag))))))

;; Make an error if the tag is not found

(defun typespec-modify-tag (this tag value)
  (let ((item (typespec-try-get-tag this tag)))
    (cond
      ((not item)   (error (format nil "Attempted to modify non existing tag ~a to typespec ~a." tag this)))
      (else         (setf (type-tag-value item) value)))))

;; Find amd modify tag or add new for

(defun typespec-add-or-modify-tag (this tag value)
  (let ((item (typespec-try-get-tag this tag)))
    (cond
      ((not item)   (typespec-add-new-tag this tag value))
      (else         (setf (type-tag-value item) value)))))

;; Make exception if there are no this tag

(defun typespec-safe-find-tag (this tag)
  (let ((item (typespec-try-get-tag this tag)))
    (cond
      ((not item)   (error (format nil "Attempted to find non existing tag ~a to typespec." tag)))
      (else         item))))

;; Check if the tags list empty

(defun typespec-tags-empty? (this)
  (null? (typespec-tags this)))

;; Return number of tags

(defun typespec-tags-couny (this)
  (length (typespec-tags this)))

;; -----------------------------------------------------------------------------
;; The arguments api
;; -----------------------------------------------------------------------------

;; Check if the arguments list is empty

(defun typespec-args-count (this)
  (length (typespec-args this)))

;; Reference to the argument

(defun typespec-args-ref (this i)
  (list-ref (typespec-args this) i))

;; Reference to the argument

(defun typespec-args-last (this i)
  (null? (typespec-args this)
         nil
         (last (typespec-args this))))

(defun typespec-args-first (this i)
  (null? (typespec-args this)
         nil
         (first (typespec-args this))))

;; Check if the arguments list is empty

(defun typespec-args-empty? (this)
  (null? (typespec-args this)))

;; Add the argument

(defun typespec-args-add (this arg)
  (setf (typespec-args this) (append (typespec-args this) (list arg))))

;; No arguments

(defun typespec-empty? (this)
  (== 0 (length (typespec-args this))))




