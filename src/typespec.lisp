(in-package :type-system/typespec)



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

(defstruct type-tag
  (name nil :type (or null string))
  (value nil :type (or null string)))

(defun type-tag-new (&rest it)
  "Construct single tagged value"
  (cond
    ((null it) (make-type-tag :name "none" :value nil))
    ((listp it)
     (let* ((name-obj (car it))
            (val-obj (cadr it)))
       (unless (or (symbolp name-obj) (stringp name-obj))
         (error (format nil "Unexpeced type-tag key ~a" it)))
       (when (null (cdr it))
         (error (format nil "Undefined type-tag value ~a" it)))
       (unless (or (symbolp val-obj) (stringp val-obj))
         (error (format nil "Unexpeced type-tag value ~a" it)))
       (let* ((name-str (if (symbolp name-obj) (symbol-name name-obj) name-obj))
              (val-str (if (symbolp val-obj) (symbol-name val-obj) val-obj)))
         (make-type-tag :name name-str :value val-str))))))

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

(defstruct typespec
  (type nil :type (or null string))
  (args nil :type list)
  (tags nil :type list)
  )

;; Construct

(defun typespec-new (&optional (type "none") (args nil) &rest tags)
  (when (and
	 (== 1 (length tags))
	 (null (car tags))
	 (setf tags nil)))
  (when (symbolp type)
    (setf type (symbol-name type)))
  (make-typespec :type type :args args :tags (type-tag-new* tags)))

;; Diff

(defmethod diff ((this typespec) (other typespec))
 ; (list--to-string (sexp-diff this other))
  "TODO")

;; The inspector for type spec

(defmethod to-str ((this typespec))
  (if (and (typespec-args-empty? this) (typespec-tags-empty? this))
      (format nil "~a" (typespec-type this))
      (let ((result ""))
        (string-append! result (format nil "(~a (" (typespec-type this)))
        (string-append! result (string-join (map 'list (lambda (it) (to-str it))
                                                 (typespec-args this))
                                            " "))
        (string-append! result ")")
        (unless (typespec-tags-empty? this)
          (map 'list
               (lambda (it)
                 (string-append! result (format nil " :~a ~a" (type-tag-name it) (type-tag-value it))))
               (typespec-tags this)))
        (string-append! result ")"))))

;; alias fucntion

(defun typespec-basetype (ts)
  (typespec-type ts))

;; -----------------------------------------------------------------------------
;;
;; -----------------------------------------------------------------------------

(defmethod typespec-substitute-for-method-call ((this typespec) (method-type string))
  (let ((result (typespec-new)))
    (if (== (typespec-type this) '-type-)
	(setf (typespec-type result) method-type)
	(setf (typespec-type result) (typespec-type this)))

    (when (not (null? (typespec-args this)))
      (setf
       (typespec-args result)
       (map 'list
	    (lambda (arg) (typespec-substitute-for-method-call arg method-type))
	    (typespec-args this))))
    result))

(defmethod typespec-is-compatible-child-method ((this typespec) (implementation typespec) (child-type string))
  ;; the base types should be same or if <this> ins the type -type-
  ;; and <implementation> type eqaul child type
  (let ((ok (or (== (typespec-type implementation) (typespec-type this))
                (and (== (typespec-type this) "_type_")
                     (== (typespec-type implementation)  child-type)))))
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
                  (return nil)))
       ;; no mor arguments
       t))))

;; -----------------------------------------------------------------------------
;; Tags API
;; -----------------------------------------------------------------------------

;; Find the tag with the name or return null

(defmethod typespec-try-get-tag ((this typespec) tag)
  (let ((n (stringify tag)))
    (loop for it in (typespec-tags this)
          do
             (when (equalp n (type-tag-name it))
               (return it)))))

;; Make error if there is this tag

(defmethod typespec-add-new-tag ((this typespec) tag value)
  (let ((item (typespec-try-get-tag this tag)))
    (cond
      ((not item)
       (append! (typespec-tags this)
		(make-type-tag :name (stringify tag) :value (stringify value))))
      (else
       (error (format nil "Attempted to add a duplicate tag ~a to typespec." tag))))))

;; Make an error if the tag is not found

(defmethod typespec-modify-tag ((this typespec) tag value)
  (let ((item (typespec-try-get-tag this tag)))
    (cond
      ((not item)   (error (format nil "Attempted to modify non existing tag ~a to typespec ~a." tag this)))
      (else         (setf (type-tag-value item) value)))))

;; Find amd modify tag or add new for

(defmethod typespec-add-or-modify-tag ((this typespec) tag value)
  (let ((item (typespec-try-get-tag this tag)))
    (cond
      ((not item)   (typespec-add-new-tag this tag value))
      (else         (setf (type-tag-value item) value)))))

;; Make exception if there are no this tag

(defmethod typespec-safe-find-tag ((this typespec) tag)
  (let ((item (typespec-try-get-tag this tag)))
    (cond
      ((not item)   (error (format nil "Attempted to find non existing tag ~a to typespec." tag)))
      (else         item))))

;; Check if the tags list empty

(defmethod typespec-tags-empty? ((this typespec))
  (null (typespec-tags this)))

;; Return number of tags

(defmethod typespec-tags-couny ((this typespec))
  (length (typespec-tags this)))

;; -----------------------------------------------------------------------------
;; The arguments api
;; -----------------------------------------------------------------------------

;; Check if the arguments list is empty

(defmethod typespec-args-count ((this typespec))
  (length (typespec-args this)))

;; Reference to the argument

(defmethod typespec-args-ref ((this typespec) (i integer))
  (list-ref (typespec-args this) i))

;; Reference to the argument

(defmethod typespec-args-last ((this typespec))
  (when (typespec-args this)
    (last (typespec-args this))))

(defmethod typespec-args-first ((this typespec))
  (when (typespec-args this)
    (first (typespec-args this))))

;; Check if the arguments list is empty

(defmethod typespec-args-empty? ((this typespec))
  (null (typespec-args this)))

;; Add the argument

(defmethod typespec-args-add ((this typespec) (arg typespec))
  (append! (typespec-args this) arg))

;; No arguments

(defmethod typespec-empty? ((this typespec))
  (== 0 (length (typespec-args this))))

(defmethod typespec-has-single-arg  ((this typespec))
  (== 1 (length (typespec-args this))))


(defmethod typespec-get-single-arg  ((this typespec))
  (assert (typespec-args this))
  (assert (== 1(typespec-args-count this)))
  (typespec-args-first this))
