(in-package :goal-lib)

;; ==============================================================================;;;;
;; CONSTANTS
;; ==============================================================================;;;;

(defconstant else t)
(defmacro null? (val) `(null ,val))
(defmacro integer? (val) `(integerp ,val))
(defmacro float? (val) `(floatp ,val))
(defmacro symbol? (val) `(symbolp ,val))
(defmacro keyword? (val) `(keywordp ,val))
(defmacro string? (val) `(stringp ,val))
(defmacro boolean? (obj) `(typep ,obj 'boolean))
(defmacro number? (obj) `(numberp ,obj))
(defmacro list? (obj) `(listp ,obj))


(defconstant true t)
(defconstant false nil)

(defmacro equal? (&rest args)
  `(equalp ,@args))

(defmacro notequal? (&rest args)
  `(not (equalp ,@args)))

(defmacro == (&rest args)
  `(equalp ,@args))

(defmacro != (&rest args)
  `(not (equalp ,@args)))

;; ==============================================================================;;;
;; Declaration
;; ==============================================================================;;;

(defmacro declare-fun (name args result)
  `(declaim (ftype (function ,args ,result) ,name)))

;; ==============================================================================;;;
;; Log
;; ==============================================================================;;;

(defmacro log-warning (fmt &rest args) `(format t ,fmt ,@args))
(defmacro log-debug (fmt &rest args) `(format t ,fmt ,@args))
(defmacro printf (fmt &rest args) `(format t ,fmt ,@args))

;; ==============================================================================;;;
;;
;; ==============================================================================;;;;

(cl:defmacro ex/defconstant (&whole whole
                          name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  `(progn
     (export ',name)
     (cl:defconstant ,name ,@(cddr whole))))

(cl:defmacro ex/defmacro (name lambda-list &body body)
  `(progn
     (export ',name)
     (defmacro ,name ,lambda-list ,@body)))


(cl:defmacro ex/defun (name lambda-list &body body)
  `(progn
     (export ',name)
     (defun ,name ,lambda-list ,@body)))

;; ==============================================================================;;;;
;; STRING TOOLS
;; ==============================================================================;;;;

(defun stringify (v)
  (cond
    ((null v) v)
    ((stringp v) v)
    ((symbolp v) (string-downcase (symbol-name v)))
    (t (format nil "~a" v))))

(defun string-append (&rest list)
  (format nil "~{~a~}" (remove nil list)))

(defmacro string-append! (var &rest list)
  `(setf ,var (format nil "~{~a~}" (remove nil (list ,var ,@list)))))


(defun string-join (list &optional (delim "&"))
    (with-output-to-string (s)
        (when list
            (format s "~A" (first list))
            (dolist (element (rest list))
	      (format s "~A~A" delim element)))))

(defun list->string (lst)
  (format nil "~{~A~}" lst))

(defun string-substring (needle haystack &key (test 'char=))
  "Returns the index of the first occurrence of the string designated
by NEEDLE within the string designated by HAYSTACK, or NIL if it does
not occur.  Characters within the string are compared by TEST, which
defaults to CHAR= (for case-sensitive comparison)."
  (search (string needle)
          (string haystack)
          :test test))

(defun string-to-one-line (s)
  (cl-ppcre:regex-replace-all "(\\n|\\s*$)" s ""))

(defmacro string-ref (str idx)
  `(char ,str ,idx))

;;;;;;;;;;;;;;;;;;;
;; Math Macros
;;;;;;;;;;;;;;;;;;;

(defmacro set! (place value)
  `(setf ,place ,value)
  )

;; (defmacro 1+ (var)
;;   `(+ ,var 1)
;;   )

(defmacro inc (val)
  "Increments a value"
  `(1+ ,val))

(defmacro +! (place amount)
  `(set! ,place (+ ,place ,amount))
  )

(defmacro 1+! (place)
  `(+! ,place 1)
  )

;; (defmacro 1- (var)
;;   `(+ ,var -1)
;;   )

(defmacro dec (val)
  "Decrements a value"
  `(1- ,val))

(defmacro -! (place amount)
  `(set! ,place (- ,place ,amount))
  )

(defmacro 1-! (place)
  `(-! ,place 1)
  )

(defmacro *! (place amount)
  `(set! ,place (* ,place ,amount))
  )

(defmacro /! (place amount)
  `(set! ,place (/ ,place ,amount))
  )

(defmacro zero? (thing)
  `(eq? ,thing 0)
  )

(defmacro nonzero? (thing)
  `(neq? ,thing 0)
  )

(defmacro or! (place &rest args)
  `(set! ,place (or ,place ,@args))
  )

(defmacro not! (var)
  `(set! ,var (not ,var)))

(defmacro true! (var)
  `(set! ,var true))

(defmacro false! (var)
  `(set! ,var false))

(defmacro minmax (val minval maxval)
  `(max (min ,val ,maxval) ,minval)
  )

(defmacro fminmax (val minval maxval)
  `(fmax (fmin ,val ,maxval) ,minval)
  )
(defmacro minmax! (val minval maxval)
  `(set! ,val (max (min ,val ,maxval) ,minval))
  )
(defmacro fminmax! (val minval maxval)
  `(set! ,val (fmax (fmin ,val ,maxval) ,minval))
  )

(defmacro maxmin (val minval maxval)
  `(min (max ,val ,maxval) ,minval)
  )

(defmacro fmaxmin (val minval maxval)
  `(fmin (fmax ,val ,maxval) ,minval)
  )

(defmacro &+! (val amount)
  `(set! ,val (&+ ,val ,amount))
  )

(defmacro &- (a b)
  `(- (the-as int ,a) (the-as int ,b))
  )

(defmacro &-> (&rest args)
  `(& (-> ,@args))
  )

(defmacro logior! (place amount)
  `(set! ,place (logior ,place ,amount))
  )

(defmacro logxor! (place amount)
  `(set! ,place (logxor ,place ,amount))
  )

(defmacro logand! (place amount)
  `(set! ,place (logand ,place ,amount))
  )

(defmacro logclear (a b)
  "Returns the result of setting the bits in b to zero in a"
  ;; put a first so the return type matches a.
  `(logand ,a (lognot ,b))
  )

(defmacro logclear! (a b)
  "Sets the bits in b to zero in a, in place"
  `(set! ,a (logand ,a (lognot ,b)))
  )

(defmacro logtest? (a b)
  "does a have any of the bits in b?"
  `(nonzero? (logand ,a ,b))
  )

(defmacro logtesta? (a b)
  "does a have ALL of the bits in b?"
  `(= (logand ,b ,a) ,b)
  )
#|
(defmacro deref (t addr &rest fields)
  `(-> (the-as (pointer ,t) ,addr) ,@fields)
  )

(defmacro &deref (t addr &rest fields)
  `(&-> (the-as (pointer ,t) ,addr) ,@fields)
  )

(defmacro shift-arith-right-32 (result in sa)
  `(set! ,result (sext32 (sar (logand #xffffffff (the-as int ,in)) ,sa)))
  )
|#
(defmacro /-0-guard (a b)
  "same as divide but returns -1 when divisor is zero (EE-like)."
  `(let ((divisor ,b))
      (if (zero? divisor)
          -1
          (/ ,a divisor))
      )
  )

(defmacro mod-0-guard (a b)
  "same as modulo but returns the dividend when divisor is zero (EE-like)."
  `(let ((divisor ,b))
      (if (zero? divisor)
          ,a
          (mod ,a divisor))
      )
  )

(defmacro float->int (a)
  "forcefully casts something as a float to int. be careful."
  `(the integer (round (the float ,a)))
  )

;; ==============================================================================
;; Bit Macros
;; ==============================================================================

(defmacro align-n (val n)
  "align val to n-byte boundaries"
  `(logand (- ,n) (+ (round ,val) (- ,n 1)))
  )

(defmacro align16 (val)
  `(align-n ,val 16)
  )

(defmacro align64 (val)
  `(align-n ,val 64)
  )

(defmacro bit-field (val base size &optional (signed t))
  "extract bits from an integer value."
  (when (and (integer? base) (integer? size))
      (when (> (+ base size) 64)
          (error "cannot extract fields across 64-bit boundaries"))
      (when (< base 0)
          (error "bitfield base cannot be negative"))
      (when (< size 0)
          (error "bitfield size cannot be negative"))
      )
  `(,(if signed 'sar 'shr) (shl ,val (- 64 (+ ,size ,base))) (- 64 ,size))
  )

;; ==============================================================================;;;;
;; LIST
;; ==============================================================================;;;;

(defmacro list-ref (list n)
  `(nth ,n ,list))

(defun list->vector (list)
  (coerce list 'vector))

(defun vector->list (array)
  (coerce array 'list))

(defmacro append! (list v)
  `(setf ,list (append ,list (cons ,v nil))))

;; ==============================================================================;;;;
;; ARRAYS
;; ==============================================================================;;;;

(defun arr-new (&key (capacity 8) (element-type nil)) 
  (if element-type
      (make-array capacity :fill-pointer 0 :adjustable t :element-type element-type)
      (make-array capacity :fill-pointer 0 :adjustable t)
      ))

(defmacro arr-first (coll)
  "Returns the first element in an array"
  `(aref ,coll 0))

(defmacro arr-last (coll)
  "Returns the last element in an array"
  `(aref ,coll (dec (length ,coll))))

(defmacro arr-last-idx (coll)
  "Returns the index of the last element in an array"
  `(dec (length ,coll)))

(defmacro arr-idx-of (coll val def)
  "Returns the index of an item in an array, returns <def> if is nothing is found."
  `(block find-element
    (dotimes (i (length ,coll))
      (if (= ,val (-> ,coll i))
        (return-from find-element i)))
    ,def))

(defmacro arr-ref (col idx &optional (default nil)) 
  `(cond
     ((< ,idx 0) ,default)
     ((>= ,idx (length ,col)) ,default)
     (t (aref ,col ,idx))))

(defmacro arr-set! (col idx val)
  `(setf (aref ,col ,idx) ,val))

(defmacro arr-push (col val)
  `(vector-push-extend ,val ,col))

(defmacro arr-count (col)
  `(length ,col))

;; ==============================================================================
;; Hash table helpters
;; ==============================================================================

(defmacro hash-ref (hash key &optional (default nil))
  `(gethash ,key ,hash ,default))
(defmacro hash-set! (hash key val)
  `(setf (gethash ,key ,hash) ,val))
(defmacro hash-count (hash)
  `(hash-table-count ,hash))
(defmacro hash-map (hash func)
  `(loop for hash-map-value being the hash-values of ,hash
           using (hash-key hash-map-key)
         collect (,func hash-map-key hash-map-value)))
(defmacro hash-clear! (hash)
  `(clrhash ,hash))
(defmacro hash-remove! (hash key)
  `(remhash ,key ,hash))
(defun make-hash (&key (capacity 8) (test 'equal))
  (make-hash-table :size capacity :test test))
(defmacro hash-has-key? (hash key)
  `(nth-value 1 (gethash ,key ,hash)))

;; ==============================================================================
;; The structure helpers
;; ==============================================================================

;; Same as with-slots but can rename fields less verbose
;;
;; Usage:
;;
;; (defstruct v3 x y z)
;; (defvar v (make-v3 :x 10 :y 20 :z 30))
;; (my/with-slots f- (v3 x y z) v
;;    (format t "~a ~a ~a" f-x f-y f-x))
;;
;; (defun foo ()
;;    (my/with-slots foo- (v3 x y z ) (make-v3 :x 10 :y 20 :z 30)
;;      (my/with-slots bar- (v3 x y z ) (make-v3 :x 40 :y 50 :z 60)
;;        (format t "FOO x ~a y ~a z ~a~%" foo-x foo-y foo-z)
;;        (format t "BAR x ~a y ~a z ~a~%" bar-x bar-y bar-z))))
;; (foo)

(defmacro my/with-slots (prefix (struct &rest fields) obj &body body)
  (assert struct) ; TODO kill the parameter STRUCT
  (let*
      ;; make empty prefix for nil
      ((pfx (if (null prefix) "" prefix))
       ;; make a list of fields and accessors
       (symbol-list
	 (loop for field-name in fields
	       collect (cons
			;; local variable name
			(intern (string-upcase (format nil "~a~a" pfx field-name)))
			;; slot name
			field-name)))
       (expr-list
	 (loop for f in symbol-list
	       collect `(,(car f) (slot-value ,obj ',(cdr f))))))
    `(let (,@expr-list) ,@body)))


;; Copy the structure and override some of values
;;
;; CL-USER> (defstruct foo bar baz)
;; FOO
;; CL-USER> (defparameter *first* (make-foo :bar 3))
;; *FIRST*
;; CL-USER> (defparameter *second* (update-struct *first* 'baz 2))
;; *SECOND*
;; CL-USER> (values *first* *second*)
;; #S(FOO :BAR 3 :BAZ NIL)
;; #S(FOO :BAR 3 :BAZ 2)

(defun update-struct (struct &rest bindings)
  (loop
    with copy = (copy-structure struct)
    for (slot value) on bindings by #'cddr
    do
       (let ((slot-name (find-symbol (symbol-name slot))))
	 (setf (slot-value copy slot-name) value))
    finally (return copy)))




;; ==============================================================================
;;
;; ==============================================================================
;; (my/defun foo (a b)
;;   (var x 2)
;;   (var y 3)
;;   (* x y a b))
;;
;; (foo 4 5)
;; ; => 120

(defun my/vardecl-p (x)
  "Return true if X is a (VAR NAME VALUE) form."
  (and (listp x)
       (> (length x) 1)
       (eq 'var (car x))))

(defmacro my/defun (name args &rest body)
  "Special form of DEFUN with a flatter format for LET vars"
  (let ((vardecls (mapcar #'cdr
                          (remove-if-not #'my/vardecl-p body)))
        (realbody (remove-if #'my/vardecl-p body)))
    `(defun ,name ,args
       (let* ,vardecls
         ,@realbody))))

;; ==============================================================================
;; Structure to list and back
;; ==============================================================================

(defun struct-direct-names (struct)
  (loop for sl in (sb-mop::class-direct-slots (class-of struct))
        collect (list
                 (sb-mop:slot-definition-name sl)
                 (slot-value sl 'sb-pcl::internal-reader-function))))

(defun struct-direct-values (struct)
  (cons (type-of struct)
        (loop for np in (struct-direct-names struct)
              collect (cons (car np)
                            (funcall (cadr np) struct)))))


(defun struct-names (struct)
  (loop for sl in (sb-mop::class-slots (class-of struct))
        collect (list
                 (sb-mop:slot-definition-name sl)
                 (slot-value sl 'sb-pcl::internal-reader-function))))

(defun struct-values (struct)
  (cons (type-of struct)
        (loop for np in (struct-names struct)
              collect (cons (car np)
                            (funcall (cadr np) struct)))))


(defun structp (val)
  (typep val 'structure-object))

(defun struct-to-list (val)
  (cond
    ((typep val 'hash-table) val)
    ((typep val 'vector) val)
    ((structp val)
     (loop for v in (struct-values val)
           collect (struct-to-list v)))
    ((consp val)
     (cons (struct-to-list (car val))
           (struct-to-list (cdr val))))
    (t val)))

(defun list-to-struct! (dst src-alst)
  (let ((val-alst (struct-to-list dst)))
    (loop for d in (cdr val-alst) for s in (cdr src-alst)  
	  do
	     (setf (slot-value dst (car d)) (cdr s)))))

(defun list-to-struct (dst src-alst)
  (let ((copy (copy-structure dst)))
    (list-to-struct! copy src-alst)
    copy))

;; (defstruct foo x)
;; (defstruct bar x y)
;; (defparameter-foo :x 1))
;; (defparameter y (make-bar 4list-to-))
;; (defparameter z (struct-to-list x))

(defun copy-parent-struct-func (pkg dst src &rest bindings)
  "Copy src structur to dst stracture and use additional bindings"
  (list-to-struct! dst (struct-to-list src)) 
  (loop
    for (slot value) on bindings by #'cddr
    do (setf (slot-value dst (find-symbol (symbol-name slot) pkg)) value)
    finally
       (return dst)))

(defmacro copy-parent-struct (dst src &rest bindings)
  "Copy src structur to dst stracture and use additional bindings" 
  `(copy-parent-struct-func *package* ,dst ,src ,@bindings))


;(defstruct baz-t x y z)
;(defvar baz (make-baz-t :x 1 :y 2 :z 3))
;(defvar bazz (make-baz-t :x 4 :y 5 :z 6))

;(print (copy-parent-struct bazz baz :x 100))


;; ==============================================================================
;; Conditional
;; ==============================================================================


(defmacro move-if-not-zero (result value check original)
    `(if (!= ,check 0)
         (setf ,result ,value)
         (setf ,result ,original)))

;; "dest = src1 < src2 ? 1 : 0 -- Compare as Signed Integers
(defmacro set-on-less-than (dest src1 src2)
    `(if (< ,src1 ,src2)
         (setf ,dest 1)
         (setf ,dest 0)))

(defconstant INT8-MAX   128)
(defconstant INT8-MIN  -127)
(defconstant INT16-MAX  32767)
(defconstant INT16-MIN  -32768)
(defconstant INT32-MAX  2147483647)
(defconstant INT32-MIN -2147483648)

(defconstant UINT8-MAX  255)
(defconstant UINT16-MAX 65535)
(defconstant UINT32-MAX 4294967295)

;; helper
(defun integer-fits? (in size is-signed)
    (cond
      ((== 1 size)
       (if is-signed
           (and (>= in INT8-MIN) (<= in INT8-MAX))
           (and (>= in 0) (<= in UINT8-MAX))))
      ((== 2 size)
       (if is-signed
           (and (>= in INT16-MIN) (<= in INT16-MAX))
           (and (>= in 0) (<= in UINT16-MAX))))
      ((== 4 size)
       (if is-signed
           (and (>= in INT32-MIN) (<= in INT32-MAX))
           (and (>= in 0) (<= in UINT32-MAX))))
      ((== 8 size)
       true)
      (else
       (assert false))))


;; ==============================================================================
;;
;; ==============================================================================

(defun make-sexpression-environment ()
  (make-hash-table :test #'eq))

(defun read-stream-sexpression (stream environment)
  (let ((char (read-char stream)))
    (or (position char "0123456789")
        (and (member char '(#\newline #\space #\tab)) :space)
        (case char
          (#\) :closing-paren)
          (#\( (loop
                  with beg = (file-position stream)
                  for x = (read-stream-sexpression stream environment)
                  until (eq x :closing-paren)
                  unless (eq x :space)
                    collect x into items
                  finally
                    (setf (gethash items environment)
                          (list :beg beg :end (file-position stream)))
                   (return items)))))))

(defun read-string-sexpression (str)
  ;;(when (null? env) (setf env (make-sexpression-environment)))
  (let ((pos 0)
        (result '()))
    ;(setf (READTABLE-CASE *READTABLE*) :PRESERVE)
    (loop
      (multiple-value-bind (exsp npos)
          (read-from-string str nil nil :start pos)
        (when (null exsp)
          (return))
        (setf pos npos)
        (setf result (cons exsp result))))
    ;(setf (READTABLE-CASE *READTABLE*) :invert)
    (reverse result)))

(defun read-file-sexpression (path)
  (let ((str (uiop:read-file-string path)))
    (read-string-sexpression str)))

(set-dispatch-macro-character #\# #\F ;dispatch on #F
    #'(lambda(s c n) nil))
(set-dispatch-macro-character #\# #\T ;dispatch on #T
    #'(lambda(s c n) t))

;; (defun get-location-string (obj &key (env nil))
;;   (if (null? env)
;;       "unknown-location"
;;       (format nil "~a ~a:~a" )
;;       ))

;(let ((env (make-environment)))
;  (with-input-from-string (in "(0(1 2 3) 4 5 (6 7))")
;    (values
;     (my-simple-lisp-reader in env)
;     env)))
