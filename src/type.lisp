(in-package :type-system/type)
(use-package :type-system/interfaces)

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

;; ----------------------------------------------------------------------------
;; The method info
;; ----------------------------------------------------------------------------

;; The method info structure

(defstruct method-info
  (id -1 :type integer)
  (name nil :type (or null string))
  (type nil :type (or null typespec))
  (defined-in-type nil :type (or null string))
  (no-virtual nil :type boolean)
  (override nil :type boolean)
  )

(defun method-info-new (id name type defined-in-type &optional (no-virtual nil) (override nil))
  (make-method-info :id id :name (stringify name) :type type
                    :defined-in-type (stringify defined-in-type)
                    :no-virtual no-virtual :override override))
  		
;; One line inspector

(defmethod to-str ((this method-info))
  (method-info-to-str this))
(defun method-info-to-str (this)
  (format nil "Method ~3a: ~20a ~a"
          (method-info-id this)
          (method-info-name this)
          (to-str (method-info-type this))))

;; Compare types TODO Not sure it is needed

(defmethod diff ((this method-info) (other method-info))
  (method-info-diff this other))
;(declaim (ftype (function (method-info method-info) string) method-info-diff))
(defun method-info-diff (this other)  
  (let-with-slots l- (method-info id name type defined-in-type no-virtual override) this
    (let-with-slots r- (method-info id name type defined-in-type no-virtual override) other
      (let ((result ""))
	(when (!= l-id r-id) (string-append! result (format nil "id: ~a vs. ~a~%" l-id r-id)))
	(when (!= l-name r-name)
	  (string-append! result (format nil "name: ~a vs. ~a~%" l-name r-name)))
	(when (!= l-type r-type)
	  (string-append! result (format nil "type: ~a vs. ~a~%" (to-str l-type) (to-str r-type))))
	(when (!= l-defined-in-type r-defined-in-type)
	  (string-append! result
			  (format nil "defined-in-typ: ~a vs. ~a~%" l-defined-in-type
				  r-defined-in-type)))
	(when (!= l-no-virtual r-no-virtual)
	  (string-append! result (format nil "no-virtual: ~a vs. ~a~%" l-no-virtual r-no-virtual)))
	(when (!= l-override r-override)
	  (string-append! result (format nil "overrides: ~a vs. ~a~%" l-override r-override)))
	result))))
   
;; ----------------------------------------------------------------------------
;; The constants
;; ----------------------------------------------------------------------------


;; ----------------------------------------------------------------------------
;; The type
;; ----------------------------------------------------------------------------

;; Default constructor

(defstruct gtype
  (methods (arr-new :element-type :method-info) :type array)
  (states (make-hash-table) :type hash-table)
  (new-method-info nil :type (or null method-info))
  (new-method-info-defined nil :type boolean)
  (generate-inspect t :type boolean)
  (parent nil :type (or null string))
  (name nil :type (or null string))
  (allow-in-runtime t :type boolean)
  (runtime-name nil :type (or null string))
  (is-boxed nil :type boolean)
  (heap-base 0 :type integer)
  )

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

(declaim (ftype (function ((or null symbol string) (or symbol string) boolean integer) gtype) gtype-new))
(defun gtype-new (parent name is-boxed heap-base)
  (let ((it (make-gtype)))
    (setf (gtype-parent it) (stringify parent))
    (setf (gtype-name it) (stringify name))
    (setf (gtype-is-boxed it) is-boxed)
    (setf (gtype-heap-base it) heap-base)
    (setf (gtype-runtime-name it) (stringify name))
    it))

(defun gtype-base-type (this)
  (gtype-parent this))

;; Dsable type for runtime

(defun gtype-disallow-in-runtime (this)
  (setf (gtype-allow-in-runtime this) false))

;; Print information for all methods defined specifically for a type.
;; Does not print inherited methods.


(defmethod to-str ((this gtype))
  (format nil "[Type] {~a}" (gtype-name this)))

(defun methods-to-str (this)
  (string-join
   (cons (if (gtype-new-method-info-defined this)
             (to-str (gtype-new-method-info this))
             "")
         (map 'list (lambda (m) (to-str m))
              (gtype-methods this)))
   #\newline))


;; Does this type have a parent that makes sense to use? Object and none both
;; don't have meaningful parents.

(defun gtype-has-parent? (this)
  (and (!= (gtype-name this) "object")
       (!= (gtype-parent this) "")))

;; Returns the parent of false

(defun gtype-get-parent (this)
  (if (!= (gtype-name this) "object")
      (gtype-parent this)
      "none"))

;; Get a method that is defined specifically for this type. Returns if it was
;; found or not.

(defun gtype-get-my-method (this name)
  (find name (gtype-methods this) :key #'method-info-name))

;; Get a method that is defined specifically in this type by id. Returns if it
;; was found or not.

(defun gtype-get-my-method-by-id (this id)
  (assert (> id 0))  ;; 0 is new, should use explicit new method functions instead.
  (find id (gtype-methods this) :key #'method-info-id)) 

;; Get the last method defined specifically for this type. Returns if there were
;; any methods defined specifically for this type or not.

(defun gtype-get-my-last-method (this)
  (let ((col (gtype-methods this)))
    (if (== 0 (arr-count col))
	nil
	(arr-last col))))

;; Get the new method defined specifically for this type. Returns if there is a
;; new method specific to this type or not.

(defun gtype-get-my-new-method (this)
  (if (gtype-new-method-info-defined this)
      (gtype-new-method-info this)
      nil))

;; Add a method defined specifically for this type.
(declaim (ftype (function (gtype method-info) method-info) type-add-method))
(defun gtype-add-method (this info)
  (let* ((id  (method-info-id info))
         (col (gtype-methods this))
         (len (arr-count col))
         (last (- len 1)))

    ;; TODO! The test below is not clear what it does should be fied
    ;; I will comment it out for now
     ;; (dotimes (i len)
     ;;   ;; iterate backward
     ;;   (let ((it (arr-ref col (- last i))))
     ;; 	 (unless (method-info-override it)
     ;; 	   (assert (== id (1+ (method-info-id it))))
     ;; 	   ;; stop iterating
     ;; 	   (return t))))
    (arr-push (gtype-methods this) info)
    info))

;; Add a NEW method defined specifically for this type. The name of this
;; function is confusing - this is specific to the method named NEW.

(defun gtype-add-new-method (this info)
  (assert (== (method-info-name info) "new"))
  (setf (gtype-new-method-info-defined this) t)
  (setf (gtype-new-method-info this) info)
  info)

(defun gtype-set-runtime-type (this name)
  (setf (gtype-runtime-name this) name))


;; ----------------------------------------------------------------------------
;; Statens
;; ----------------------------------------------------------------------------

;; Add the state to the class

(defun gtype-add-state (this name type)
  (let ((state (hash-ref (gtype-states this) name nil)))
	(when state
	  (error (format nil "State ~a is multiply defined" name)))
	(hash-set! (gtype-states this) name type)))

(defun gtype-find-state (this name)
  (hash-ref (gtype-states this) name nil))

;; ----------------------------------------------------------------------------
;; The type
;; ----------------------------------------------------------------------------

(defun incompatible-diff (expected-type-name other)
  (format nil "diff is not implemented between ~a and ~a~%"
          expected-type-name other))

(defmethod diff ((this gtype) (other gtype))
  (gtype-diff this other))

(declaim (ftype (function (gtype gtype) string) gtype-diff))
(defun gtype-diff (this other)
  (let-with-slots l- (gtype methods states new-method-info
			  new-method-info-defined generate-inspect
			  parent name allow-in-runtime runtime-name
			  is-boxed heap-base) this
    (let-with-slots r- (gtype methods states new-method-info
			    new-method-info-defined generate-inspect
			    parent name allow-in-runtime runtime-name
			    is-boxed heap-base) other
      (let ((result ""))
	;;; Check methods count
	(unless (== l-methods r-methods)
	  (let* ((l-methods.size (arr-count l-methods))
		 (r-methods.size (arr-count r-methods))
		 (min-methods-size (min l-methods.size r-methods.size)))
	    (when (!= l-methods.size r-methods.size)
	      (string-append! result (format nil "Number of additional methods ~a vs. ~a~%"
					     l-methods.size r-methods.size)))
	    ;; Check methods one to one
	    (dotimes (i min-methods-size)
	      (let ((l (arr-ref l-methods i))
		    (r (arr-ref r-methods i)))
		(when (!= l r)
		  (string-append! result (format nil "Method def ~a (~a/~a):~%~a~%"
						 i (method-info-name l) (method-info-name r)
						 (method-info-diff l r))))))))
	;;; Check states
	(unless (== l-states r-states)
	  "States are different:~%"
	  (hash-map
	   (gtype-states this)
	   (lambda (name l-state)
	     (assert l-state)
	     (let ((r-state (gtype-find-state other name)))
	       (cond
		 ((not r-state)
		  (string-append! result (format nil "  ~a is in one, but not the other-~%" name)))
		 ((notequal? l-state r-state)
		  (string-append! result (format nil "  ~a is defined differently: ~a vs ~a~%"
						 name  (to-str l-state) (to-str r-state))))))))

	  (hash-map
	   (gtype-states other)
	   (lambda (name l-state)
	     (let ((r-state (gtype-find-state this name)))
	       (assert l-state)
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
	  (string-append! result (format nil "generate-inspect: ~a vs. ~a~%" l-generate-inspect r-generate-inspect)))
	
	result)
      )))
