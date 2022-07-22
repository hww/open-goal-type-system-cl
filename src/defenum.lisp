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
;; Utilities
;; ==============================================================================

(defun is-key? (obj)
  "Verify if the string starts with ':' character"
  (or
   (and (stringp obj) (== #\: (char obj 0)))
   (and (symbolp obj) (== #\: (char (symbol-name obj) 0)))))

(defun get-key (obj)
  "Return object starts with ':' character or null"
  (if (is-key? obj)
      (cond
        ((stringp obj) obj)
        ((symbolp obj) (symbol-name obj))
        (t nil))
      nil))

(defun get-boolean (obj)
  "Return a boolean value or make error"
  (if (boolean? obj)
      obj
      (error (format nil "Expected boolean value found ~a" obj))))

;; ==============================================================================
;;
;; Parse defenum
;;
;; (defenum gs-psm
;;     :bitfield false
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

(defun parse-defenum (this defenum)
  "Parse s-expression and build the enum type"
  ;; helper
  (defun is-type (expected actual)
    ;;(-> symbol? typespec? boolean?)
    (tc this (make-a-typespec this expected) actual))

  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; default enum type will be int32.
  (let* ((par-type (make-a-typespec this "int32"))
         (is-bitfield nil)
         (entries (make-hash))
         (iter defenum)
         (enum-name (car iter)))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (setf iter (cdr iter))
    (unless (symbol? enum-name)
      (error (format nil "defenum must be given a symbol as its name but found ~a" enum-name)))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (let ((other-info nil)
          (option-name nil)
          (option-value nil)
          (current (car iter)))
      ;; Iterate over the options
      (loop
        (when (null iter) (return)) ; exit from loop
        ;;
        (setf current (car iter))
        ;; get option name
        (setf option-name (get-key current))
        (setf iter (cdr iter))
        (when option-name
          ;; get option value
          (setf option-value (car iter))
          (setf iter (cdr iter))
          ;; parse option
          (cond
            ((== option-name ":type")
             (setf par-type (parse-typespec this option-value)))
            ((== option-name ":bitfield")
             (cond
               ((boolean? option-value)
                (setf is-bitfield (get-boolean option-value)))
               (else
                (error (format nil "Invalid option `~a` to :bitfield option.\n" option-value)))))
            ((== option-name ":copy-entries")
             (setf other-info (try-enum-lookup this (parse-typespec this option-value)))
             (unless other-info
               (error (format nil "Cannot copy entries from `~a`, it is not a valid enum type" option-value)))
             ;; copy entries from given enum
             (loop for k being the hash-keys in (enum-type-entries other-info) using (hash-value v)
                   do
                      (when (hash-ref entries k nil)
                        (error (format nil "Entry `~a` appears multiple times" k)))
                      (hash-set! entries k v)))
            ;; Some unkown option
            (else
             (error (format nil "Unknown option `~a` for defenum.\n" option-name))))))
      ;; End of iteration
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ;; Find the base type
      (let ((type (lookup-type this par-type))
            (highest -1))
        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;; Iterate with items
        ;; (printf "parse-defenum\n ~a\n" iter)
        (loop
          (when (null iter)
            (return))
          ;; If there is more items in the list
          (let* ((field (car iter))
                 (entry-name (car field))
                 (item (hash-ref entries entry-name nil)))
            ;; Check duplicate
            (when item
              (error (format nil "Entry `~a` appears multiple times." entry-name)))

            (let ((rest (cdr field)))

              (cond
                ((not (null rest))
                 (let ((item-value (car rest)))
                   (unless (integer? item-value)
                     (error (format nil "Expected integer for enum item-value, got `~a`\n" item-value)))
                   (unless (integer-fits? item-value  (get-load-size type) (get-load-signed type))
                     (error (format nil "Integer `~a` does not fit inside a `~a`\n" item-value (gtype-name type))))
                   (when (> (hash-count entries) 0)
                     (setf highest item-value))

                   (setf highest (max highest item-value))

                   (setf rest (cdr rest))
                   (unless (null? rest)
                     (error (format nil "Got too many items in defenum `~a` entry `~a`\n" enum-name entry-name)))

                   (hash-set! entries entry-name item-value)))
                (else
                 (1+! highest)
                 (hash-set! entries entry-name highest)))))

          (setf iter (cdr iter))))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      (cond
            ((is-type "integer" par-type)
             (let* ((parent (get-type-of-type this #'value-type-p (base-type par-type)))
                    (new-type (enum-type-new parent enum-name is-bitfield entries)))
               (gtype-set-runtime-type new-type (gtype-runtime-name parent))
               (add-type this (symbol-name enum-name) new-type)
               new-type))
            (else
             (error (format nil "Creating an enum with type `~a` is not allowed or not supported yet."
                            (to-str par-type))))))))
