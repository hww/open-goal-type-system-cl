
;; Test for parse the type spec -------------------------------------------------

(module+ test
  (require rackunit)
  (let ((tsys (type-system-new)))
    (add-builtin-types tsys)
    (let ((ts (parse-type-spec tsys #'int)))
      (check-equal? (inspect ts) "int"))

    (let ((ts (parse-type-spec tsys #'(int (int int) :behavior int))))
      (display (inspect ts))
      (check-equal? (inspect ts) "(int (int int) :behavior int)"))))


;; Test for parse the type spec -------------------------------------------------

(define (my-error-handler e)
  (raise (exn:fail:syntax
          (exn-message e)
          (exn-continuation-marks e)
          (list #'here))))


(module+ test
  (require rackunit)
  (let ((tsys (type-system-new)))
    (add-builtin-types tsys)
    ;; Large test
    (printf "~%[START] Deftype parsing test~%")
    (printf "Reading file...~%")
    (define expr (for-file-parse "goalc-all-types.gc"))
    (printf "Parsign deftypes...~%")
    (define deftype-cnt 0)
    (define defenum-cnt 0)
    (define constants (make-hash))
    (for-each-in-list
     expr
     (lambda (e)
       (define fe (syntax-e e))
       (when (list? fe)
         (define first (syntax-e (car fe)))
         (with-handlers
           ([exn:fail?
             (lambda (ex)
               (raise (exn:fail:syntax
                       (exn-message ex)
                       (exn-continuation-marks ex)
                       (list #'loc e))))])
           (cond
             ((== first 'declare-type)
              (printf "Parse ~a...~%" (syntax->datum (cadr fe)))
              (parse-declare-type (cdr fe) tsys)
              (+1! deftype-cnt))
             ((== first 'deftype)
              (printf "Parse ~a...~%" (syntax->datum (cadr fe)))
              (parse-deftype (cdr fe) tsys constants)
              (+1! deftype-cnt))
             ((== first 'defenum)
              (printf "Parse ~a...~%" (syntax->datum (cadr fe)))
              (parse-defenum (cdr fe) tsys)
              (+1! defenum-cnt)))))))
    (printf "[END] ~a deftypes ~a defenum parsed~%" deftype-cnt defenum-cnt)))

