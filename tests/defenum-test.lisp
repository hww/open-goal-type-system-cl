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
