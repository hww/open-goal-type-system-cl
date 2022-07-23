(in-package :type-system/test)
(use-package :type-system)

(defun run-defenum-test ()

  (let ((tsys (type-system-new!)))
    ;; Simple test
    (let* ((gs-psm-exp '(defenum
                         gs-psm
                         :type uint32
                         :bitfield nil
                         (ct32 0)
                         (ct24 1)
                         (ct16 2)))
           (gs-psm (parse-defenum tsys (cdr gs-psm-exp))))
      (check-equal? (to-str gs-psm) "[EnumType] gs-psm")))

  (let ((tsys (type-system-new!)))
       ;; Large test
       (printf "~%[START] Defenum test~%")
       (printf "Reading file...~%")
       (let ((expr (read-file-sexpression "./tests/goalc-all-types.gc"))
             (counter 0))
         (printf "Parsing defenums...~%")

         (loop for e in expr do
           (when (list? e)
             (let ((item (car e)))
               (when (== (symbol-name item) "defenum")
                 (parse-defenum tsys (cdr e))
                 (1+! counter)))))
         (printf "[END] ~a defenums parsing~%" counter)))

  )
