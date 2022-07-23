(in-package :type-system/test)
(use-package :type-system)


 
(defun parse-typespec-test ()

  (let ((tsys (type-system-new!)))

    (let ((ts (parse-typespec tsys 'int)))
      (check-equal? (to-str ts) "int"))

    (let ((ts (parse-typespec tsys '(function int int int :behavior int))))
      (format t ">>>>~a~%" ts)
      (check-equal? (to-str ts) "(function (int int int) :behavior int)"))))

(defun parse-file-test ()
  (let ((tsys (type-system-new!)))
	;; Large test
	(printf "~%[START] Deftype parsing test~%")
	(printf "Reading file...~%")
    (let ((expr (read-file-sexpression "./tests/goalc-all-types.gc"))
	  (deftype-cnt 0)
	  (defenum-cnt 0)
	  (constants (make-hash)))
	  (printf "Parsign deftypes...~%")

      (loop for fe in expr do     
	(when (list? fe)
	  (let ((item (string-downcase (symbol-name (car fe)))))
	    (cond
	      ((== item "declare-type")
	       (printf "Parse ~a...~%" (cadr fe))
	       (parse-declare-type tsys (cdr fe))
	       (1+! deftype-cnt))
	      ((== item "deftype")
	       (printf "Parse ~a...~%" (cadr fe))
	       (parse-deftype tsys (cdr fe) constants)
	       (1+! deftype-cnt))
	      ((== item "defenum")
	       (printf "Parse ~a...~%" (cadr fe))
	       (parse-defenum tsys (cdr fe))
	       (1+! defenum-cnt))))))
      (printf "[END] ~a deftypes ~a defenum parsed~%" deftype-cnt defenum-cnt))))

(defun run-deftype-test ()
  (parse-typespec-test)
  (parse-file-test))		  
