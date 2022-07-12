(in-package :type-system)


(defun typespecp (thing)
  (and (listp thing)
       (symbolp (car thing))))

(deftype typespec (&optional type)
    `(and (listp ,type)
          (satisfies typespecp)))

(typespecp '(foo))


