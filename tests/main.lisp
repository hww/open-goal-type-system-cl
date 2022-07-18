(in-package :type-system-test)

(defun main-test ()
  (check-equal? (/ 2 1) 2)
  )

(defun test! ()
  (main-test)
  (typespec-test)
  (type-test)
  (basic-types-test)
  )

