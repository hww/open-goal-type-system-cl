(in-package :type-system/test)

(defun run-main-test ()
  (check-equal? (/ 2 1) 2)
  )

(defun test! ()
  (run-main-test)
  (run-typespec-test)
  (run-type-test)
  (run-basic-types-test)
  (run-type-system-test)
  )

