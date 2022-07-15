(in-package :type-system-test)

(def-suite* main-test)

(test dumy-test
  (is (= 2 (/ 4 1))  "2 equal 2")
  )

(defun run-tests-for-ci ()
  (setf 5am:*on-failure* :debug)
  (setf 5am:*on-error* :debug)
  (setf 5am:*verbose-failures* t)
  (run! 'main-test)
  (run! 'typespec-test))

