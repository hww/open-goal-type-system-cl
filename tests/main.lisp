(in-package :type-system.tests)

(def-suite* main-test)

(test dumy-test 
  (is (= 2 (+ 1 1))  "2 equal 2")
  )

(defun run-tests-for-ci ()
  (run! 'main-test)
  (typespec-test))
