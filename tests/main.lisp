(in-package :type-system.tests)

(def-suite* my-suite)

(test my-test 
  (is (= 2 (+ 1 1))  "2 equal 2")
  (is (= 2 (+ 1 2)) "2 equal 2")
  )
