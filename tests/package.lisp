(uiop:define-package #:type-system.tests
    (:use #:cl #:type-system #:fiveam)
  (:import-from :type-system)
  (:export :run-tests-for-ci
	   :typespec-test))
