(uiop:define-package #:test-lib
  (:use #:cl #:type-system)
  (:import-from :type-system)
  (:export
   ;;
   :with-gensym
   :check-equal?
   :check-not-equal?
   :check-equal-downcase?
   :check-true
   ))

(uiop:define-package #:type-system/test
  (:use #:cl #:type-system #:test-lib #:alexandria)
  (:import-from :type-system)
  (:export
   :test!
   :run-typespec-test
   :run-type-test
   :run-basic-types-test
   :run-type-system-test
   :run-defenum-test
   )
  (:reexport
   :type-system
   :alexandria
  ))
