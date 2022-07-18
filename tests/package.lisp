(uiop:define-package #:test-lib
  (:use #:cl #:type-system)
  (:import-from :type-system)
  (:export
   ;;
   :with-gensym
   :check-equal?
   :check-not-equal?
   :check-equal-downcase?
   ))

(uiop:define-package #:type-system-test
  (:use #:cl #:type-system #:test-lib)
  (:import-from :type-system)
  (:export
   :test!
   :typespec
   :type-test
   :basic-types-test
   )
  )
