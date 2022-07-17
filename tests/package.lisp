(uiop:define-package #:type-system-test
  (:use #:cl #:type-system #:fiveam)
  (:import-from :type-system)
  (:export
   ;;
   :test!
   :typespec
   :type-test
   ;;
   :with-gensym
   :check-equal?
   :check-not-equal?
   )
  )
