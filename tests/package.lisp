(uiop:define-package #:type-system-test
  (:use #:cl #:type-system)
  (:import-from :type-system)
  (:export
   ;;
   :with-gensym
   :check-equal?
   :check-not-equal?
   ;;
   :test!
   :typespec
   :type-test
   )
  )
