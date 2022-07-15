(asdf:defsystem #:type-system-test
  :description "Test system for type-system"
  :author "Valeriya Pudova <valery.hww@gmail.com>"
  :license "LLGPL"
  :encoding :utf-8
  :serial t
  :depends-on (#:type-system #:fiveam #:cl-interpol)
  :components ((:file "tests/package")
	       (:file "tests/main")
               (:file "tests/typespec-test")))
