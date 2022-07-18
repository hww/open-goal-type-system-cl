
(asdf:defsystem #:type-system-test
  :description "Test system for type-system"
  :author "Valeriya Pudova <valery.hww@gmail.com>"
  :license "LLGPL"
  :encoding :utf-8
  :serial t
  :depends-on (#:type-system #:alexandria)
  :components ((:file "tests/package")
	       (:file "tests/test-lib")
	       (:file "tests/main")
               (:file "tests/typespec-test")
	       (:file "tests/type-test")
	       (:file "tests/basic-types-test")
	       ))
