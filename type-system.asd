
(asdf:defsystem #:type-system
  :description "Open GOAL Type System"
  :version "0.1.0"
  :author "Valeriya Pudova <valery.hww@gmail.com>"
  :license "LLGPL"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cl-interpol #:alexandria)
  :components ((:file "package")
	       (:file "src/main")
	       (:file "src/goal-lib")
	       (:file "src/typespec")
	       (:file "src/type")
	       (:file "src/basic-types")
	       ))
