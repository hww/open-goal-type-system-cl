(asdf:defsystem #:type-system
  :description "Open GOAL Type System"
  :version "0.1.0"
  :author "Valeriya Pudova <valery.hww@gmail.com>"
  :license "LLGPL"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :pathname "src/"
  :depends-on (#:alexandria #:named-readtables #:cl-ppcre)
  :components ((:file "package")
               (:file "goal-lib" :depends-on ("package"))
               (:file "interfaces" :depends-on ("package" "goal-lib"))
               (:file "typespec" :depends-on ("package" "interfaces" "goal-lib"))
               (:file "type" :depends-on ("package" "typespec"))
               (:file "basic-types" :depends-on ("package" "type"))
               (:file "type-system" :depends-on ("package" "basic-types"))
               (:file "builtin-types" :depends-on ("package" "type-system" "basic-types" "type"))
               (:file "defenum" :depends-on ("package" "type-system" "basic-types" "type"))
               (:file "deftype" :depends-on ("package" "type-system" "basic-types" "type"))
	       (:file "state" :depends-on ("package" "type-system" "basic-types" "type"))
               ))


(asdf:defsystem #:type-system/test
  :description "Test system for type-system"
  :author "Valeriya Pudova <valery.hww@gmail.com>"
  :license "LLGPL"
  :encoding :utf-8
  :serial t
  :depends-on (#:type-system #:alexandria)
  :pathname "tests/"
  :components ((:file "package")
               (:file "test-lib" :depends-on ("package"))
               (:file "typespec-test" :depends-on ("package" "test-lib"))
               (:file "type-test" :depends-on ("package" "test-lib"))
               (:file "basic-types-test" :depends-on ("package" "test-lib"))
               (:file "type-system-test" :depends-on ("package" "test-lib"))
               (:file "defenum-test" :depends-on ("package" "test-lib"))
	       (:file "deftype-test" :depends-on ("package" "test-lib"))
               (:file "main" :depends-on ("package" "typespec-test" "type-test" "basic-types-test" "type-system-test"))
               ))
