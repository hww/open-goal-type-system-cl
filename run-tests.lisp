;; run-tests.lisp

;(ql:quickload "type-system")
(ql:quickload "type-system.tests") 
(in-package :type-system.tests)
(run! 'my-suite) 
