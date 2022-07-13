(in-package :type-system.tests)

(def-suite* typespec-suite)

(defmacro check-equal? (a expected)
  `(is (equal ,a ,expected)
       (format nil "~%  is not equal~%   given: ~a~%  expect: ~a~%" ,a ,expected)))

(defmacro check-not-equal? (a expected)
  `(is (not (equal ,a ,expected))
       (format nil "~%  is equal~%   given: ~a~%  expect: ~a~%" ,a ,expected)))

(test type-tag-test
  (let ((ts (typespec-new :baz)))
    (check-equal? (typespec-inspect ts) "BAZ")
    (check-equal? (typespec-basetype ts) :baz)
    (let ((ts (typespec-new :bazz '() :x 1 :y 2)))
      (check-equal? (typespec-inspect ts) "(BAZZ :Z 1 :Y 2)")
      (check-equal? (typespec-basetype ts) :bazz)
      )))

(test typespec-tags-test
      (check-equal? (type-tag-new :foo 1) (type-tag-new :foo 1))
      (check-not-equal? (type-tag-new :foo 1) (type-tag-new :bar 1))
      (check-not-equal? (type-tag-new :foo 1) (type-tag-new :foo 2))
      (let ((ts (typespec-new :baz)))
	(typespec-add-new-tag ts :foo 1)
	(typespec-add-new-tag ts :bar 2)
	(check-equal? (typespec-try-get-tag ts :foo) (type-tag-new :foo 1))
	(typespec-modify-tag ts :foo 3)
	(check-equal? (typespec-try-get-tag ts :foo) (type-tag-new :foo 3))
	(typespec-add-or-modify-tag ts :baz 4)
	(check-equal? (typespec-try-get-tag ts :baz) (type-tag-new :baz 4))
	))

(test typespec-args-test
      (let ((ts (typespec-new :foo))
	    (int (typespec-new :int)))
	(typespec-args-add ts int)
	(typespec-args-add ts int)
	(check-equal? (typespec-args-ref ts 0) (typespec 'int '() '()))
	(check-equal? (typespec-args-count ts) 2)
	))

(defun typespec-test ()
  (run! 'typespec-suite))
