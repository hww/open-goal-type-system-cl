(in-package :type-system-test)

(def-suite* typespec-test)

(defmacro check-equal? (a expected)
  `(is (equalp ,a ,expected)
       (format nil "~%  Expected equal~%   given: ~a~%  expect: ~a~%" ,a ,expected)))

(defmacro check-not-equal? (a expected)
  `(is (not (equalp ,a ,expected))
       (format nil "~%  Expected not be equal~%   given: ~a~%  expect: ~a~%" ,a ,expected)))

(test type-tag-test
  (let ((ts (typespec-new :baz)))
    (check-equal? (typespec-inspect ts) "BAZ")
    (check-equal? (typespec-basetype ts) :baz)
    (let ((ts (typespec-new :bazz nil :x 1 :y 2)))
      (check-equal? (typespec-inspect ts) "(BAZZ X 1 Y 2)")
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
  ;; Create the signature: (foo int int)
  (let ((ts (typespec-new :foo))
	(int (typespec-new :int)))
    (typespec-args-add ts int)
    (typespec-args-add ts int)
    (check-equal? (typespec-args-ref ts 0) (typespec-new :int))
    (check-equal? (typespec-args-count ts) 2)
    ))

(defun typespec-test ()
  (setf 5am:*on-failure* :debug)
  (setf 5am:*on-error* :debug)
  (run! 'typespec-test))
