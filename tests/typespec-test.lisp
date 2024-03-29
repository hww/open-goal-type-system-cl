(in-package :type-system/test)

(defun type-tag-test ()
  (let ((ts (typespec-new :baz)))
    (check-equal? (to-str ts) "BAZ")
    (check-equal? (typespec-basetype ts) "BAZ")
    (let ((ts (typespec-new :bazz nil :x "A" :y "B")))
      (check-equal? (to-str ts) "(BAZZ () :X A :Y B)")
      (check-equal? (typespec-basetype ts) "BAZZ")
      )))

(defun typespec-tags-test()
  (check-equal? (type-tag-new :foo "A") (type-tag-new :foo "A"))
  (check-not-equal? (type-tag-new :foo "A") (type-tag-new :bar "A"))
  (check-not-equal? (type-tag-new :foo "A") (type-tag-new :foo "B"))
  (let ((ts (typespec-new :baz)))
    (typespec-add-new-tag ts :foo "A")
    (typespec-add-new-tag ts :bar "B")
    (check-equal? (to-str ts) "(BAZ () :FOO A :BAR B)")
    (check-equal? (typespec-try-get-tag ts :foo) (type-tag-new :foo "A"))
    (typespec-modify-tag ts :foo "C")
    (check-equal? (typespec-try-get-tag ts :foo) (type-tag-new :foo "C"))
    (typespec-add-or-modify-tag ts :baz "D")
    (check-equal? (typespec-try-get-tag ts :baz) (type-tag-new :baz "D"))
    ))

(defun typespec-args-test ()
  ;; Create the signature: (foo int int)
  (let ((ts (typespec-new :foo))
	(int (typespec-new :int)))
    (typespec-args-add ts int)
    (typespec-args-add ts int)
    (check-equal? (to-str ts)  "(FOO (INT INT))")
    (check-equal? (typespec-args-ref ts 0) (typespec-new :int))
    (check-equal? (typespec-args-count ts) 2)
    ))

(defun run-typespec-test ()
  (type-tag-test)
  (typespec-tags-test)
  (typespec-args-test)
  )
