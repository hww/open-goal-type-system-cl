;; Test MethodInfo  -------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ((mi1 (method-info-new 1 'name1 (type-spec 'foo1 '() '()) 'foo1 #f #t))
        (mi2 (method-info-new 2 'name2 (type-spec 'foo2 '() '()) 'foo2 #f #t)))
    (check-equal? mi1 (method-info 1 'name1 (type-spec 'foo1 '() '()) 'foo1 #f #t))

    (check-equal? (diff mi1 mi2) "id: 1 vs. 2\nname:: name1 vs. name2\ntype: foo1 vs. foo2\ndefined-in-typ: foo1 vs. foo2\n")
    ))

