(in-package :type-system-test)
(use-package :type-system/type)

(defun type-test ()
  (let ((mi1 (method-info-new 1 'name1 (typespec-new 'foo1 nil nil) 'foo1 nil t))
        (mi2 (method-info-new 2 'name2 (typespec-new 'foo2 nil nil) 'foo2 nil t)))
    (check-equal? mi1
		  (method-info-new 1 'name1 (typespec-new 'foo1 nil nil) 'foo1 nil t))

    (check-equal?
     (string-downcase (diff mi1 mi2))
     (format nil "id: 1 vs. 2~%name: name1 vs. name2~%type: foo1 vs. foo2~%defined-in-typ: foo1 vs. foo2~%"))
    ))


