(in-package :type-system/test)
(use-package :test-lib)
(use-package :type-system)

(defun value-type-test ()
  (check-equal-downcase? 
   (to-str (value-type-new "foo" "bar" nil 10 t))
   (format nil "[ValueType] bar~% parent: foo~% boxed: nil~% size: 10~% sext: T~%")))

(defun reference-type-test ()
  (check-equal-downcase?
   (to-str (reference-type-new "foo" "bar" nil 10))
   (format nil "[ReferenceType] bar~% parent: foo~% boxed: nil~%")))

(defun struct-type-test ()
  (check-equal-downcase?
   (to-str (struct-type-new "" "foo" nil nil nil  10))
   "[structtype] foo parent:  boxed: nil size: 0 pack: nil misalign: nil heap-base: 10 stack-singleton: nil fields: methods:"
))


(defun bitfield-type-test ()
  (check-equal-downcase?
   (to-str (bitfield-type-new "foo" "bar" 10 T))
   (format nil "Parent type: foo~%Fields:~%Mem size: 10, load size: 10, signed T, align 10~%")))

(defun enum-types-test ()
  (let* ((base (value-type-new 'foo 'bar nil 10 nil))
         (enum1 (enum-type-new base 'bar nil (plist-hash-table '(:foo 1 :bar  2))))
         (enum2 (enum-type-new base 'bar nil (plist-hash-table '(:foo 1 :bar  2 :baz 3)))))
    (check-equal-downcase? (to-str enum1) "[EnumType] bar")
    (check-equal-downcase? (diff enum1 enum2)
			   (format nil "Entries are different:~%Number of entries 2 vs 3~%"))
    ))

(defun run-basic-types-test ()
  (value-type-test)
  (reference-type-test)
  (struct-type-test)
  (enum-types-test)
  )
