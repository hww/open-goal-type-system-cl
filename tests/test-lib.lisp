(in-package :test-lib)

(defmacro multilinep (s)
  (if (and (stringp s) (find #\newline s)) t nil))

(defun wrapstring (s)
  (if (stringp s)
      (format nil "[~a] ~s" (length s) s)
      s))

(defmacro check-equal? (a expected)
  ;; `(is (equalp ,a ,expected)
  ;;      (format nil "~%  Expected equal~%   given: ~a~%  expect: ~a~%" ,a ,expected))) 
  `(unless (equalp ,a ,expected)
     (error (format nil "~%  Expected equal~%   given: ~a~%  expect: ~a~%"
		    (wrapstring ,a)
		    (wrapstring ,expected)))))

(defmacro check-not-equal? (a expected)
  ;; `(is (not (equalp ,a ,expected))
  ;;      (format nil "~%  Expected not be equal~%   given: ~a~%  expect: ~a~%" ,a ,expected)))
  `(unless (not (equalp ,a ,expected))
     (error (format nil "~%  Expected not be equal~%   given: ~a~%  expect: ~a~%"
		    (wrapstring ,a)
		    (wrapstring ,expected)))))

