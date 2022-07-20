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

(defmacro check-true (a)
  ;; `(is (not (equalp ,a ,expected))
  ;;      (format nil "~%  Expected not be equal~%   given: ~a~%  expect: ~a~%" ,a ,expected)))
  `(let ((check-true-result ,a))
     (unless check-true-result
       (error (format nil "~%  Expected TRUE given: ~a~%" (wrapstring check-true-result))))))


(defun string-trim! (s)
  (let* ((l1 (string-downcase s))
	 (l2 (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) l1))
	 (l3 (cl-ppcre:regex-replace-all "(\\n)" l2 ""))
	 (l4 (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) l3))
	 (l5 (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) l4))
	 (l6 (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) l5))
	 )
    l6))

(defmacro check-equal-downcase? (a expected)
  `(let ((s1 (string-trim! ,a))
	 (s2 (string-trim! ,expected)))
     (unless (equalp s1 s2)
       (error (format nil "~%  Expected equal~%   given: ~a~%  expect: ~a~%"
		      (wrapstring s1)
		      (wrapstring s2))))))

    
