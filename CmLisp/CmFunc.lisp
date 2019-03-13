(defun hello-sse ()
  "This is a function to test documentation"
  (format t "Hello SSE"))

(defun test-optional (a b &optional (c 10) d)
  (list a b c d))

(defun make-rectangle (width &optional (length width))
  (list width length))

(defun test-is-supplied (a &optional (b 10 b-supplied-p))
  (list a b b-supplied-p))

(defun test-rest (&rest names)
  (format t "The number of names: ~a~%" (length names)))

(defun test-keyword-paras (&key a b c)
  (list a b c))

(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

;;;mix different parameter types
(defun test-mix (x &optional y &key z) ;bad style, &optional and &key found in the same list
  (list x y z))

;;; return from a function
(defun test-dotimes ()
  (dotimes (i 10)
    (format t "This is the ~ath~%" i)))

(defun test-func-return (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from test-func-return (list i j))))))

;;;high-order functions
(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))

(defun my-verbose-sum (a b)
  (+ a b))

(defun echo (a)
  a)
