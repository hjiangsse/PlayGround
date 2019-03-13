;;When and Unless
(defmacro my-when (condition &rest body)
  "define my own when macro"
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &rest body)
  "define my own unless macro"
  `(if (not ,condition) (progn ,@body)))

;;Cond
(defun test-cond ()
  (let ((rand-num (random 5)))
    (cond ((= rand-num 0)
           (format t "I gussed a zero."))
          ((= rand-num 1)
           (format t "I gussed a one."))
          ((= rand-num 2)
           (format t "I gussed a two."))
          ((= rand-num 3)
           (format t "I gussed a three."))
          ((= rand-num 4)
           (format t "I gussed a four."))
          (t (format t "I guess something very big.")))))

;;AND, OR and NOT

;;Looping in Common Lisp
(defun echo-lst (lst)
  "echo a list, index followed by element"
  (let ((index 0))
    (dolist (elem lst)
      (format t "~a:~a~%" index elem)
      (incf index))))

;;find the first palindrome in a string list
(defun find-first-panlidrome (str-lst)
  (dolist (elem str-lst)
    (if (funcall #'(lambda (x)
                     (string= (reverse x) x))
                 elem)
        (progn
          (format t "~a~%" elem)
          (return)))))

;;time table
(defun print-times-table (x y)
  "Print a x * y times table"
  (dotimes (i x)
    (dotimes (j y)
      (format t "~a * ~a = ~d |" i j (* i j)))
    (format t "~%")))

;;More general DO loop
(defun test-do ()
  (do ((i 0 (+ i 1))
       (j 0 (+ j 1)))
      ((> (+ i j) 10) (format t "~a ~a~%" i j))))

;;Test the LOOP macro
(defun test-loop ()
  ;;infinite loop
