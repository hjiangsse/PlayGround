;;;Variable Basics
(defun test-shadows (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter: ~a~%" x))

;;;Lexical Variables and Closures
(defun gen-counter (start)
  #'(lambda ()
      (setf start (+ start 1))))

(defun two-funs (x)
  (list (function (lambda () x))
        (function (lambda (y) (setq x y)))))

;;;Dynamic Variables
(defun test-bind ()
  (format t "Before assignment ~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment ~18tX: ~d~%" *x*))
