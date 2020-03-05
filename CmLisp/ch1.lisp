(defun get-times-recur (a lst)
  (if (null lst)
	  0
	  (if (eql a (car lst))
		  (+ 1 (get-times-recur a (cdr lst)))
		  (get-times-recur a (cdr lst)))))

(defun get-times-iter (a lst)
  (let ((times 0))
	(dolist (e lst)
	  (if (eql a e)
		  (setf times (+ times 1))))
	times))

(defun my-summit (lst)
  (if (null lst)
	  0
	  (if (listp (car lst))
		  (+ (my-summit (car lst)) (my-summit (cdr lst)))
		  (+ (car lst) (my-summit (cdr lst))))))
