;;;-----------------------------------minfree-----------------------------------
(defun front-seg (lst m)
  (remove-if-not #'(lambda (x) (<= x m))
				 lst))

(defun end-seg (lst m)
  (remove-if #'(lambda (x) (<= x m))
			 lst))

(defun util-search (lst l u)
  (let* ((mid (floor (/ (+ l u) 2)))
		 (front (front-seg lst mid))
		 (end (end-seg lst mid)))
	(cond ((null lst) l)
		  ((eql (length front) (+ (- mid l) 1))
		   (util-search end (+ mid 1) u))
		  (t (util-search front l mid)))))
		  
(defun minfree (lst)
  (util-search lst 1 (length lst)))

;;;------------------------------the number puzzle------------------------------
(defun min-first-queue (q1 q2 q3)
  ;;return the min element in three queue head
  ;;and the queue index of this element
  (let ((q1-h (car q1))
		(q2-h (car q2))
		(q3-h (car q3)))
	(cond ((and (< q1-h q2-h)
			  (< q1-h q2-h))
		 (values q1-h 1))
		((and (< q2-h q1-h)
			  (< q2-h q3-h))
		 (values q2-h 2))
		((and (< q3-h q1-h)
			  (< q3-h q2-h))
		 (values q3-h 3)))))

(defun next-queues (q1 q2 q3)
  ;;example (next-queues '(2) '(3) '(5)) --> '(4) '(3 6) '(5 10)
  (multiple-value-bind (min idx) (min-first-queue q1 q2 q3)
	(cond ((eql idx 1)
		   (values (append (cdr q1) (list (* 2 min)))
				   (append q2 (list (* 3 min)))
				   (append q3 (list (* 5 min)))))
		  ((eql idx 2)
		   (values q1
				   (append (cdr q2) (list (* 3 min)))
				   (append q3 (list (* 5 min)))))
		  ((eql idx 3)
		   (values q1
				   q2
				   (append (cdr q3) (list (* 5 min))))))))

(defun work-horse (n res q2 q3 q5)
  ;;this is the work horse function of num-puzzle
  ;;f(n,X,q2,q3,q5) = X if n == 1
  ;;                = f(n-1, X u x, nq2, nq3, nq5) otherwise
  ;;                which x = min-first-queue(q2, q3, q5)
  ;;                      nq2 nq3 nq5 = next-queue(q2 q3 q5)
  (cond ((eql n 1) (car res))
		(t
		 (multiple-value-bind (nq2 nq3 nq5)
			 (next-queues q2 q3 q5)
		   (work-horse (- n 1)
					   (cons (min-first-queue q2 q3 q5) res)
					   nq2
					   nq3
					   nq5)))))

(defun num-puzzle (n)
  ;;find the nth number which only contains factor 2,3,5
  (work-horse n '(1) '(2) '(3) '(5)))
