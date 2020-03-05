;;;run-length encoding:Compression
(defun n-elts (elt n)
  (if (> n 1)
	  (list n elt)
	  elt))

(defun compr (elt n lst)
  (if (null lst)
	  (list (n-elts elt n))
	  (let ((next (car lst)))
		(if (eql next elt)
			(compr elt (+ n 1) (cdr lst))
			(cons (n-elts elt n)
				  (compr next 1 (cdr lst)))))))

(defun compress (x)
  (if (consp x)
	  (compr (car x) 1 (cdr x))
	  x))

;;;run-length encoding:Expansion
(defun list-of (n elt)
  (if (zerop n)
	  nil
	  (cons elt (list-of (- n 1) elt))))

(defun uncompress (lst)
  (if (null lst)
	  nil
	  (let ((elt (car lst))
			(rest (uncompress (cdr lst))))
		(if (consp elt)
			(append (apply #'list-of elt)
					rest)
			(cons elt rest)))))

(defun my-nth (num lst)
  (if (null lst)
	  nil
	  (if (eql num 0)
		  (car lst)
		  (my-nth (- num 1) (cdr lst)))))

(defun my-nth-cdr (num lst)
  (if (null lst)
	  nil
	  (if (eql num 0)
		  lst
		  (my-nth-cdr (- num 1) (cdr lst)))))

(defun my-last (lst)
  (my-nth-cdr (- (length lst) 1) lst))

(defun summit (lst)
  (let ((res 0))
	(if (null lst)
		res
		(+ (car lst) (summit (cdr lst))))))

;;;treat list like trees
(defun my-copy-tree (tr)
  (if (atom tr)
	  tr
	  (cons (my-copy-tree (car tr))
			(my-copy-tree (cdr tr)))))

(defun my-copy-list (lst)
  (if (atom lst)
	  lst
	  (cons (car lst)
			(my-copy-list (cdr lst)))))

(defun my-subst (new old tree)
  (if (eql tree old)
	  new
	  (if (atom tree)
		  tree
		  (cons (my-subst new old (car tree))
				(my-subst new old (cdr tree))))))



;;;-------------2. New Union--------------------------
;;; A new kind of union, remove element in a list but
;;; keep the original element order
(defun new-remove (elem set)
  (let ((res-set nil))
	(dolist (e set)
	  (if (not (eql e elem))
		  (push e res-set)))
	(reverse res-set)))

;;; remove sub-set from set, keep the original order of
;;; the set
(defun new-set-remove (sub-set set)
  (let ((res-set set))
	(dolist (e sub-set)
	  (setf res-set (new-remove e res-set)))
	res-set))

;;; new set union. keep the original order of the sets
(defun new-union (set1 set2)
  (let ((set-intersection (intersection set1 set2)))
	(append set1 (new-set-remove set-intersection set2))))


;;;----------3. Occurrence------------------------------------

;;; rew calulate the occurrence
(defun raw-cal (lst)
  (let ((res-alist nil))
	(dolist (e lst)
	  (let ((assoc-elem (assoc e res-alist)))
		(if assoc-elem
			(let ((pos (position assoc-elem res-alist)))
			  (setf (nth pos res-alist) (cons (car assoc-elem) (incf (cdr assoc-elem)))))
			(push (cons e 1) res-alist))))
	res-alist))

;;sort the raw result
(defun occurences (lst)
  (let ((raw-occur (raw-cal lst)))
	(sort raw-occur #'(lambda (x y) (> (cdr x) (cdr y))))))


;;;----------4. Pos+------------------------------------------
(defun rev-car (lst)
  (car (last lst)))

(defun rev-cdr (lst)
  (reverse (cdr (reverse lst))))

(defun rev-cons (e lst)
  (reverse (cons e (reverse lst))))

(defun pos+-recur (lst)
  (if (null lst)
	  nil
	  (rev-cons (+ (rev-car lst) (- (length lst) 1))
				(pos+-recur (rev-cdr lst)))))

(defun pos+-iter (lst)
  (let ((res nil) (pos 0))
	(dolist (e lst)
	  (progn
		(setf res (cons (+ e pos) res))
		(setf pos (+ pos 1))))
	(reverse res)))


(defun pos+-mapcar (lst)
  (let ((pos 0))
	(mapcar #'(lambda (x)
				(setf x (+ x pos))
				(setf pos (+ pos 1))
				x)
			lst)))

;;;-------5. New Govement-----------------------------------
(defun gov-car (lst)
  (cdr lst))

(defun gov-cdr (lst)
  (car lst))

(defun gov-cons (e lst)
  (append lst (cons e nil)))

(defun gov-list (&rest elems)
  (let ((res nil))
	(dolist (e elems)
	  (setf res (gov-cons e res)))
	res))

(defun gov-length (lst)
  (if (null lst)
	  0
	  (+ 1 (gov-length (gov-car lst)))))

(defun gov-member (e lst)
  (if (null lst)
	  nil
	  (if (eql (gov-cdr lst) e)
		  lst
		  (gov-member e (gov-car lst)))))


;;;----------7. Show dots------------------------------------
(defun print-char-n-times (c n)
  (do ((i 1 (+ i 1)))
	  ((> i n))
	(format t "~A" c)))

(defun show-work (cnt lst)
  (if (null lst)
	  (progn
		(format t "NIL")
		(print-char-n-times ")" cnt))
	  (progn
		  (format t "(~A . " (car lst))
		  (setf cnt (+ cnt 1))
		  (show-dots cnt (cdr lst)))))


