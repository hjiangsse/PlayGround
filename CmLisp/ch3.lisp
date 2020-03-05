(defun make-inc-array (n)
  (let ((init-array (make-array n)))
	(do ((i 1 (+ i 1)))
		((> i n))
		(setf (aref init-array (- i 1)) i))
	init-array))

;;;Binary Search a sorted vector
(defun bi-search-work (dest vec start end)
  (if (= start end)
	  (if (equal dest (svref vec start))
		  start
		  nil)
	  (let* ((mid-pos (floor (/ (+ start end) 2)))
			 (mid-elem (svref vec mid-pos)))
		(if (equal dest mid-elem)
			mid-elem
			(if (< dest mid-elem)
				(bi-search-work dest vec start (- mid-pos 1))
				(bi-search-work dest vec (+ mid-pos 1) end))))))

(defun bi-search (elem vec)
  (bi-search-work elem vec 0 (- (length vec) 1)))

(defun second-word (str)
  (let ((first-space-pos (position #\  str)))
	(if (null first-space-pos)
		nil
		(let ((p1 (+ first-space-pos 1)))
		  (subseq str p1 (position #\  str :start p1))))))


;;;-----------------Parsing Dates----------------------------
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
	(if p1
		(let ((p2 (position-if #'(lambda (c)
								   (not (funcall test c)))
							   str :start p1)))
		  (cons (subseq str p1 p2)
				(if p2
					(tokens str test p2)
					nil)))
		nil)))

(defun constituent (c)
  (and (graphic-char-p c)
	   (not (char= c #\ ))))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
	"jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
					:test #'string-equal)))
	(if p
		(+ p 1)
		nil)))

(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
	(list (parse-integer (first toks))
		  (parse-month (second toks))
		  (parse-integer (third toks)))))

;;;----------------Structure---------------------------------
