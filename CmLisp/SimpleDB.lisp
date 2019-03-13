(defvar *db* nil)

(defun make-cd (title artist rating tipped)
  (list :title title :artist artist :rating rating :tipped tipped))

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [Y/N]: ")))

;;add cds to the database, using some utils functions
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;;dump the database to a file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;;load the file into the database
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;tell whether a word is a palindrome
(defun is-palindrome (str)
  (string= str (reverse str)))

;;select by artist
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (string= (getf cd :artist) artist))
   *db*))

;;general selecter
(defun select (select-fn)
  (remove-if-not select-fn *db*))

;;select function generator
(defun artist-selector (artist)
  #'(lambda (cd) (string= (getf cd :artist) artist)))

(defun title-selector (title)
  #'(lambda (cd) (string= (getf cd :title) title)))

;;test keyword parameters
(defun foo (&key a (b 20) (c 30 c-p))
  (list a b c c-p))

;;general selector function
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (string= (getf cd :title) title) t)
       (if artist (string= (getf cd :artist) artist) t)
       (if rating (string= (getf cd :rating) rating) t)
       (if ripped-p (string= (getf cd :ripped) ripped) t))))

;;updating exsit record
(defun update (select-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall select-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))
