;;;Test list push and range
(defun test-push-range (lst-name)
  (redis:red-rpush lst-name "A")
  (redis:red-rpush lst-name "B")
  (redis:red-lpush lst-name "first")
  (redis:red-lrange lst-name 0 -1))

;;Test capped list
(defun test-capped-list (lst-name)
  (dotimes (i 10)
    (redis:red-lpush lst-name i))
  (redis:red-ltrim lst-name 0 3)
  (redis:red-lrange lst-name 0 -1))
