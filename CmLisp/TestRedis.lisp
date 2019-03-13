;;;;This is a project to test cl-redis and Redis system

(ql:quickload "cl-redis")

;;;Connect to a Redis database
(defun connect-redis (&optional (addr "127.0.0.1")
                        (port 6379))
  (redis:connect :host addr :port port))

;;Disconnect the Redis database
(defun disconnect-redis ()
  (redis:disconnect))

;;;the follwing are some test functions
(defun test-con-and-discon ()
  (if (connect-redis)
      (format t "Connect Redis OK!~%"))
  (if (disconnect-redis)
      (format t "Disconnect Redis OK!~%")))

;;;String Test
(defun test-string-get-set ()
  (when (redis:red-set "counter" 10)
    (format t "red-set ok!~%"))
  (format t "get the counter: ~a~%"
          (redis:red-get "counter"))
  (when (redis:red-incr "counter") ;;increment the integer value of KEY
    (format t "after incr, get the counter: ~a~%"
            (redis:red-get "counter")))
  (when (redis:red-incrby "counter" 10)
    (format t "after incrny 10, get the counter: ~a~%"
            (redis:red-get "counter")))
  (when (redis:red-decr "counter") ;;decreement the interger value of KEY
    (format t "after decr, get the counter: ~a~%"
            (redis:red-get "counter")))
  (when (redis:red-decrby "counter" 15) ;;decreement the interger value of KEY
    (format t "after decrby 15, get the counter: ~a~%"
            (redis:red-get "counter"))))
