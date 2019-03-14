;;;;This is a project to test cl-redis and Redis system

(ql:quickload "cl-redis")
;;;;-------------------------------------------------------------------
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

;;;;-------------------------------------------------------------------

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

(defun test-mset-get ()
  (when (redis:red-mset "a" 10 "b" 20 "c" 30)
    (format t "multi set key ok!~%"))
  (format t "~a~%" (redis:red-mget "a" "b")))

;;;Test if a key exists and delete a key and its value
(defun is-key-exist (key)
  (redis:red-exists key))

(defun del-exist-key (key)
  (if (is-key-exist key)
      (redis:red-del key)
      (format t "key not exist!~%")))

;;;Test redis expires: keys with limited time to live
(defun test-expires (key value secs)
  ;;if key is alreay exist, getset the value
  ;;else set the value
  (if (is-key-exist key)
      (redis:red-expire key secs)
      (progn
        (redis:red-set key value)
        (redis:red-expire key secs))))
;;;;-------------------------------------------------------------------


;;;Test list push and range
(defun test-push-range (lst-name)
  (redis:red-rpush lst-name "A")
  (redis:red-rpush lst-name "B")
  (redis:red-lpush lst-name "first")
  (redis:red-lrange lst-name 0 -1))

;;;Test capped list
(defun test-capped-list (lst-name)
  (dotimes (i 10)
    (redis:red-lpush lst-name i))
  (redis:red-ltrim lst-name 0 3)
  (redis:red-lrange lst-name 0 -1))

;;;;-------------------------------------------------------------------

;;;Test Hash
(defun test-hash-set-get (hash-name)
  (redis:red-hmset hash-name
                   "username" "antirez"
                   "birthyear" "1977"
                   "verified" "1")
  (redis:red-hget hash-name "username")               ;;retrive a single field
  (redis:red-hmget hash-name "username" "verified")   ;;retrive multi fields
  ;; do some opration on individual field
  (redis:red-hincrby hash-name "birthyear" 10))

;;;;-------------------------------------------------------------------

;;;Test Redis Set
(defun test-redis-set (set-name)
  (redis:red-sadd set-name 1 2 3 0)
  (redis:red-smembers set-name)
  ;;test for membership
  (when (redis:red-sismember set-name 2)
    (format t "~a is a member of ~a~%" 2 set-name))
  (when (not (redis:red-sismember set-name 10))
    (format t "~a is not a member of ~a~%" 10 set-name)))

;;test redis intersection
(defun test-sinter ()
  (redis:red-sadd "set1" 1 2 3 4)
  (redis:red-sadd "set2" 2 3 4 5)
  (redis:red-sinter "set1" "set2"))

;;test redis union
(defun test-union ()
  (redis:red-sadd "set1" 1 2 3 4)
  (redis:red-sadd "set2" 2 3 4 5)
  (redis:red-sunion "set1" "set2"))

;;test redis diff
(defun test-diff ()
  (redis:red-sadd "set1" 1 2 3 4)
  (redis:red-sadd "set2" 2 3 4 5)
  (redis:red-sdiff "set1" "set2"))


;;A card game, test spop
(defun card-game ()
  (let ((deck (gen-deck)))
    ;;put the deck into a set
    (dolist (elem deck)
      (redis:red-sadd "cards" elem))
    ;;(redis:red-smembers "cards")))
    (dotimes (i 5)
      (format t "~a~%" (redis:red-spop "cards")))))

;;generate a deck 4 * 13 = 52
(defun gen-deck ()
  (let ((prefix-lst '("C" "D" "H" "S"))
        (card-num 13)
        (res nil))
    (dolist (elem prefix-lst)
      (dotimes (i card-num)
        (push (format nil "~a~a" elem i) res)))
    res))

;;;;-------------------------------------------------------------------
