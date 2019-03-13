(defun test-redis ()
  ;;(redis:connect :host "127.0.0.1" :port 6379))
  (redis:with-connection ()
    (red:ping)))
