;;; functions.el -*- lexical-binding: t; -*-

;; TODO get this to work
;; seems to be an issue around the =parse-iso8601= even though it says it should work
(defun oll/get-unix-timestamp(time)
  "Converts the given time to a unixtimestamp in seconds since the epoch."
  (interactive "sTime: ")
  (insert (prin1-to-string (time-convert (parse-iso8601-time-string time) 'integer)))
  )
