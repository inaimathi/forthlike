(in-package :forthlike)

(defun println (thing) (format t "~a~%" thing))

(defmacro aif (test if-true &optional if-false)
  `(let ((it ,test))
     (if it ,if-true ,if-false)))

(defmacro bif (test) `(if ,test "true" "false"))

(defmacro with-pop! ((&rest symbols) &body body)
  `(let ,(loop for s in symbols collect `(,s (pop!)))
     ,@body))

(defun parse-num (str)
  (multiple-value-bind (int end) (parse-integer str :junk-allowed t)
    (if (and int (/= end (length str)) (eq #\. (aref str end)))
	(ignore-errors
	  (multiple-value-bind (float f-end) (parse-integer str :start (+ end 1))
	    (+ int (float (/ float (expt 10 (- f-end end 1)))))))
	int)))