(in-package :forthlike)

(defmacro fn ((&rest args) &body body)
  (declare (ignore args))
  `(lambda (dict stack in)
     (declare (ignorable dict stack in))
     ,@body
     ,(when body t)))

(defmacro define-primitives (&rest name/def-list)
  `(progn ,@(loop for (name def) on name/def-list by #'cddr
	       collect `(intern! *words* ,name ,def))))

(defun parse-num (str)
  (multiple-value-bind (int end) (parse-integer str :junk-allowed t)
    (if (and int (/= end (length str)) (eq #\. (aref str end)))
	(ignore-errors
	  (multiple-value-bind (float f-end) (parse-integer str :start (+ end 1))
	    (+ int (float (/ float (expt 10 (- f-end end 1)))))))
	int)))
