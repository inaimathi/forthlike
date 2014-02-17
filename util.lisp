(in-package :forthlike)

(define-condition forth-error (error) ())
(define-condition repl-break (forth-error) ())
(define-condition undefined-word (forth-error) ())
(define-condition stack-underflow (forth-error) ())
(define-condition unexpected-type (forth-error) ())

(defun fn-argcount (arg-checks)
  (when arg-checks
    `((unless (>= (len stack) 
		  ,(if (numberp (first arg-checks)) 
		       (first arg-checks)
		       (length arg-checks)))
	(error (make-instance 'stack-underflow))))))

(defun fn-argcheck (arg-checks)
  (unless (or (null arg-checks) (numberp (first arg-checks)))
    (with-gensyms (elem tp)
      `((loop for ,tp in '(,@arg-checks) for ,elem in (messages stack)
	   unless (typep ,elem ,tp) do (error (make-instance 'unexpected-type)))))))

(defmacro fn ((&rest arg-checks) &body body)
  `(lambda (dict stack in)
     (declare (ignorable dict stack in))
     ,@(fn-argcount arg-checks)
     ,@(fn-argcheck arg-checks)
     ,@body))

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
