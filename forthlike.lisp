(in-package #:forthlike)

(defparameter *words* (make-instance 'dqueue))

(defmethod pull! ((looking-for list) (s stream))
  (loop for c = (read-char s nil :eof)
     until (or (member c looking-for) (eq c :eof)) collect c into word
     finally (return (values (coerce word 'string) c))))

(defmethod pull! ((looking-for character) (s stream))
  (loop for c = (read-char s nil :eof)
     until (or (eql c looking-for) (eq c :eof)) collect c into word
     finally (return (values (coerce word 'string) c))))

(defmethod pull-word! ((s stream)) (pull! (list #\space #\newline) s))

(defmethod intern! ((dict dqueue) (name string) value)
  (push! (list (intern name :keyword) value) dict))

(defmethod lookup ((dict dqueue) (word string))
  (aif (assoc (intern word :keyword) (messages dict))
    (values (second it) t)
    (values nil nil)))

(defmethod numeric? ((str string))
  (loop for c across str
     unless (> 58 (char-code c) 47) do (return nil)
     finally (return t)))

(defmethod parse-word ((dict dqueue) (word string))
  (cond ((string= "" word) "")
	((numeric? word) (parse-integer word :junk-allowed t))
	(t (multiple-value-bind (wd found?) (lookup dict word)
	     (unless found? (error (make-instance 'undefined-word)))
	     wd))))

(defmethod eval-word ((dict dqueue) (stack dqueue) (in stream) word)
  (push! word stack))

(defmethod eval-word ((dict dqueue) (stack dqueue) (in stream) (word string))
  (unless (string= "" word) (call-next-method)))

(defmethod eval-word ((dict dqueue) (stack dqueue) (in stream) (word function))
  (funcall word dict stack in))

(define-primitives
  "bye" (fn () (error (make-instance 'repl-break)))
  "." (fn (wd) (print-word wd) (ln))
  ".s" (fn () (print-stack stack) (ln))
  
  "true" t
  "false" nil
  
  "dup" (fn (a) (push! a stack) (push! a stack))
  "swap" (fn (a b)
	   (push! a stack)
	   (push! b stack))
  
  "'" (fn () (push! (pull-word! in) stack))
  "eval" (fn (wd) (eval-word dict stack in (parse-word dict wd)))
  
  "\"" (fn () (push! (pull! #\" in) stack))
  
  "+" (fn ((a number) (b number)) (push! (+ a b) stack))
  "*" (fn ((a number) (b number)) (push! (* a b) stack))
  "/" (fn ((a number) (b number)) (push! (/ a b) stack))
  "-" (fn ((a number) (b number)) (push! (- a b) stack))
  
  "=" (fn (a b) (push! (equal a b) stack))
  ">" (fn ((a number) (b number)) (push! (> a b) stack))
  "<" (fn ((a number) (b number)) (push! (< a b) stack))
  
  "not" (fn ((a boolean)) (push! (not a) stack))
  "and" (fn ((a boolean) (b boolean)) (push! (and a b) stack))
  "or" (fn ((a boolean) (b boolean)) (push! (or a b) stack))

  "if" (fn ((true? boolean))
	 (loop for wd = (pull-word! in) until (string= wd "then")
	    when true? do (eval-word dict stack in (parse-word dict wd))))
  
  ":" (fn () 
	(let ((name (pull-word! in))
	      (words (loop for wd = (pull-word! in)   
			until (string= wd ";") 
			if (string= wd "\"") collect (pull! #\" in)
			else collect (parse-word dict wd))))
	  (intern! dict name (fn () (mapc (lambda (w) (eval-word dict stack in w)) words))))))

(defun repl ()
  (let ((stack (make-instance 'dqueue)))
    (handler-case 
	(loop (progn (format t "~~4>> ")
		     (multiple-value-bind (wd last-char) (pull! (list #\space #\newline) *standard-input*)
		       (handler-case
			   (unless (eql last-char :eof)
			     (eval-word *words* stack *standard-input*
					(parse-word *words* wd)))
			 (stack-underflow (e) (print-error :stack-underflow wd stack e))
			 (unexpected-type (e) (print-error :unexpected-type wd stack e))
			 (undefined-word (e) (print-error :undefined-word wd stack e))))))
      (repl-break () (format t "Cheers!")))))
