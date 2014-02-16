(in-package #:forthlike)

(defparameter *words* (make-instance 'dqueue))

(defmethod print-stack ((q dqueue))
  (format t "(~a) < " (len q))
  (loop for wd in (messages q)
     do (print-word wd) do (format t " "))
  (format t ">~%"))

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
  (second (assoc (intern word :keyword) (messages dict))))

(defmethod numeric? ((str string))
  (loop for c across str
     unless (> 58 (char-code c) 47) do (return nil)
     finally (return t)))

(defmethod parse-word ((dict dqueue) (word string))
  (cond ((numeric? word) (parse-integer word :junk-allowed t))
	(t (let ((wd (lookup dict word)))
	     (unless wd (format t "Undefined '~a'~%" word))
	     wd))))

(defmethod print-word (word) (format t "~s" word))

(defmethod eval-word ((dict dqueue) (stack dqueue) (in stream) word)
  (push! word stack))

(defmethod eval-word ((dict dqueue) (stack dqueue) (in stream) (word null)) t)

(defmethod eval-word ((dict dqueue) (stack dqueue) (in stream) (word function))
  (funcall word dict stack in))

(define-primitives
  "bye" (fn ())
  "." (fn () (print-word (pop! stack)) (format t "~%"))
  ".s" (fn () (print-stack stack))
  
  "true" :true
  "false" :false
  
  "dup" (fn () (push! (first (messages stack)) stack))
  "swap" (fn () (rotatef (first (messages stack)) 
			 (second (messages stack))))
  
  "'" (fn () (push! (pull-word! in) stack))
  "eval" (fn () (eval-word dict stack in (parse-word dict (pop! stack))))
  
  "\"" (fn () (push! (pull! #\" in) stack))
  
  "+" (fn () (push! (+ (pop! stack) (pop! stack)) stack))
  "*" (fn () (push! (* (pop! stack) (pop! stack)) stack))
  "/" (fn () (push! (/ (pop! stack) (pop! stack)) stack))
  "-" (fn () (push! (- (pop! stack) (pop! stack)) stack))
  
  "=" (fn () (push! (if (equal (pop! stack) (pop! stack)) :true :false)
		    stack))
  ">" (fn () (push! (if (> (pop! stack) (pop! stack)) :true :false)
		    stack))
  "<" (fn () (push! (if (< (pop! stack) (pop! stack)) :true :false)
		    stack))
  
  "not" (fn () (push! (if (eq :false (pop! stack)) :true :false) stack))
  "and" (fn () (push! (if (and (eq :true (pop! stack)) (eq :true (pop! stack))) :true :false) stack))
  "or" (fn () (push! (if (or (eq :true (pop! stack)) (eq :true (pop! stack))) :true :false) stack))

  "if" (fn () (let ((true? (eq (pop! stack) :true)))
		(loop for wd = (pull-word! in) until (string= wd "then")
		   when true? do (eval-word dict stack in (parse-word dict wd)))))
  
  ":" (fn () (let ((name (pull-word! in))
		   (words (loop for wd = (pull-word! in) until (string= wd ";") collect (parse-word dict wd))))
	       (intern! dict name (fn () (mapc (lambda (w) (eval-word dict stack in w)) words))))))

(defun repl ()
  (let ((stack (make-instance 'dqueue)))
    (loop do (format t "~~4>> ")
       while (multiple-value-bind (wd last-char)
		 (pull! (list #\space #\newline) *standard-input*)
	       (unless (eql last-char :eof)
		 (eval-word *words* stack *standard-input*
			    (parse-word *words* wd)))))))
