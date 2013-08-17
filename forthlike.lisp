(in-package #:forthlike)

;;; Types of things:
;;; numbers, symbols, streams, functions
;;; - ints and floats are numbers
;;; - strings, lists, etc are streams
;;; - lambdas are functions
;;; - anything else is a symbol (if a symbol is assigned, it evaluates to its value, otherwise it evaluates to itself)
;;; - booleans are symbols (yes, that means you can re-assign :true and :false. It's dumb, but you can)
;;; - should hashes be their own thing, or can they be represented as streams?

(defparameter *stack* nil)
(defparameter *words* (make-hash-table :test #'equal))
(defparameter *input* (make-string-input-stream ""))

(defun pull! (&optional (looking-for #\ ))
  (loop for c = (read-char *input* nil :eof)
     until (or (eq c looking-for) (eq c :eof)) 
     collect c into word
     finally (return (values (when word (coerce word 'string)) c))))

(defun pop! () (pop *stack*))

(defun push! (thing) (push thing *stack*))

(defun ev (word)
  (if (or (string= "true" word) (string= "false" word))
      (push! word)
      (aif (parse-num word)
	   (push! it)
	   (aif (gethash word *words*)
		(funcall it)
		(format t "Unknown word: ~s~%" word)))))

(defmacro def (name &body body)
  `(setf (gethash ,name *words*) (lambda () ,@body)))

(def "." (println (pop!)))
(def ".s"
  (println "")
  (if *stack*
      (loop for i from 0 for elem in *stack*
	 do (format t "< ~a > :: ~a~%" i elem))
      (format t "< Empty stack >~%"))
  (println ""))

(def "'" (push! (pull!)))
(def "," (funcall (gethash (pop!) *words*)))
(def "\"" (push! (format nil "~s" (aif (pull! #\") it ""))))

(def "dup" (push! (first *stack*)))
(def "swap" (rotatef (first *stack*) (second *stack*)))

(def "+" (push! (+ (pop!) (pop!))))
(def "*" (push! (* (pop!) (pop!))))
(def "/" (with-pop! (b) (push! (/ (pop!) b))))
(def "-" (with-pop! (b) (push! (- (pop!) b))))

(def "=" (push! (bif (equal (pop!) (pop!)))))
(def ">" (push! (bif (with-pop (b) (> (pop!) b)))))
(def "<" (push! (bif (with-pop (b) (< (pop!) b)))))
(def "not" (push! (if (string= (pop!) "false") "true" "false")))
(def "and" (push! (with-pop! (a b) (bif (and (string= "true" a) (string= "true" b))))))
(def "or" (push! (with-pop! (a b) (bif (or (string= "true" a) (string= "true" b))))))
(def "if" (let ((true? (string= (pop!) "true")))
	    (loop for wd = (pull!) until (string= wd "then") 
	       when true? do (ev wd))))

(def ":" (let ((name (pull!))
	       (words (loop for wd = (pull!) until (string= wd ";") collect wd)))
	   (def name (mapc #'ev words))))


(defun forthlike-eval (str)
  (setf *input* (make-string-input-stream str))
  (loop do (multiple-value-bind (word ends-with) (pull!)
	     (when word 
	       (ev word)
	       (peek-char t *input* nil :eof))
	     (when (eq :eof ends-with) (return)))))

(defun forthlike-load (file-path)
  (with-open-file (s file-path)
    (loop for res = (read-line s nil :eof)
       until (eq res :eof)
	 do (forthlike-eval res))))

(defun repl ()
  (loop for line = (progn (format t "~~4th >> ") (read-line)) 
     until (string= line "bye") do (forthlike-eval line)))