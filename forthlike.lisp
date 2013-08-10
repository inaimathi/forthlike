(in-package #:forthlike)

(defparameter *stack* nil)
(defparameter *words* (make-hash-table :test #'equal))
(defparameter *input* "")

(defun pull! (&optional (looking-for #\ ))
  (multiple-value-bind (word len) (split-sequence looking-for *input* :count 1)
    (setf *input* (subseq *input* len))
    (first word)))

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

(def "`" (push! (pull!)))
(def "," (funcall (gethash (pop!) *words*)))
(def "\"" (push! (format nil "~s" (pull! "\""))))

(def "dup" (push! (first *stack*)))

(def "+" (push! (+ (pop!) (pop!))))
(def "*" (push! (* (pop!) (pop!))))
(def "/" (with-pop! (b) (push! (/ (pop!) b))))
(def "-" (with-pop! (b) (push! (- (pop!) b))))

(def "=" (push! (bif (equal (pop!) (pop!)))))
(def ">" (push! (bif (with-pop (b) (> (pop!) b)))))
(def "not" (push! (if (string= (pop!) "false") "true" "false")))
(def "and" (push! (with-pop! (a b) (bif (and (string= "true" a) (string= "true" b))))))
(def "or" (push! (with-pop! (a b) (bif (or (string= "true" a) (string= "true" b))))))
(def "if" (if (string= (pop!) "true")
	      (with-pop! (a) (pop!) (ev a))
	      (progn (pop!) (ev (pop!)))))

(def ":" (let ((name (pull!))
	       (words (loop for wd = (pull!) until (string= wd ";") collect wd)))
	   (def name (mapc #'ev words))))

(defun forthlike-eval (str)
  (setf *input* str)
  (loop until (string= *input* "") do (ev (pull!))))

(defun forthlike-load (file-path)
  (with-open-file (s file-path)
    (loop for res = (read-line s nil :eof)
       until (eq res :eof)
	 do (forthlike-eval res))))

(defun repl ()
  (loop for line = (progn (format t "~~4th >> ") (read-line)) 
     until (string= line "bye") do (forthlike-eval line)))