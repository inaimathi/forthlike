(in-package #:forthlike)

(defclass dqueue ()
  ((messages :accessor messages :initform nil)
   (last-cons :accessor last-cons :initform nil)
   (len :accessor len :initform 0)))

(defmethod set-first! (message (q dqueue))
  (with-slots (messages last-cons) q
    (setf messages (list message)
	  last-cons messages)))

(defmethod enqueue! (message (q dqueue))
  (with-slots (last-cons) q
    (if (empty? q)
	(set-first! message q)
	(setf (cdr last-cons) (list message)
	      last-cons (cdr last-cons)))
    (incf (len q))
    (messages q)))

(defmethod push! (message (q dqueue))
  (if (empty? q)
      (set-first! message q)
      (push message (messages q)))
  (incf (len q))
  (messages q))

(defmethod pop! ((q dqueue))
  (if (empty? q)
      (values nil nil)
      (progn 
	(decf (len q))
	(values (pop (messages q)) t))))

(defmethod empty? ((q dqueue)) (= 0 (len q)))
