;;; xvmasm -- xvm's assembler

(in-package :xvmasm)

(defparameter *whitespace* '(#\Space #\Newline #\Tab #\Linefeed))
(defparameter *toksplitters* (append *whitespace* '(#\,)))

(defun tokenize (string)
  (let ((rval '())
	(slist (coerce string 'list))
	(tmp nil)
	(endloop nil)
	(x nil))
    (setf x (car slist)
	  slist (cdr slist))
    (loop until (or endloop (equal x nil)) do
	 ;(dformat t "x = ~a, tmp = ~a, slist = ~a, rval = ~a~%"
	;	 x tmp slist rval)
	 (cond
	   ((find-in-string x *toksplitters*)
	    (progn
	      ;(dformat t "found whitespace~%")
	      (if tmp
		  (progn
		    (setf rval (append rval (list (coerce tmp 'string))))
		    (setf tmp nil)))))
	   ((equal x #\")
	    (progn
	      ;(dformat t "found quote~%")
	      (if (not tmp)
		  (setf tmp (list x))
		  (setf rval (append rval (list (coerce tmp 'string)))
			tmp (list x)))
	      (setf x (car slist)
		    slist (cdr slist))
	      (loop until (equal x #\") do
		   (if (equal x nil)
		       (error "xvmasm: unbalanced quote"))
		   (setf tmp (append tmp (list x)))
		   (setf x (car slist)
			 slist (cdr slist)))
	      (setf tmp (append tmp (list x)))
	      (setf rval (append rval (list (coerce tmp 'string)))
		    tmp nil)))
	   ((equal x #\;)
	    (if tmp
		(setf rval (append rval (list (coerce tmp 'string)))))
	    (setf tmp nil
		  endloop t))
	   (t (progn
		;(dformat t "found char~%")
		(setf tmp (append tmp (list x))))))
	 (setf x (car slist)
	       slist (cdr slist)))
    (if tmp
	(progn
	  ;(dformat t "exited loop with filled tmp~%")
	  (setf rval (append rval (list (coerce tmp 'string))))))
    rval))

(defun upcase-tokens (toks)
  (let (tmp)
    (dolist (x toks)
      (if (not (equal (car (coerce x 'list)) #\"))
	  (setf tmp (append tmp (list (string-upcase x))))
	  (setf tmp (append tmp (list x)))))
    tmp))
