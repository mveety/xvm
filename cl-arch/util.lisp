(in-package :xvm)

(defmacro append-to-list (lst dat)
  `(progn
    (setf ,lst (append ,lst (list ,dat)))
    ,lst))

(declaim (inline le-u32int-to-list le-u16int-to-list le-u8int-to-list)
	 (ftype (function (fixnum) list) le-u32int-to-list)
	 (ftype (function (fixnum) list) le-u16int-to-list)
	 (ftype (function (fixnum) list) le-u8int-to-list))

(defun le-u32int-to-list (num)
  (let ((rval nil)
	(bound-num (u32int num))
	(fixes '(0 1 2 3)))
    (dolist (x fixes)
      (append-to-list rval (ldb (byte 8 (* 8 x)) bound-num)))
    rval))

(defun le-u16int-to-list (num)
  (let ((rval nil)
	(bound-num (u16int num))
	(fixes '(0 1)))
    (dolist (x fixes)
      (append-to-list rval (ldb (byte 8 (* 8 x)) bound-num)))
    rval))

(defun le-u8int-to-list (num)
  (let ((bound-num (u8int num)))
    (list (ldb (byte 8 0) bound-num))))

(defun le-list-to-u32int (lst)
  (let ((rval 0)
	(tmp lst)
	(fixes '(0 1 2 3)))
    (dolist (x fixes)
      (setf (ldb (byte 8 (* 8 x)) rval) (u8int (car tmp))
	    tmp (cdr tmp)))
    (u32int rval)))

(defun le-list-to-u16int (lst)
  (let ((rval 0))
    (setf (ldb (byte 8 0) rval) (car lst)
	  (ldb (byte 8 8) rval) (cadr lst))
    (u16int rval)))

(defun le-list-to-u8int (lst)
  (u8int (car lst)))

(defun le-list-to-uint (lst)
  (case (length lst)
    (1 (le-list-to-u8int lst))
    (2 (le-list-to-u16int lst))
    (3 (error "number must be a multiple of two"))
    (4 (le-list-to-u32int lst))
    (t (error "number is too long"))))

(defun make-byte-array (size)
  (make-array size
	      :element-type 'u8int
	      :initial-element 0
	      :adjustable nil))

(defmacro kilobytes (n)
  `(* ,n 1024))

(defmacro megabytes (n)
  `(* ,n (* 1024 1024)))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for binding in syms collect `(,binding (gensym)))
     ,@body))

(defmacro while (test &body body)
  (with-gensyms (start end rval)
    `(block nil
       (let (,rval)
	 (tagbody
	    ,start
	    (if (not ,test) (go ,end))
	    (setf ,rval (progn ,@body))
	    (go ,start)
	    ,end)
	 ,rval))))

(defmacro for (((&body bindings) test (&body iter)) &body body)
  (with-gensyms (start end rval)
    `(block nil
       (let (,rval)
	 (let (,@bindings)
	   (tagbody
	      ,start
	      (if (not ,test) (go ,end))
	      (setf ,rval (progn ,@body))
	      (progn ,@iter)
	      (go ,start)
	      ,end)
	   ,rval)))))

(defmacro defparameters (&rest var-pairs)
  `(progn
     ,@(loop for binds in var-pairs collect
	    `(defparameter ,(car binds) ,(cadr binds)))))

(defmacro defvars (&rest var-pairs)
  `(progn
     ,@(loop for var in var-pairs collect
	    `(defvar ,(car var) ,(cadr var)))))

(defmacro iota ((&rest vars) &optional (start 0) (delta 1))
  (let ((i start)
	(llist nil))
    (setf llist (loop for var in vars collect
		     `(,var ,(- (incf i delta) delta))))
    `(defparameters ,@llist)))

(defun range-list (start end &optional (delta 1))
  (let ((rval nil)
	(i start))
    (while (<= i end)
      (append-to-list rval i)
      (incf i delta))
    rval))

(defun find-in-list (e lst)
  (if (equal lst nil)
      nil
      (if (equal e (car lst))
	  t
	  (find-in-list e (cdr lst)))))

(defun remove-entry (entry lst)
  (let ((cur (car lst))
	(rest (cdr lst))
	(rval nil))
    (while (not (and (equal cur nil)
		     (equal (length rest) 0)))
      (if (not (equal entry cur))
	  (append-to-list rval cur))
      (setf cur (car rest)
	    rest (cdr rest)))
    rval))

(defmacro defun-block (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (block nil
       ,@body)))
