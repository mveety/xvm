;;; xvmasm -- xvm's assembler

(in-package :xvmasm)

(defun find-in-string (c lst)
  (let ((rval nil)
	tmp
	(ttype (type-of lst)))
    (if (equal (type-of ttype) 'cons)
	(if (and (equal (car ttype) 'simple-array)
		 (equal (car (cdr ttype)) 'character))
	    (setf tmp (coerce lst 'list))
	    (error "incorrect type"))
	(if (equal ttype 'cons)
	    (setf tmp lst)
	    (error "incorrect type")))
    (dolist (x tmp)
      (if (equal c x)
	  (setf rval t)))
    rval))

(defun print-block (stream &rest strings)
  (dolist (x strings)
    (format stream "~a~%" x)))

(defun find-string-in-list (lst string)
  (if (equal lst nil)
      nil
      (if (string-equal (car lst) string)
	  t
	  (find-string-in-list (cdr lst) string))))

(defun string-last (string)
  (subseq string (- (length string) 1)))

(defun string-first (string)
  (subseq string 0 1))

(defmacro append-to-list (lst dat)
  `(progn
    (setf ,lst (append ,lst (list ,dat)))
    ,lst))

(defun string-to-num (str)
  (let ((tmp nil))
    (if (> (length str) 3)
	(if (and (string-equal (subseq str 0 1) "0")
		 (or (string-equal (subseq str 1 2) "x")
		     (string-equal (subseq str 1 2) "X")))
	    (setf str (coerce (append (coerce "#16r" 'list)
				      (coerce (subseq str 2) 'list))
			      'string))))
    (setf tmp (read-from-string str))
    (if (not (numberp tmp))
	(error "string-to-num: not a number"))
    tmp))

(defun le-u32int-to-byte (u32int)
  (let ((rval nil)
	(fixes '(0 1 2 3)))
    (dolist (x fixes)
      (append-to-list rval (ldb (byte 8 (* 8 x)) u32int)))
    rval))

(defun le-u16int-to-byte (u16int)
  (let ((rval nil)
	(fixes '(0 1)))
    (dolist (x fixes)
      (append-to-list rval (ldb (byte 8 (* 8 x)) u16int)))
    rval))

(defun le-u8int-to-byte (u8int)
  (list (ldb (byte 8 0) u8int)))
