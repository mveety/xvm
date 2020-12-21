(in-package :xvmasm)

(defun get-instruction (string)
  (dolist (x *inst-list*)
    (if (string-equal (getf x :string) string)
	(return (list :inst (getf x :name))))))

(defun get-instruction-def (string)
  (dolist (x *inst-list*)
    (if (string-equal (getf x :string) string)
	(return x))))

(defun strip-mods (string)
  (if (find-in-string #\. string)
      (cond
	((equal (subseq string (- (length string) 2) (- (length string) 1)) ".")
	 (subseq string 0 (- (length string) 2)))
	((equal (subseq string (- (length string) 3) (- (length string) 2)) ".")
	 (subseq string 0 (- (length string) 3)))
	(t string))
      string))

(defun get-register (string)
  (dolist (x *reg-list*)
    (if (string-equal (getf x :string) string)
	(return (list :register (getf x :name))))))

(defun get-register-def (string)
  (dolist (x *reg-list*)
    (if (string-equal (getf x :string) string)
	(return x))))

(defun get-mods (string)
  (let* ((instdef (get-instruction-def (strip-mods string)))
	 (modmem nil)
	 (modwidth nil)
	 (smod1 nil)
	 (smod2 nil)
	 (smodmem nil)
	 (smodwidth nil)
	 (tmp nil))
    (if (and (or (getf instdef :modwidth)
		 (getf instdef :modmem))
	     (find-in-string #\. string))
	(block nil
	  (cond
	    ((equal (subseq string (- (length string) 3)
			    (- (length string) 2)) ".")
	     (setf tmp (subseq string (- (length string) 3) (length string))))
	    ((equal (subseq string (- (length string) 2)
			    (- (length string) 1)) ".")
	     (setf tmp (subseq string (- (length string) 2) (length string))))
	    (t (return '(:WIDTH nil :MODE nil))))
	  (if (or (equal (length tmp) 2) (equal (length tmp) 3))
	      (setf smod1 (subseq tmp 1 2)))
	  (if (equal (length tmp) 3)
	      (setf smod2 (subseq tmp 2 3)))
	  (if smod1
	      (cond
		((find-in-string (car (coerce smod1 'list)) "BHW")
		 (setf smodwidth smod1))
		((find-in-string (car (coerce smod1 'list)) "AR")
		 (setf smodmem smod1))))
	  (if smod2
	      (cond
		((find-in-string (car (coerce smod2 'list)) "BHW")
		 (setf smodwidth smod2))
		((find-in-string (car (coerce smod2 'list)) "AR")
		 (setf smodmem smod2))))
	  (if smodmem
	      (cond
		((equal smodmem "A") (setf modmem 'absolute))
		((equal smodmem "R") (setf modmem 'relative))))
	  (if smodwidth
	      (cond
		((equal smodwidth "B") (setf modwidth 'byte))
		((equal smodwidth "H") (setf modwidth 'halfword))
		((equal smodwidth "W") (setf modwidth 'word))))
	  (list :WIDTH (if (getf instdef :modwidth) modwidth nil)
		:MODE (if (getf instdef :modmem) modmem nil)))
	'(:WIDTH nil :MODE nil))))

(defun process-literal (string)
  (cond
    ((string-equal (subseq string 0 1) "@") (list 'label (subseq string 1)))
    ((string-equal (subseq string 0 1) "$") (list 'define (subseq string 1)))
    (t (string-to-num string))))

(defun process-instruction (tok-list)
  (let ((opstring (car tok-list))
	(args (cdr tok-list))
	(instdef (get-instruction-def (strip-mods (car tok-list))))
	(rval (list :opcode (strip-mods (car tok-list))
		    :size (getf (get-instruction-def (strip-mods
						      (car tok-list)))
				:length)
		    :mods nil
		    :args (list :reg1 nil :reg2 nil :const nil))))
    ;; process mods
    (setf (getf rval :mods) (get-mods opstring))
    ;; process args
    (if (not (equal (getf instdef :constarg) 0))
	(if (or (getf instdef :reverse)
		(equal (getf instdef :regargs) 0))
	    (setf (getf (getf rval :args) :const) (process-literal (car args)))
	    (setf (getf (getf rval :args) :const) (process-literal (cadr args))))
	(setf (getf (getf rval :args) :const) nil))
    (if (equal (getf instdef :regargs) 1)
	(if (getf instdef :reverse)
	    (setf (getf (getf rval :args) :reg1) (get-register (cadr args)))
	    (setf (getf (getf rval :args) :reg1) (get-register (car args)))))
    (if (equal (getf instdef :regargs) 2)
	(progn
	  (setf (getf (getf rval :args) :reg1) (get-register (car args)))
	  (setf (getf (getf rval :args) :reg2) (get-register (cadr args)))))
    (if (equal (getf instdef :regargs) 0)
	(progn
	  (setf (getf (getf rval :args) :reg1) nil)
	  (setf (getf (getf rval :args) :reg2) nil)))
    rval))
