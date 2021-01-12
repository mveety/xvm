(in-package :xvm)

(defun test-get-microinst (name)
  (let ((insts *inst-list*)
	(rval nil))
    (dolist (x insts)
      (unless rval
	(if (equal name (getf x :name))
	    (setf rval x))))
    rval))

(defun assemble (start-address strings)
  (let* ((datastream (xvmasm::assemble-strings start-address strings)))
    (load-list-to-memory datastream start-address)))

(defun asm (start-address &rest strings)
  (assemble start-address strings))

(defun memory-dump (start len)
  (let ((rval nil)
	(fixes (range-list start (1- (+ start len)))))
    (dolist (x fixes)
      (append-to-list rval (memory x)))
    rval))

(defun run-cycle (cpu)
  (let* ((fetch-data (fetch cpu))
	 (decode-data (decode cpu fetch-data)))
    (exec cpu decode-data)))

;;; quick and dirty function to more easily test the processor
(defun asm-test (reg-initials reg-results &rest insts)
  (let ((tcpu (make-cpu))
	asmres
	(retval t))
    (dolist (x reg-initials)
      (setf (register tcpu (car x)) (cadr x)))
    (if (equal *memory-map* nil)
	(initialize-all-memory))
    (unmap-sysman)
    (setf (cpu-halted tcpu) nil)
    (setf asmres (assemble 0 insts))
    (if (not (equal (getf asmres :size) (getf asmres :written)))
	(error "unable to assemble code block"))
    (dotimes (x (length insts))
      (if (run-cycle tcpu)
	  (error "interrupt thrown at line ~a" x)))
    (dolist (x reg-results)
      (if (equal (register tcpu (car x)) (cadr x))
	  (format t "passed: r~a = ~a~%"
		  (car x)
		  (register tcpu (car x)))
	  (progn
	    (format t "failed: r~a = ~a, should be ~a~%"
		    (car x)
		    (register tcpu (car x))
		    (cadr x))
	    (setf retval nil))))
    (list retval tcpu)))
