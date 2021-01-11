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
    (print datastream)
    (load-list-to-memory datastream start-address)))

(defun asm (start-address &rest strings)
  (assemble start-address strings))
