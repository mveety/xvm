(in-package :xvm.test)

(defun test-get-microinst (name)
  (let ((insts *inst-list*)
	(rval nil))
    (dolist (x insts)
      (unless rval
	(if (equal name (getf x :name))
	    (setf rval x))))
    rval))
