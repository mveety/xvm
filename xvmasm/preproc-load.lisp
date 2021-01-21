(in-package :xvmasm)

(defun preproc-load (args)
  (if *allow-plugins*
      (dolist (x args)
	(let ((clean-fname (subseq x 1 (- (length x) 1))))
	  (dformat t "preproc: loading lisp file ~a~%" clean-fname)
	  (if (probe-file clean-fname)
	      (load clean-fname :verbose nil :print nil)
	      (error "file not found: ~a" clean-fname))))
      (if *error-on-plugin-load*
	  (error "plugin loading is disabled!"))))

(eval-when (:load-toplevel :execute)
  (add-preproc-plugin "load" #'preproc-load 1))
