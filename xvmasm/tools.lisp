(in-package :xvmasm)

(defvar *old-debugger-hook* nil)
(defvar debugger-modified nil)
(defvar debugger-disabled nil)
(defparameter *debugging* nil)

(defmacro dformat (&rest args)
  `(if *debugging*
       (format ,@args)))

(defun debug-ignore (c h)
  (declare (ignore h))
  (format t "~a:~a error: ~a~%" *curfile* *curline* c)
  #+sbcl (sb-ext:exit)
  #+ccl (ccl:quit)
  #+clisp (ext:exit)
  #-(or sbcl ccl) (abort))

(defun quit-program ()
  #+sbcl (sb-ext:exit)
  #+ccl (ccl:quit)
  #+clisp (ext:exit)
  #-(or sbcl ccl clisp) (error "unable to exit: unsupported lisp"))

(defun disable-debugger ()
  #+(or sbcl ccl clisp) (progn
			  (if (not debugger-modified)
			      (setf *old-debugger-hook* *debugger-hook*))
			  (if (not debugger-disabled)
			      (setf *debugger-hook* #'debug-ignore)))
  #-(or sbcl ccl clisp) (error "unable to disable debugger: unsupported lisp")
  nil)

(defun enable-debugger ()
  #+(or sbcl ccl clisp) (progn
			  (if (and debugger-modified
				   debugger-disabled)
			      (setf *debugger-hook* *old-debugger-hook*)
			      (progn
				(if (not debugger-modified)
				    (error "old debugger not saved"))
				(if (not debugger-disabled)
				    (error "debugger is not disabled")))))
  #-(or sbcl ccl clisp) (error "unable to enable debugger: unsupported lisp")
  nil)
