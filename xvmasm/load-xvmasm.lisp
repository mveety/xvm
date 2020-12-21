(in-package :cl-user)

(ql:quickload '(unix-options split-sequence))
(load "package.lisp")

#+sbcl (defparameter *fasl-ext* "fasl")
#+ccl (defparameter *fasl-ext* "wx64fsl")
#+(and x86 ccl) (defparameter *fasl-ext* "wx32fsl")
#+(and x86-64 ccl) (defparameter *fasl-ext* "wx64fsl")
#+clisp (defparameter *fasl-ext* "fas")
#-(or sbcl ccl) (error "unsuppored lisp!")

(defparameter *source-files* '("package.lisp"
			       "state.lisp"
			       "tools.lisp"
			       "util.lisp"
			       "insts.lisp"
			       "regs.lisp"
			       "tokenizer.lisp"
			       "preproc.lisp"
			       "lexparse.lisp"
			       "inststream.lisp"
			       "stage2.lisp"
			       "codegen.lisp"
			       "readfile.lisp"
			       "writefile.lisp"
			       "assemble.lisp"
			       "main.lisp"
			       "testinsts.lisp"))

(defparameter *plugins* '("preproc-include.lisp"
			  "preproc-load.lisp"
			  "preproc-repeat.lisp"))

(defun fname-lisp-to-fasl (string)
  (if (<= (length string) 4)
      (error "invalid file name: ~a" string))
  (let* ((basename (subseq string 0 (- (length string) 4)))
	 (newname nil)
	 (tmp (coerce basename 'list)))
    (setf tmp (append tmp (coerce *fasl-ext* 'list))
	  newname (coerce tmp 'string))
    newname))

(defun compile-xvmasm (&key (print nil) (verbose nil) (quiet nil)
			 (list-files t))
  (dolist (x *source-files*)
    (let ((faslname (fname-lisp-to-fasl x)))
      (if (equal (probe-file faslname) nil)
	  (progn
	    (if (and (not print) (not quiet))
		(if list-files (format t "compiling ~a to ~a~%" x faslname)))
	    (compile-file x :print print :verbose verbose))))))

(defun load-xvmasm-source (&key (print nil) (verbose nil) (quiet nil)
			     (list-files t))
  (dolist (x *source-files*)
    (if (and (not print) (not quiet))
	(if list-files (format t "loading ~a~%" x)))
    (load x :print print :verbose verbose)))

(defun load-xvmasm-compiled (&key (print nil) (verbose nil) (quiet nil)
			       (list-files t))
  (compile-xvmasm :print print :verbose verbose :quiet quiet :list-files list-files)
  (dolist (x *source-files*)
    (if (and (not print) (not quiet))
	(if list-files (format t "loading ~a~%" (fname-lisp-to-fasl x))))
    (load (fname-lisp-to-fasl x) :print print :verbose verbose)))

(defun load-xvmasm (&key (compile t) (verbose nil) (print nil) (plugins t)
		      (list-files t))
  (if compile
      (load-xvmasm-compiled :verbose verbose :print print :list-files list-files)
      (load-xvmasm-source :verbose verbose :print print :list-files list-files))
  (if plugins
      (dolist (x *plugins*)
	(if (not print)
	    (if list-files (format t "loading plugin ~a~%" x)))
	(load x :print print :verbose verbose)))
  (format t "evaluate xvmasm:emit-exec-file to generate an executable~%"))

(defun make-xvmasm (&key (compile t) (verbose nil) (print nil)
		      (fname nil) (plugins t) (list-files t)
		      (build-options nil))
  (load-xvmasm :compile compile :verbose verbose :print print :plugins plugins
	       :list-files list-files)
  (xvmasm:emit-exec-file fname build-options))

(export '(compile-xvmasm load-xvmasm make-xvmasm))
(format t "run load-xvmasm to load the assembler~%")
(format t "run make-xvmasm to build the assembler~%")
