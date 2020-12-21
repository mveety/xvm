(in-package :xvmasm)

(defparameter *input-file* nil)
(defparameter *output-file* nil)
(defparameter *print-version* nil)
(defparameter *print-usage* nil)
(defparameter *print-bytestream* nil)
#-(or os-windows windows-target win32) (defparameter *execfile-name* "xvmasm")
#+(or os-windows windows-target win32) (defparameter *execfile-name* "xvmasm.exe")

(defun fname-asm-to-bin (string)
  (if (<= (length string) 4)
      (error "invalid file name: ~a" string))
  (let* ((basename (subseq string 0 (- (length string) 4)))
	 (newname nil)
	 (tmp (coerce basename 'list)))
    (setf tmp (append tmp (coerce ".bin" 'list))
	  newname (coerce tmp 'string))
    newname))

(defun handle-options (argv)
  (let* ((curopt nil)
	 (rest-argv nil)
	 (endloop nil))
    (setf curopt (car argv)
	  rest-argv (cdr argv))
    (loop until (or endloop (equal curopt nil)) do
	 (if (string-equal (subseq curopt 0 1) "-")
	     (let ((tmp (subseq curopt 1)))
	       (cond
		 ((string-equal tmp "v") (setf *print-version* t
					       endloop t))
		 ((string-equal tmp "d") (setf *debugging* t))
		 ((string-equal tmp "b") (setf *print-bytestream* t))
		 ((string-equal tmp "f") (setf *print-files* t))
		 ((string-equal tmp "s") (setf *print-stats* t))
		 ((string-equal tmp "n") (setf *allow-plugins* nil))
		 ((string-equal tmp "e") (setf *error-on-plugin-load* t
					       *error-on-warning* t))
		 ((string-equal tmp "o")
		  (setf curopt (car rest-argv)
			rest-argv (cdr rest-argv))
		  (if (equal curopt nil)
		      (error "invalid argument"))
		  (if (string-equal (subseq curopt 0 1) "-")
		      (error "invalid argument"))
		  (setf *output-file* curopt))
		 ((string-equal tmp "p")
		  (setf curopt (car rest-argv)
			rest-argv (cdr rest-argv))
		  (if (probe-file curopt)
		      (if *allow-plugins*
			  (progn
			    (dformat t "loading plugin ~a~%" curopt)
			    (load curopt :verbose nil :print nil))
			  (if *error-on-plugin-load*
			      (error "plugin loading is disabled!")))
		      (error "file ~a not found" curopt)))
		 ((string-equal tmp "h") (setf *print-usage* t
					       endloop t))))
	     (setf *input-file* curopt
		   endloop t))
	 (setf curopt (car rest-argv)
	       rest-argv (cdr rest-argv)))))

(defun print-usage (&optional (exit-program nil))
  (format t "usage: xvmasm [-h|-v] [-e] [-n|-p file] [-f] [-b] [-s] [-o outfile] infile~%")
  (print-block t
	       "    h        -- show this output"
	       "    v        -- show version"
	       "    f        -- print processed file names"
	       "    b        -- print output bytestream"
	       "    s        -- print statistics"
	       "    p [file] -- load a plugin"
	       "    n        -- disable plugin loading"
	       "    e        -- error on warning"
	       "    o [file] -- output file"
	       "    [file]   -- input file")
  (if exit-program
      (quit-program)))

(defun print-version (&optional (exit-program nil))
  (format t "xvmasm revision ~a~%" +revision+)
  (format t "xvm microcode revision ~a~%" +microcode-revision+)
  (if (not (equal *build-options* nil))
      (progn
	(format t "build options: ")
	(dolist (x *build-options*)
	  (format t "~a " x))
	(format t "~%")))
  (if (and (not (equal *preprocessor-plugins* nil))
	   *allow-plugins*)
      (progn
	(format t "plugins:~%")
	(dolist (x *preprocessor-plugins*)
	  (format t "    ~a revision ~a~%" (car x) (caddr x)))))
  (if exit-program
      (quit-program)))

(defun xvmasm-main ()
  (let ((*datastream* nil)
	(*total-lines* 0)
	(*total-instructions* 0)
	(*total-bytes* 0)
	(*total-files* 0))
    (disable-debugger)
    (handle-options (unix-options:cli-options))
    (if *print-version* (print-version t))
    (if *print-usage* (print-usage t))
    (if (equal *input-file* nil)
	(error "no input file given"))
    (dformat t "loaded plugins: ")
    (list-loaded-plugins :newline t)
    (if (and (equal *output-file* nil)
	     (not *print-bytestream*))
	(setf *output-file* (fname-asm-to-bin *input-file*)))
    (setf *datastream* (assemble *input-file* *output-file*))
    (if *print-bytestream*
	(format t "~a~%" *datastream*))
    (if (or *debugging* *print-stats*)
	(progn
	  (format t "stats:~%")
	  (format t "    files processed: ~a~%" *total-files*)
	  (format t "    lines processed: ~a~%" *total-lines*)
	  (format t "    instructions processed: ~a~%" *total-instructions*)
	  (format t "    output size: ~a~%" *total-bytes*)))
    nil))

(defun emit-exec-file (&optional (fname *execfile-name*) (build-options nil))
  (setf *build-options* build-options)
  (if (equal fname nil) (setf fname *execfile-name*))
  #+sbcl (sb-ext:save-lisp-and-die fname
				   :executable t
				   :toplevel #'xvmasm:xvmasm-main)
  #+ccl (ccl:save-application fname
			      :toplevel-function #'xvmasm:xvmasm-main
			      :prepend-kernel t)
  #+clisp (ext:saveinitmem fname
			   :init-function #'xvmasm:xvmasm-main
			   :executable t
			   :norc t)
  #-(or sbcl ccl clisp) (error "unable to build binary: unsupported lisp"))
