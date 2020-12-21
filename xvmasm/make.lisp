(in-package :cl-user)

(load "load-xvmasm.lisp")

(defun contains-string (str lst)
  (if (equal lst nil)
      nil
      (if (string-equal (car lst) str)
	  t
	  (contains-string str (cdr lst)))))

(defun print-block (stream &rest strings)
  (dolist (x strings)
    (format stream "~a~%" x)))

(if (contains-string "help" (unix-options:cli-options))
    (progn
      (print-block t "usage: make.lisp [options...]"
		   "  note: prefix an option with 'no-' to disable it"
		   "      compile    -- compile sources before loading"
		   "      verbose    -- verbose build (for load, compile)"
		   "      print      -- print build (for load, compile)"
		   "      plugins    -- enable plugin support"
		   "      list-files -- print file name when built or loaded"
		   " defaults: compile no-list-files no-print no-verbose plugins")
      #+sbcl (sb-ext:exit)
      #+ccl (ccl:quit)
      #+clisp (ext:exit)
      #-(or sbcl ccl clisp) (error "unsupported lisp")))

(let* ((opts (unix-options:cli-options))
       (build-compile (not (contains-string "no-compile" opts)))
       (build-list-files (contains-string "list-files" opts))
       (build-print (contains-string "print" opts))
       (build-verbose (contains-string "verbose" opts))
       (build-plugins (not (contains-string "no-plugins" opts)))
       (build-options nil))
  (setf build-options
	(list (if build-compile "compile" "no-compile")
	      (if build-verbose "verbose" "no-verbose")
	      (if build-print "print" "no-print")
	      (if build-list-files "list-files" "no-list-files")
	      (if build-plugins "plugins" "no-plugins")))
  (format t "building with options: ")
  (dolist (x build-options)
    (format t "~a " x))
  (format t "~%")
  (make-xvmasm :compile build-compile
	       :verbose build-verbose
	       :print build-print
	       :list-files build-list-files
	       :plugins build-plugins
	       :build-options build-options))
