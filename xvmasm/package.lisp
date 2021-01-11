(in-package :cl-user)

(defpackage :xvmasm
  (:use :cl)
  (:export #:assemble
	   #:assemble-strings
	   #:xvmasm-main
	   #:emit-exec-file
	   #:+revision+
	   #:*build-options*
	   #:append-to-list
	   #:add-preproc-plugin
	   ))
