;;; -*- Syntax: Common-Lisp -*-

(in-package :cl-user)

(asdf:defsystem xvmasm
  :author "Matthew Veety <mveety@gmail.com>"
  :description "simple assembler for xvm"
  :license "BSD"
  :version "7.0"
  :build-operation asdf:program-op
  :build-pathname "xvmasm"
  :entry-point "xvmasm:xvmasm-main"
  :depends-on (:unix-options :split-sequence)
  :serial t
  :components ((:file "package")
	       (:file "state")
	       (:file "tools")
	       (:file "util")
	       (:file "insts")
	       (:file "regs")
	       (:file "tokenizer")
	       (:file "preproc")
	       (:file "lexparse")
	       (:file "inststream")
	       (:file "stage2")
	       (:file "codegen")
	       (:file "readfile")
	       (:file "writefile")
	       (:file "assemble")
	       (:file "main")
	       (:file "testinsts")
	       (:file "preproc-include")
	       (:file "preproc-load")
	       (:file "preproc-repeat")))
