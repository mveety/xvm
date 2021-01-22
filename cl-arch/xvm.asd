;;; -*- Syntax: Common-Lisp -*-

(in-package :cl-user)

(asdf:defsystem xvm
  :author "Matthew Veety <mveety@gmail.com>"
  :description "xvm platform emulator"
  :license "BSD"
  :version "1.0"
  :depends-on (:bordeaux-threads :chanl :xvmasm)
  :serial t
  :components ((:file "package")
	       ;; the core of the implementation
	       (:file "types")      ; types used in the emulator
	       (:file "util")       ; utility functions
	       (:file "config")     ; configuration
	       (:file "insts")      ; microcode
	       (:file "memory")     ; core memory interface
	       (:file "memimage")   ; reading/writing memory images
	       (:file "sysman")     ; part 1 of the system manager
	       (:file "block-ram")  ; hack to get around array size limits
	       (:file "ram")        ; system ram
	       (:file "cpu")        ; core cpu interface
	       (:file "fetch")      ; instruction fetch
	       (:file "decode")     ; instruction decode
	       (:file "exec")       ; instruction execution
	       (:file "devices")    ; core device interface
	       (:file "test-utils") ; repl-based testing util functions
	       ;; instruction implementations
	       (:file "insts-data")
	       (:file "insts-byte")
	       (:file "insts-math")
	       (:file "insts-logic")
	       (:file "insts-comp")
	       (:file "insts-branch")
	       (:file "insts-control")
	       (:file "insts-devices")
	       ;; device implementations
	       (:file "device-console")
	       (:file "device-bootcons")
	       ))
