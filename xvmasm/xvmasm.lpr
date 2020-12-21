;; -*- lisp-version: "10.1 [32-bit Windows] (Mar 7, 2020 0:12)"; -*-

(in-package :cg-user)

(defpackage :XVMASM
  (:export #:xvmasm-main))

(define-project :name :xvmasm
  :modules (list (make-instance 'module :name "assemble.lisp")
                 (make-instance 'module :name "codegen.lisp")
                 (make-instance 'module :name "insts.lisp")
                 (make-instance 'module :name "inststream.lisp")
                 (make-instance 'module :name "lexparse.lisp")
                 (make-instance 'module :name "main.lisp")
                 (make-instance 'module :name "package.lisp")
                 (make-instance 'module :name "preproc.lisp")
                 (make-instance 'module :name "preproc-include.lisp")
                 (make-instance 'module :name "preproc-load.lisp")
                 (make-instance 'module :name "preproc-repeat.lisp")
                 (make-instance 'module :name "readfile.lisp")
                 (make-instance 'module :name "regs.lisp")
                 (make-instance 'module :name "stage2.lisp")
                 (make-instance 'module :name "state.lisp")
                 (make-instance 'module :name "testinsts.lisp")
                 (make-instance 'module :name "testplugin.lisp")
                 (make-instance 'module :name "tokenizer.lisp")
                 (make-instance 'module :name "tools.lisp")
                 (make-instance 'module :name "util.lisp")
                 (make-instance 'module :name "writefile.lisp"))
  :projects nil
  :libraries nil
  :editable-files nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :xvmasm
  :main-form nil
  :compilation-unit t
  :concatenate-project-fasls nil
  :verbose nil
  :runtime-modules nil
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :top-level :debugger)
  :build-flags (list :allow-runtime-debug :runtime-bundle)
  :autoload-warning nil
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+M +t \"Console for Debugging\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :build-number 0
  :run-with-console t
  :project-file-version-info nil
  :on-initialization 'xvmasm:xvmasm-main
  :default-error-handler-for-delivery 'report-unexpected-error-and-exit
  :on-restart 'do-default-restart)

;; End of Project Definition
