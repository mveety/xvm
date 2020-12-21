(in-package :xvm)

;;; no-op is handled in exec.lisp

(definstruction sint (:args 0 :cycles 1 :use-width nil :use-mode nil)
  (setf (flag cpu 2) 1))

(definstruction cint (:args 0 :cycles 1 :use-width nil :use-mode nil)
  (setf (flag cpu 2) 0))

(definstruction halt (:args 0 :cycles 1 :use-width nil :use-mode nil)
  (setf (cpu-halted cpu) t))
