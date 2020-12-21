(in-package :xvm)

(definstruction eq (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (if (equal (register cpu (car args)) (register cpu (cadr args)))
      (setf (flag cpu 3) 1)
      (setf (flag cpu 3) 0)))

(definstruction neq (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (if (not (equal (register cpu (car args)) (register cpu (cadr args))))
      (setf (flag cpu 3) 1)
      (setf (flag cpu 3) 0)))

(definstruction lt (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (if (< (register cpu (car args)) (register cpu (cadr args)))
      (setf (flag cpu 3) 1)
      (setf (flag cpu 3) 0)))

(definstruction gt (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (if (> (register cpu (car args)) (register cpu (cadr args)))
      (setf (flag cpu 3) 1)
      (setf (flag cpu 3) 0)))

(definstruction nz (:args 1 :cycles 1 :use-width nil :use-mode nil)
  (if (not (equal (register cpu (car args)) 0))
      (setf (flag cpu 3) 1)
      (setf (flag cpu 3) 0)))
