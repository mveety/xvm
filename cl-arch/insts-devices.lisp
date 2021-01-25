(in-package :xvm)

(defun incommon (cpu width dev reg)
  (let* ((devtmp (devread dev)))
    (case width
      ((4 nil) (setf (register cpu reg) (u32int devtmp)))
      (2 (setf (register cpu reg) (u16int devtmp)))
      (1 (setf (register cpu reg) (u8int devtmp))))))

(defun outcommon (cpu width dev reg)
  (case width
    ((4 nil) (devwrite dev (u32int (register cpu reg))))
    (2 (devwrite dev (u16int (register cpu reg))))
    (1 (devwrite dev (u8int (register cpu reg))))))

(definstruction in (:args 2 :cycles 1 :use-width t)
  (incommon cpu width (cadr args) (car args)))

(definstruction inr (:args 2 :cycles 1 :use-width t)
  (let ((devaddr (register cpu (cadr args))))
    (incommon cpu width devaddr (car args))))

(definstruction out (:args 2 :cycles 1 :use-width t)
  (outcommon cpu width (car args) (cadr args)))

(definstruction outr (:args 2 :cycles 1 :use-width t)
  (let ((devaddr (register cpu (car args))))
    (outcommon cpu width devaddr (cadr args))))

(definstruction dint (:args 1 :cycles 1 :cpu nil)
  (devinterrupt (car args)))

(definstruction dintr (:args 1 :cycles 1 :cpu t)
  (devinterrupt (register cpu (car args))))
