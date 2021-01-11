(in-package :xvm)

(declaim (inline overflow-check))
(defun overflow-check (cpu n)
  (if (typep n 'u32int)
      (setf (flag cpu 9) 0)
      (setf (flag cpu 9) 1)))

(definstruction add (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((a (register cpu (car args)))
	(b (register cpu (cadr args)))
	(tmp 0))
    (setf tmp (+ a b)
	  (register cpu (car args)) (u32int tmp))
    (overflow-check cpu tmp)))

(definstruction sub (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((a (register cpu (car args)))
	(b (register cpu (cadr args)))
	(tmp 0))
    (setf tmp (- a b)
	  (register cpu (car args)) (u32int tmp))
    (if (< a b)
	(setf (flag cpu 9) 1)
	(overflow-check cpu tmp))))

(definstruction mul (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((a (register cpu (car args)))
	(b (register cpu (cadr args)))
	(tmp 0))
    (setf tmp (* a b)
	  (register cpu (car args)) (u32int tmp))
    (overflow-check cpu tmp)))

(definstruction div (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((a (register cpu (car args)))
	(b (register cpu (cadr args)))
	(tmp 0))
    (if (equal b 0)
	(progn
	  (setf (register cpu (car args)) 0
		(flag cpu 10) 1))
	(progn
	  (setf tmp (values (floor a b))
		(register cpu (car args)) (u32int tmp)
		(flag cpu 10) 0)
	  (overflow-check cpu tmp)))))

(definstruction mod (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((a (register cpu (car args)))
	(b (register cpu (cadr args)))
	(tmp 0))
    (if (equal b 0)
	(progn
	  (setf (register cpu (car args)) 0
		(flag cpu 10) 1))
	(progn
	  (setf tmp (mod a b)
		(register cpu (car args)) (u32int tmp)
		(flag cpu 10) 0)
	  (overflow-check cpu tmp)))))

(definstruction inc (:args 1 :cycles 1 :use-width nil :use-mode nil)
  (let ((a (register cpu (car args)))
	(tmp 0))
    (setf tmp (+ a 1)
	  (register cpu (car args)) (u32int tmp))
    (overflow-check cpu tmp)))

(definstruction dec (:args 1 :cycles 1 :use-width nil :use-mode nil)
  (let ((a (register cpu (car args)))
	(tmp 0))
    (if (eq a 0)
	(setf (flag cpu 9) 1
	      (register cpu (car args)) (u32int -1))
	(progn
	  (setf tmp (- a 1)
		(register cpu (car args)) (u32int tmp))
	  (overflow-check cpu tmp)))))
