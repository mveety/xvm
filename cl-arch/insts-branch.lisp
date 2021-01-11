(in-package :xvm)

(defun process-addr (cpu mode addr)
  (cond
    ((equal mode 1) addr)
    ((equal mode 2) (+ addr (register cpu op)))
    (t (if (flagp cpu 1)
	   (+ addr (register cpu op))
	   addr))))

(definstruction jmp (:args 1 :cycles 1 :writepc nil :use-mode t)
  (setf (register cpu pc) (process-addr cpu mode (car args))))

(definstruction jmpr (:args 1 :cycles 1 :writepc nil :use-mode t)
  (setf (register cpu pc) (process-addr cpu mode (register cpu (car args)))))

(definstruction jz (:args 2 :cycles 1 :writepc nil :use-mode t :use-len t)
  (if (zerop (register cpu (car args)))
      (setf (register cpu pc) (process-addr cpu mode (cadr args)))
      (incf (register cpu pc) len)))

(definstruction jzr (:args 2 :cycles 1 :writepc nil :use-mode t :use-len t)
  (if (zerop (register cpu (car args)))
      (setf (register cpu pc) (process-addr cpu mode (register cpu (cadr args))))
      (incf (register cpu pc) len)))

(definstruction jnz (:args 2 :cycles 1 :writepc nil :use-mode t :use-len t)
  (if (not (zerop (register cpu (car args))))
      (setf (register cpu pc) (process-addr cpu mode (cadr args)))
      (incf (register cpu pc) len)))

(definstruction jnzr (:args 2 :cycles 1 :writepc nil :use-mode t :use-len t)
  (if (not (zerop (register cpu (car args))))
      (setf (register cpu pc) (process-addr cpu mode (register cpu (cadr args))))
      (incf (register cpu pc) len)))

(definstruction br (:args 1 :cycles 1 :writepc nil :use-mode t :use-len t)
  (if (flagp cpu 3)
      (setf (register cpu pc) (process-addr cpu mode (car args)))
      (incf (register cpu pc) len)))

(definstruction brr (:args 1 :cycles 1 :writepc nil :use-mode t :use-len t)
  (if (flagp cpu 3)
      (setf (register cpu pc) (process-addr cpu mode (register cpu (car args))))
      (incf (register cpu pc) len)))

(definstruction call (:args 1 :cycles 1 :writepc nil :use-mode t :use-len t)
  (let ((raddr (process-addr cpu mode (car args))))
    (if (stack-check-range cpu 1 :push)
	(stack-push cpu (+ (register cpu pc) len))
	(cpuerror 0))
    (setf (register cpu pc) raddr)))

(definstruction callr (:args 1 :cycles 1 :writepc nil :use-mode t :use-len t)
  (let ((raddr (process-addr cpu mode (register cpu (car args)))))
    (if (stack-check-range cpu 1 :push)
	(stack-push cpu (+ (register cpu pc) len))
	(cpuerror 0))
    (setf (register cpu pc) raddr)))

(definstruction ret (:args 0 :cycles 1 :writepc nil)
  (setf (register cpu pc) (stack-pop cpu)))

(definstruction syscall (:args 0 :cycles 1 :writepc nil :use-len t)
  (if (not (stack-check-range cpu 2 :push))
      (cpuerror 0))
  (stack-push cpu (+ (register cpu pc) len))
  (stack-push cpu (register cpu op))
  (stack-push cpu (register cpu fl))
  (setf (flag cpu 0) 0
	(flag cpu 1) 0
	(register cpu pc) (register cpu sv)))

(defun sys-iret-common (cpu)
  (setf (register cpu fl) (stack-pop cpu)
	(register cpu op) (stack-pop cpu)
	(register cpu pc) (stack-pop cpu)))

(definstruction sysret (:args 0 :cycles 1 :writepc nil)
  (sys-iret-common cpu))

(definstruction iret (:args 0 :cycles 1 :writepc nil)
  (sys-iret-common cpu))

(definstruction int (:args 1 :cycles 1)
  (let ((intn (car args)))
    (if (equal intn nil)
	nil
	(if (flagp cpu 2)
	    (setf (cpu-do-interrupt cpu) t
		  (cpu-interrupt-number cpu) intn)
	    nil))))
