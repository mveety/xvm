(in-package :xvm)

(defun ldcommon (cpu width mode srcaddr destreg)
  (block nil
    (let* ((reg destreg)
	   (const srcaddr)
	   (rmode (cond
		    ((equal mode 1) 'absolute)
		    ((equal mode 2) 'relative)
		    (t (if (flagp cpu 1)
			   'relative
			   'absolute))))
	   (offset (register cpu op))
	   (raddr (if (eq rmode 'relative)
		      (+ const offset)
		      const)))
      (case width
	((4 nil) (if (memcheck32 raddr :read)
		     (setf (register cpu reg) (memread32 raddr))
		     (cpuerror 0)))
	(2 (if (memcheck16 raddr :read)
	       (setf (register cpu reg) (memread16 raddr))
	       (cpuerror 0)))
	(1 (if (memcheck raddr :read)
	       (setf (register cpu reg) (memread raddr))
	       (cpuerror 0)))))
    nil))

(definstruction ld (:args 2 :cycles 1 :use-width t :use-mode t :nil-return nil)
  (ldcommon cpu width mode (cadr args) (car args)))

(definstruction ldr (:args 2 :cycles 1 :use-width t :use-mode t :nil-return nil)
  (ldcommon cpu width mode (register cpu (cadr args)) (car args)))

(definstruction ldc (:args 2 :cycles 1 :use-width t :use-mode nil)
  (let ((arg2 (cadr args)))
    (case width
      ((4 nil) (setf (register cpu (car args)) (u32int arg2)))
      (2 (setf (register cpu (car args)) (u16int arg2)))
      (1 (setf (register cpu (car args)) (u8int arg2))))))

(definstruction ldi (:args 2 :cycles 1 :use-width t :use-mode t :nil-return nil)
  (let* ((iaddr 0)
	 (rmode (cond
		  ((equal mode 1) 'absolute)
		  ((equal mode 2) 'relative)
		  (t (if (flagp cpu 1)
			 'relative
			 'absolute))))
	 (offset (register cpu op))
	 (arg2 (cadr args))
	 (raddr (if (eq rmode 'relative)
		    (+ arg2 offset)
		    arg2)))
    (if (memcheck32 raddr :read)
	(setf iaddr (memread32 raddr))
	(cpuerror 0))
    (ldcommon cpu width mode iaddr (car args))))

(defun stocommon (cpu width mode srcreg destaddr)
  (block nil
    (let* ((reg srcreg)
	   (const destaddr)
	   (rmode (cond
		    ((equal mode 1) 'absolute)
		    ((equal mode 2) 'relative)
		    (t (if (flagp cpu 1)
			   'relative
			   'absolute))))
	   (offset (register cpu op))
	   (raddr (if (equal rmode 'relative)
		      (+ const offset)
		      const)))
      (case width
	((4 nil) (if (memcheck32 raddr :write)
		     (memwrite32 raddr (u32int (register cpu reg)))
		     (cpuerror 0)))
	(2 (if (memcheck16 raddr :write)
	       (memwrite16 raddr (u16int (register cpu reg)))
	       (cpuerror 0)))
	(1 (if (memcheck raddr :write)
	       (memwrite raddr (u8int (register cpu reg)))
	       (cpuerror 0))))
      nil)))

(definstruction sto (:args 2 :cycles 1 :use-width t :use-mode t :nil-return nil)
  (stocommon cpu width mode (cadr args) (car args)))

(definstruction stor (:args 2 :cycles 1 :use-width t :use-mode t :nil-return nil)
  (stocommon cpu width mode (cadr args) (register cpu (car args))))

(definstruction stoi (:args 2 :cycles 1 :use-width t :use-mode t :nil-return nil)
  (let* ((iaddr 0)
	 (rmode (cond
		  ((equal mode 1) 'absolute)
		  ((equal mode 2) 'relative)
		  (t (if (flagp cpu 1)
			 'relative
			 'absolute))))
	 (offset (register cpu op))
	 (arg1 (car args))
	 (raddr (if (eq rmode 'relative)
		    (+ arg1 offset)
		    arg1)))
    (if (memcheck32 raddr :read)
	(setf iaddr (memread32 raddr))
	(cpuerror 0))
    (stocommon cpu width mode (cadr args) iaddr)))

(definstruction mov (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (setf (register cpu (car args)) (register cpu (cadr args)))
  nil)

(definstruction swap (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((reg1data (register cpu (car args))))
    (setf (register cpu (car args)) (register cpu (cadr args))
	  (register cpu (cadr args)) reg1data))
  nil)

#|
problems with stack implementing stack operations:
stack-check only looks to see if the current sp is readable (fine for pop),
but not the next sp. when pushing be sure to use stack-check-range with a
value of one. really this should be fixed, but i'm working around it now.
I'll probably regret this choice later on.
|#

(definstruction push (:args 1 :cycles 1 :use-width nil :use-mode nil)
  (if (stack-check-range cpu 1 :push)
      (stack-push cpu (register cpu (car args)))
      (cpuerror 0))
  nil)

(definstruction pop (:args 1 :cycles 1 :use-width nil :use-mode nil)
  (if (stack-check cpu)
      (setf (register cpu (car args)) (stack-pop cpu))
      (cpuerror 0))
  nil)

(definstruction popc (:args 1 :cycles 1 :use-width nil :use-mode nil)
  (let ((arg1 (car args)))
    (if (stack-check-range cpu arg1 :pop)
	(setf (register cpu sp) (- (register cpu sp) (* 4 arg1)))
	(cpuerror 0))
    nil))

(defvar asaved-registers (list r0 r1 r2 r3 r4 r5 r6 r7 op fp))

(definstruction pusha (:args 0 :cycles 1)
  (if (stack-check-range cpu (length asaved-registers) :push)
      (dolist (x asaved-registers)
	(stack-push cpu (register cpu x)))
      (cpuerror 0)))

(definstruction popa (:args 0 :cycles 1)
  (if (stack-check-range cpu (length asaved-registers) :pop)
      (dolist (x asaved-registers)
	(setf (register cpu x) (stack-pop cpu)))
      (cpuerror 0)))
