(in-package :xvm)

(defstruct cpu
  (registers (make-array 16
			 :element-type 'u32int
			 :initial-element 0
			 :adjustable nil)
	     :type (simple-array u32int))
  (scratch-reg (make-array 16
			   :element-type 'u32int
			   :initial-element 0
			   :adjustable nil)
	       :type (simple-array u32int))
  (do-interrupt nil :type boolean)
  (interrupt-number 0 :type u8int)
  (next-pc 0 :type u32int) ; this is unneeded. holdover from C design docs
  (halted t :type boolean)
  (cycles 0 :type fixnum))

(defun register (c n)
  (with-accessors ((r cpu-registers)) c
    (aref r n)))

(defun scratch-register (c n)
  (with-accessors ((r cpu-scratch-reg)) c
    (aref r n)))

(defun (setf register) (val c n)
  (with-accessors ((r cpu-registers)) c
    (setf (aref r n) (u32int val))))

(defun (setf scratch-register) (val c n)
  (with-accessors ((r cpu-scratch-reg)) c
    (setf (aref r n) (u32int val))))

(defun flag (c n)
  (ldb (byte 1 n) (register c 15)))

(defmacro flagp (c n)
  `(equal (flag ,c ,n) 1))

(defun (setf flag) (val c n)
  (let ((tmp (register c 15)))
    (setf (ldb (byte 1 n) tmp) val
	  (register c 15) tmp)))

(defun interrupt-number (c)
  (ldb (byte 4 4) (register c 15)))

(defun (setf interrupt-number) (val c)
  (let ((tmp (register c 15)))
    (setf (ldb (byte 4 4) tmp) val
	  (register c 15) tmp)))

;;; define some names for all of the registers
(iota (R0 R1 R2 R3 R4 R5 R6 R7) 0)
(iota (C0 C1 C2 C3 C4 C5 C6 C7) 8)
(iota (OP CP PC SP FP SV FV FL) 8)

(defun save-registers (c)
  (dolist (x (range-list 0 15))
    (setf (scratch-register c x) (register c x))))

(defun restore-registers (c)
  (dolist (x (range-list 0 15))
    (setf (register c x) (scratch-register c x))))

(defun interrupt (c int)
  (if (not (cpu-do-interrupt c))
      (progn
	(setf (cpu-do-interrupt c) t
	      (cpu-interrupt-number c) int)
	t)
      nil))

(defun sync-interrupts (c)
  (with-accessors ((int-status cpu-do-interrupt)
		   (int-num cpu-interrupt-number)) c
    (if (and int-status (flagp c 2))
	(setf (flag c 8) 1
	      (interrupt-number c) (u4int int-num)
	      int-status nil
	      int-num 0))))

(defun stack-check (c)
  (and (memcheck32 (register c sp) :read)
       (memcheck32 (register c sp) :write)))

(defun stack-check-range (c n &optional (op :push))
  (let ((rval t))
    (dotimes (x n)
      (when rval
	(if (equal op :pop)
	    (setf rval (and (memcheck32 (- (register c sp) (* 4 x)) :read)
			    (memcheck32 (- (register c sp) (* 4 x)) :write)))
	    (setf rval (and (memcheck32 (+ (register c sp) (* 4 x)) :read)
			    (memcheck32 (+ (register c sp) (* 4 x)) :write))))))
    rval))

(defun stack-push (c val)
  (setf (register c sp) (u32int (+ (register c sp) 4)))
  (memwrite32 (register c sp) (u32int val)))

(defun stack-pop (c)
  (setf (register c sp) (u32int (- (register c sp) 4)))
  (u32int (memread32 (+ (register c sp) 4))))
