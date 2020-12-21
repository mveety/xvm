(in-package :xvm)

#|
problems:
bsetc, bclrc, and btestc emit style-warnings because (cadr args) might
possibly be nil. this is clearly false (the args can only ever be fixnums
and this is checked before the functions are called. I need to work out
a way to make these type errors shut up.
I added some if wrappers to check to see if (cadr args) is nil, but this
will have performance implications. it would be better to just tell the
compiler to shut up.
|#

(definstruction bset (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((reg (register cpu (car args)))
	(bit (register cpu (cadr args))))
    (setf (ldb (byte 1 bit) reg) 1
	  (register cpu (car args)) reg)))

(definstruction bsetc (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((reg (register cpu (car args)))
	(bit (cadr args)))
    (if (equal bit nil)
	(cpuerror 0)
	(setf (ldb (byte 1 bit) reg) 1
	      (register cpu (car args)) reg))))

(definstruction bclr (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((reg (register cpu (car args)))
	(bit (register cpu (cadr args))))
    (setf (ldb (byte 1 bit) reg) 0
	  (register cpu (car args)) reg)))

(definstruction bclrc (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((reg (register cpu (car args)))
	(bit (cadr args)))
    (if (equal bit nil)
	(cpuerror 0)
	(setf (ldb (byte 1 bit) reg) 0
	      (register cpu (car args)) reg))))

(definstruction btest (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((reg (register cpu (car args)))
	(bit (register cpu (cadr args))))
    (if (logbitp bit reg)
	(setf (flag cpu 3) 1)
	(setf (flag cpu 3) 0))))

(definstruction btestc (:args 2 :cycles 1 :use-width nil :use-mode nil)
  (let ((reg (register cpu (car args)))
	(bit (cadr args)))
    (if (equal bit nil)
	(cpuerror 0)
	(if (logbitp bit reg)
	    (setf (flag cpu 3) 1)
	    (setf (flag cpu 3) 0)))))
