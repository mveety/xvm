(in-package :xvm)

(defun memcheck-indirect (addr &optional (offset 0) (operation :read))
  (if (memcheck addr :read)
      (memcheck (+ (memory addr) offset) operation)
      nil))

(defun memread-indirect (addr &optional (offset 0))
  (if (memcheck-indirect addr offset :read)
      (memory (+ (memory addr) offset))
      nil))

(defun memwrite-indirect (addr val &optional (offset 0))
  (if (memcheck-indirect addr offset :write)
      (setf (memory (+ (memory addr) offset)) val)
      nil))

(defun no-op (&rest args)
  (declare (ignore args))
  nil)

(defstruct (opcode-function (:conc-name op-))
  (function #'no-op)
  (args 0)
  (cycles 1)
  (writepc t))

(defvar *opcodes* (make-array 256
			      :element-type 'opcode-function
			      :initial-element (make-opcode-function)))

(defmacro definstruction (name ;; below are kind of sensible defaults for insts
			  (&key (args 0)
				(cycles 1)
				(cpu t)
				(writepc t)
				(use-width nil)
				(use-mode nil)
				(use-len nil)
				(nil-return t))
			  &body body)
  (flet ((get-microinst-by-name (name)
	   (let ((insts *inst-list*)
		 (rval nil))
	     (dolist (x insts)
	       (unless rval
		 (if (equal name (getf x :name))
		     (setf rval x))))
	     rval)))
    (let* ((microinst (get-microinst-by-name name))
	   (opcode (getf microinst :opcode))
	   (fn-name (intern (string-upcase (format nil "INST-~A" name))))
	   (declares nil)
	   (ignores nil)
	   (fixed-body (if nil-return
			   `(block nil ,@body nil)
			   `(block nil ,@body)))
	   ;;(dectype '(type (cons fixnum fixnum) args))
	   (tmp nil))
      (setf ignores
	    (if (or (eq args 0)
		    (not cpu)
		    (not use-width)
		    (not use-len)
		    (not use-mode))
		      (list 'ignore
			    (if (not cpu) 'cpu)
			    (if (not use-width) 'width)
			    (if (not use-mode) 'mode)
			    (if (not use-len) 'len)
			    (if (eq args 0) 'args))
		      nil))
      (if ignores
	  (progn
	    (dolist (x ignores)
	      (if (not (equal x nil))
		  (append-to-list tmp x)))
	    (setf declares (list 'declare tmp))))
	  ;;;(setf declares (list 'declare dectype)))
      `(progn
	 (defun ,fn-name (cpu width mode len &rest args)
	   ,declares
	   ,fixed-body) ;; return nil by default. a little less typing for me
	 (setf (aref *opcodes* ,opcode)
	       (make-opcode-function :function #',fn-name
				     :args ,args
				     :cycles ,cycles
				     :writepc ,writepc))))))

(defmacro cpuerror (n)
  `(return '(:error t :interrupt ,n)))

(definstruction nop (:args 0 :cycles 1 :cpu nil)
  ;; this instruction does nothing!
  nil)

(defun-block exec (c dinst)
  (sync-interrupts c)
  (if (and (flagp c 8)
	   (flagp c 2))
      (progn
	(stack-push c (register c pc))
	(stack-push c (register c fl))
	(setf (register c pc) (register c fv)
	      (flag c 0) 0
	      (flag c 1) 0
	      (cpu-cycles c) (+ (cpu-cycles c) 1))
	(if (cpu-halted c)
	    (setf (cpu-halted c) nil))
	nil)
      (progn
	#|
	so this function sets up the cpu struct for instruction execution.
	instruction functions return a nil if all's well, otherwise they send
	back an error list in the form of (:error t :interrupt n). this will
	cause exec to restore the registers back to what they were, set up
	an interrupt, and return t. if all's well exec will do nothing and
	return nil.
	|#
	(if (cpu-halted c)
	    (return nil))
	(save-registers c)
	(if (or (equal (getf dinst :opcode) nil)
		(not (typep (getf dinst :opcode) 'u8int)))
	    (error "invalid instruction format: ~a" dinst))
	;; this let is heinous. sorry about that. i think in variables.
	(let* ((opcode (getf dinst :opcode))
	       (modmem (getf dinst :mem))
	       (modwidth (getf dinst :width))
	       (length (getf dinst :length))
	       (arg0 (getf dinst :arg0))
	       (arg1 (getf dinst :arg1))
	       (opfnst (aref *opcodes* opcode))
	       (opfn (op-function opfnst))
	       (opcycles (op-cycles opfnst))
	       (opwritepc (op-writepc opfnst))
	       (errval nil))
	  (setf errval (funcall opfn c modwidth modmem length arg0 arg1))
	  (if (getf errval :error)
	      (progn
		(setf (cpu-do-interrupt c) t
		      (cpu-interrupt-number c) (getf errval :interrupt)
		      (cpu-cycles c) (+ (cpu-cycles c) opcycles))
		(restore-registers c)
		t)
	      (progn
		(if opwritepc
		    (setf (register c pc) (+ (register c pc) length)))
		(setf (cpu-cycles c) (+ (cpu-cycles c) opcycles))
		nil))))))
