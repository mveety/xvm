(in-package :xvm)

(defparameter *memory-size* 0)
(defparameter *memory-map* nil)

(defun default-reader (&rest args)
  (declare (ignore args))
  0)

(defun default-writer (&rest args)
  (declare (ignore args))
  t)

(defun default-checker (&rest args)
  (declare (ignore args))
  t)

(defstruct memmap-entry
  (start 0 :type u32int) ; first valid address
  (end 0 :type u32int)   ; last valid address
  (size 0 :type u32int)  ; size of memory area
  (io-offset 0 :type u32int) ; offset added to arguments of reader/writer/checker
  (reader #'default-reader :type function) ; read method
  (writer #'default-writer :type function) ; write method
  (checker #'default-checker :type function)) ; validity checker method

;;; (defun reader (addr) ...)
;;; (defun writer (addr value) ...)
;;; (defun checker (addr operation) ...)

(declaim (ftype (function (u32int) (or memmap-entry null)) get-memmap-entry))
(defun get-memmap-entry (addr)
  (let ((cur (car *memory-map*))
	(rest (cdr *memory-map*)))
    (loop until (or (equal cur nil)
		    (and (>= addr (memmap-entry-start cur))
			 (>= (memmap-entry-end cur) addr)))
       do (setf cur (car rest)
		rest (cdr rest)))
    cur))

(defun add-memmap-entry (start size reader writer
			  &optional (io-offset 0) (checker #'default-checker))
  (let ((tmp (make-memmap-entry :start start
				:end (- (+ start size) 1)
				:size size
				:io-offset io-offset
				:reader reader
				:writer writer
				:checker checker)))
    (incf *memory-size* size)
    (append-to-list *memory-map* tmp)))

(defun remap-memmap-entry (addr reader writer
			   &optional (io-offset 0) (checker #'default-checker))
  (let ((me (get-memmap-entry addr)))
    (if (equal me nil)
	(error "invalid memory map entry")
	(setf (memmap-entry-reader me) reader
	      (memmap-entry-writer me) writer
	      (memmap-entry-io-offset me) io-offset
	      (memmap-entry-checker me) checker))
    nil))

(defun unmap-memmap-entry (addr)
  (let ((me (get-memmap-entry addr)))
    (setf *memory-map* (remove-entry me *memory-map*))))

;;; (declaim (ftype (function (u32int) (or u8int null)) memread))
(defun memread (addr)
  (let ((me (get-memmap-entry addr)))
    (if (equal me nil)
	(error "invalid address"))
    (with-accessors ((offset memmap-entry-io-offset)
		     (readfn memmap-entry-reader)) me
      (funcall readfn (- addr offset)))))

;;; (declaim (ftype (function (u32int u8int) boolean) memwrite))
(defun memwrite (addr value)
  (let ((me (get-memmap-entry addr)))
    (if (equal me nil)
	(error "invalid address"))
    (with-accessors ((offset memmap-entry-io-offset)
		     (writefn memmap-entry-writer)) me
      (funcall writefn (- addr offset) value))))

;;; (declaim (ftype (function (u32int &optional keyword) boolean) memcheck))
(defun memcheck (addr &optional (operation :read))
  (let ((me (get-memmap-entry addr)))
    (if (equal me nil)
	nil
	(with-accessors ((io-offset memmap-entry-io-offset)
			 (checker memmap-entry-checker)) me
	  (funcall checker (- addr io-offset) operation)))))

(defun memcheck-range (addr size &optional (operation :read))
  (let ((status t))
    (dotimes (x size)
      (if status
	  (setf status (memcheck (+ addr x) operation))))
    status))

#|(declaim (ftype (function (u32int) (or u8int null)) memory)
	 (ftype (function (u8int u32int) boolean) (setf memory)))|#
(defun memory (addr)
  (memread addr))

(defun (setf memory) (val addr)
  (memwrite addr val))

(declaim (ftype (function ((or u16int fixnum)) (or boolean u16int)) memread16)
	 (ftype (function ((or u32int fixnum)) (or boolean u32int)) memread32)
	 (ftype (function (u32int (or u16int fixnum)) boolean) memwrite16)
	 (ftype (function (u32int (or u32int fixnum)) boolean) memwrite32))
(defun memread16 (addr)
  (let ((tmp 0)
	(low (memread addr))
	(high (memread (1+ addr))))
    (if (and low high)
	(progn
	  (setf (ldb (byte 8 0) tmp) low
		(ldb (byte 8 8) tmp) high)
	  (u16int tmp))
	nil)))

(defun memread32 (addr)
  (let ((tmp 0)
	(low (memread addr))
	(midlow (memread (1+ addr)))
	(midhigh (memread (+ addr 2)))
	(high (memread (+ addr 3))))
    (if (and low midlow midhigh high)
	(progn
	  (setf (ldb (byte 8 (* 8 0)) tmp) low
		(ldb (byte 8 (* 8 1)) tmp) midlow
		(ldb (byte 8 (* 8 2)) tmp) midhigh
		(ldb (byte 8 (* 8 3)) tmp) high)
	  (u32int tmp))
	nil)))

(defun memwrite16 (addr val)
  (let* ((nlist (le-u16int-to-list val))
	 (fixes '(0 1))
	 (rval t))
    (dolist (x fixes)
      (if rval
	  (setf rval (and rval (memwrite (+ addr x) (nth x nlist))))))
    nil))

(defun memwrite32 (addr val)
  (let* ((nlist (le-u32int-to-list val))
	 (fixes '(0 1 2 3))
	 (rval t))
    (dolist (x fixes)
      (if rval
	  (setf rval (and rval (memwrite (+ addr x) (nth x nlist))))))
    nil))

#|(declaim (ftype (function (u32int &optional keyword) boolean)
		memcheck16
		memcheck32))|#
(defun memcheck16 (addr &optional (operation :read))
  (and (memcheck addr operation)
       (memcheck (1+ addr) operation)))

(defun memcheck32 (addr &optional (operation :read))
  (and (memcheck addr operation)
       (memcheck (+ addr 1) operation)
       (memcheck (+ addr 2) operation)
       (memcheck (+ addr 3) operation)))
