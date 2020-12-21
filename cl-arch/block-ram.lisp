(in-package :xvm)

#|
this implementation of the ram breaks it up into blocks in case your lisp
implementation is unable to make big arrays (like ccl, for example)
|#

(declaim (inline check-block-size block-check-addr
		 linear-to-block-addr block-ram-error-check
		 get-block-and-addr)
	 (ftype (function (u32int) (or boolean u8int)) block-ram-read)
	 (ftype (function (u32int u8int) boolean) block-ram-write)
	 (ftype (function (u32int &optional keyword) boolean) block-ram-check))

(defparameter *ram-blocks* nil)
(defparameter *nblocks* 0)
(defparameter *block-size* *ram-block-size*)
(defparameter *block-ram-initialized* nil)

(defun check-block-size ()
  (if (< *ram-size* *ram-block-size*)
      t
      (if (not (equal (mod *ram-size* *ram-block-size*) 0))
	  nil
	  t)))

(defun block-check-addr (addr)
  (if (< addr (* *nblocks* *block-size*))
      t
      nil))

(defun linear-to-block-addr (addr)
  (if (< addr (* *nblocks* *block-size*))
      (list (floor addr *block-size*)
	    (mod addr *block-size*))
      nil))

(defun populate-ram-blocks (ram-size &key (block-size *ram-block-size*))
  (let ((nblocks (+ (floor ram-size block-size)
		    (if (equal (mod ram-size block-size) 0)
			0
			1)))
	(left ram-size)
	(tmp-block nil))
    (dotimes (n nblocks)
      (if (and (equal (floor left ram-size) 0)
	       (> left 0))
	  (setf tmp-block (make-byte-array left))
	  (setf tmp-block (make-byte-array block-size)
		left (- left block-size)))
      (append-to-list *ram-blocks* tmp-block))
    (setf *nblocks* nblocks
	  *block-size* block-size)))

(defun block-ram-error-check ()
  (if (not *block-ram-initialized*)
      (error "ram is not initialized")))

(defun get-block-and-addr (addr)
  (let* ((baddr (linear-to-block-addr addr))
	 (blk (nth (car baddr) *ram-blocks*))
	 (addr (cadr baddr)))
    (list blk addr)))

(defun block-ram-read (addr)
  (block-ram-error-check)
  (if (block-check-addr addr)
      (let* ((blk-addr (get-block-and-addr addr)))
	(aref (car blk-addr) (cadr blk-addr)))
      nil))

(defun block-ram-write (addr val)
  (block-ram-error-check)
  (if (block-check-addr addr)
      (let* ((blk-addr (get-block-and-addr addr)))
	(setf (aref (car blk-addr) (cadr blk-addr)) (u8int val))
	t)
      nil))

(defun block-ram-check (addr &optional (operation :read))
  (declare (ignore operation))
  (block-ram-error-check)
  (block-check-addr addr))

(defun initialize-block-ram ()
  (if (not *block-ram-initialized*)
      (progn
	(populate-ram-blocks *ram-size*)
	(setf *block-ram-initialized* t)
	(add-memmap-entry (1+ (cadr *rom-map*))
			  (- *ram-size* *rom-size*)
			  #'block-ram-read
			  #'block-ram-write
			  0
			  #'block-ram-check)
	t)
      nil))
