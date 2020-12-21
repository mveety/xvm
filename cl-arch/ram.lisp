(in-package :xvm)

(declaim (type (or (simple-array u8int) null) *ram*)
	 (ftype (function (u32int) (or boolean u8int)) ram-read)
	 (ftype (function (u32int u8int) boolean) ram-write)
	 (ftype (function (u32int &optional keyword) boolean) ram-check))

(defparameter *ram* nil)
(defparameter *ram-initialized* nil)

(defun ram-error-check ()
  (if (not *ram-initialized*)
      (error "ram is not initialized")))

(defun ram-read (addr)
  (ram-error-check)
  (if (< addr *ram-size*)
      (aref *ram* addr)
      nil))

(defun ram-write (addr val)
  (ram-error-check)
  (if (< addr *ram-size*)
      (progn
	(setf (aref *ram* addr) (u8int val))
	t)
      nil))

(defun ram-check (addr &optional (operation :read))
  (declare (ignore operation))
  (ram-error-check)
  (if (< addr *ram-size*)
      t
      nil))

(defun initialize-ram ()
  (setf *ram* (make-byte-array *ram-size*)
	*ram-initialized* t)
  (add-memmap-entry (1+ (cadr *rom-map*))
		    (- *ram-size* *rom-size*)
		    #'ram-read
		    #'ram-write
		    0
		    #'ram-check))

(defun initialize-all-memory ()
  (if (equal *ram-driver* 'block)
      (progn
	(initialize-sysman #'block-ram-read #'block-ram-write #'block-ram-check)
	(initialize-block-ram))
      (progn
	(initialize-sysman #'ram-read #'ram-write #'ram-check)
	(initialize-ram))))
