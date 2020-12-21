(in-package :xvm)

(defclass default-device ()
  ((name :initarg :name
	 :initform "unamed device")
   (start-address :initarg :start-address
		  :initform 0
		  :type u32int)
   (size :initarg :size
	 :initform 0
	 :type u32int)
   (memory :initarg :memory
	   :initform (make-byte-array 512))))

(defgeneric device-read (device-object addr)
  (:documentation "read from a device"))

(defgeneric device-write (device-object addr value)
  (:documentation "write to a device"))

(defgeneric device-interrupt (device-object addr)
  (:documentation "send a device interrupt"))

(defgeneric device-init (device-object)
  (:documentation "initialize emulator device module"))

(defparameter *master-cpu* nil) ; the cpu that handles interrupts
(defparameter *devices* (make-array *total-devices*
				    :adjustable nil
				    :initial-element nil))

(defun register-device (device-object address)
  (if (>= address *total-devices*)
      (error "device high order address is higher than max address ~a"
	     *total-devices*))
  (if (not (aref *devices* address))
      (setf (aref *devices* address) device-object)
      (if *error-on-device-remap*
	  (error "unable to remap device ~a" address)
	  (let ((*error-on-device-remap* t))
	    (setf (aref *devices* address) nil)
	    (register-device device-object address)))))

(defun unregister-device (address)
  (if (>= address *total-devices*)
      (error "device high order address is higher than max address ~a"
	     *total-devices*))
  (if (not (aref *devices* address))
      (if *error-on-unmapped-device*
	  (error "device ~a is unmapped!" address)
	  nil)
      (progn
	(setf (aref *devices* address) nil)
	t)))

(defun devread (address)
  (let* ((high-byte (u8int (ldb (byte 8 8) address)))
	 (low-byte (u8int (ldb (byte 8 0) address)))
	 (device-object (aref *devices* high-byte)))
    (if (not device-object)
	(if *error-on-unmapped-device*
	    (error "device ~a is unmapped!" high-byte)
	    0)
	(device-read device-object low-byte))))

(defun devwrite (address value)
  (let* ((high-byte (u8int (ldb (byte 8 8) address)))
	 (low-byte (u8int (ldb (byte 8 0) address)))
	 (device-object (aref *devices* high-byte)))
    (if (not device-object)
	(if *error-on-unmapped-device*
	    (error "device ~a is unmapped!" high-byte)
	    nil)
	(device-write device-object low-byte value))))

(defun devinterrupt (address)
  (let* ((high-byte (u8int (ldb (byte 8 8) address)))
	 (low-byte (u8int (ldb (byte 8 0) address)))
	 (device-object (aref *devices* high-byte)))
    (if (not device-object)
	(if *error-on-unmapped-device*
	    (error "device ~a is unmapped!" high-byte)
	    nil)
	(device-interrupt device-object low-byte))))

;;; this is for devices to throw ints
(defun cpuinterrupt (int)
  (if (equal *master-cpu* nil)
      (error "master cpu is not set! emulator not started?")
      (interrupt *master-cpu* int)))
