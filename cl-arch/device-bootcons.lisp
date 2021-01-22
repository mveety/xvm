(in-package :xvm)

(defclass bootcons (default-device)
  ((name :initform "simple boot console")
   (start-address :initform 0)
   (memory :initform nil)
   (outstream :initarg :outstream
	      :initform *standard-output*)
   (instream :initarg :instream
	     :initform *standard-input*)))

(defmethod device-read ((cons bootcons) addr)
  (with-slots (memory) cons
    (case addr
      (0 0)
      (1 0)
      (2 (if memory
	     (let ((tmp (car memory)))
	       (setf memory (cdr memory))
	       tmp)
	     0))
      (otherwise 0))))

(defmethod device-write ((cons bootcons) addr value)
  (with-slots (outstream) cons
    (case addr
      (0 nil)
      (1 (progn
	   (write-byte (u8int value) outstream)
	   nil))
      (2 nil)
      (otherwise nil))))

(defmethod device-interrupt ((cons bootcons) addr)
  (declare (ignore cons addr))
  nil)

(defmethod device-init ((cons bootcons))
  (declare (ignore cons))
  nil)
