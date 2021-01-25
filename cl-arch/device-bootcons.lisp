(in-package :xvm)

(defclass bootcons (default-device)
  ((name :initform "simple boot console")
   (start-address :initform 0)
   (memory :initform nil)
   (use-format :initform nil
	       :initarg :use-format)
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
  (with-slots (outstream use-format) cons
    (case addr
      (0 nil)
      (1 (if use-format
	     (progn
	       (format outstream "~a" (code-char (u8int value)))
	       (force-output outstream)
	       nil)
	     (progn
	       (write-byte (u8int value) outstream)
	       nil)))
      (2 nil)
      (otherwise nil))))

(defmethod device-interrupt ((cons bootcons) addr)
  (declare (ignore cons addr))
  nil)

(defmethod device-init ((cons bootcons))
  (if (slot-value cons 'use-format)
      (format t "bootcons: warning: using format for console output~%"))
  nil)

(defun devtest-bootcons ()
  (let ((*devices* (make-array *total-devices*
			       :adjustable nil
			       :initial-element nil))
	(testdev (make-instance 'bootcons
				:use-format t
				:memory '(65 66 67 68 10))))
    (register-device testdev 0)
    (print (all-devinfo))
    (format t "~%~%")
    (asm-test nil `((,r0 68))
	      "in.b r0,2"
	      "out.b 1,r0"
	      "in.b r0,2"
	      "out.b 1,r0"
	      "in.b r0,2"
	      "out.b 1,r0"
	      "in.b r0,2"
	      "out.b 1,r0"
	      "in.b r1,2"
	      "out.b 1,r1")))
