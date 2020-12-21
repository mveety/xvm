(in-package :xvm)

(defclass console (default-device)
  ((name :initform "boot console")
   (start-address :initform 0)
   (size :initform 0)
   (memory :initform nil)
   (outchan :initform (make-instance 'chanl:channel))
   (stream :initform *terminal-io*
	   :initarg :stream)
   (inproc :initform nil)
   (outproc :initform nil)
   (cmdreg :initform 0)
   (kblock :initform (bt:make-lock "keybuf lock"))
   (ignore-keys :initform t)
   (autoecho :initform nil)
   (keybuf :initform nil)))

(defun cons-reader (cons)
  (let ((keytmp 'eof))
    (with-slots (kblock keybuf stream ignore-keys
			outchan autoecho) cons
      (while (not (equal keytmp 'eof))
	(setf keytmp (read-byte stream nil 'eof))
	(unless (or ignore-keys (equal keytmp 'eof))
	  (when autoecho
	    (chanl:send outchan keytmp :blockp nil))
	  (bt:with-lock-held (kblock)
	    (append-to-list keybuf keytmp)))))))

(defun cons-writer (cons)
  (let (outtmp)
    (with-slots (outchan stream) cons 
      (loop
	 (setf outtmp (chanl:recv outchan))
	 (write-byte outtmp stream)))))

(defmethod device-interrupt ((cons console) addr)
  (when (equal addr 3)
    (with-slots (cmdreg ignore-keys autoecho kblock keybuf) cons
      (case cmdreg
	(0 nil)
	(1 (setf ignore-keys t))
	(2 (setf ignore-keys nil))
	(3 (setf autoecho t))
	(4 (setf autoecho nil))
	(5 (bt:with-lock-held (kblock)
	     (setf keybuf nil)))
	(otherwise nil)))))

(defmethod device-read ((cons console) addr)
  (let ((rval nil))
    (with-slots (cmdreg kblock keybuf) cons
      (case addr
	(0 (setf rval cmdreg))
	(1 (bt:with-lock-held (kblock)
	     (setf rval (car keybuf))
	     (unless (equal rval nil)
	       (setf keybuf (cdr keybuf)))))
	(otherwise (setf rval nil))))
    (when (equal rval nil)
      (setf rval 0))
    (u8int rval)))

(defmethod device-write ((cons console) addr value)
  (with-slots (cmdreg outchan) cons
    (case addr
      (0 (setf cmdreg (u8int value))
	 nil)
      (2 (chanl:send outchan (u8int value))
	 nil)
      (otherwise nil))))

(defmethod device-init ((cons console))
  (with-slots (inproc outproc) cons
    (when (equal inproc nil)
      (setf inproc (bt:make-thread (lambda () (cons-reader cons))
				   :name "console input thread")))
    (when (equal outproc nil)
      (setf outproc (bt:make-thread (lambda () (cons-writer cons))
				    :name "console output thread"))))
  nil)
