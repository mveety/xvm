(in-package :xvm)

(declaim (type u32int *tty-default-size*))
(defparameter *tty-default-size* (le-list-to-u32int '(80 24 0 0)))

(defclass tty-device (default-device chanl:channel)
  ((name :initarg :name :initform "simple tty")
   (memory :initarg :memory ; supports up to 160x80
	   :initform (make-byte-array 12800))
   (thread :initform nil)
   ;;; registers
   (cmd :initform 0 :type u32int)
   (cint :initform nil)
   (dint :initform 0 :type u8int)
   (kbctl :initform 0 :type u8int)
   (kbdat :initform 0 :type u8int)
   (tty1 :initform 0 :type u8int)
   (ttyx :initform 0 :type u8int)
   (ttyy :initform 0 :type u8int)
   (ttycap :initform *tty-default-size* :type u32int)
   (uartst :initform 0 :type u32int)))
