(in-package :xvm)

(defclass console (default-device)
  ((name :initform "boot console")
   (start-address :initform 0)
   (size :initform 0)
   (memory :initform nil)
   (stream :initform *terminal-io*
	   :initarg :stream)
   (devproc :initform nil)
   (consbuf :initform nil)
   (conslock :initform (bt:make-lock "console object lock"))
   ;; registers
   (xsize :initform 80)
   (ysize :initform 24)
   (curx :initform 0)
   (cury :initform 0)
   (autoecho :initform t) ; echo keys read from the keyboard
   (has-keys :initform nil) ; has keys in the buffer (for polling)
   (allow-int :initform t) ; allow device to send interrupts to the cpu
   (int-num :initform #16r0f
	    :initarg :interrupt) ; interrupt vector
   ))

(defun console-putch (cons win char)
  (with-slots (xsize ysize curx cury) cons
    (if (>= curx xsize)
	(progn
	  (incf cury)
	  (setf curx 0)))
    (if (>= cury ysize)
	(setf cury 0))
    (charms:write-char-at-point win char curx cury)))

(defun console-process-defun (cons)
  (with-slots (memory stream consbuf conslock
	       xsize ysize curx cury
	       autoecho has-keys allow-int int-num) cons
    (charms:with-curses ()
      (let ((keyin nil)
	    (win charms:*standard-window*)
	    (tmp nil)
	    )
	(charms:disable-echoing)
	(charms:enable-raw-input)
	(multiple-value-bind (w h)
	    (charms:window-dimensions win)
	  (setf xsize w
		ysize h))
	(loop until exit do
	  (setf keyin (charms:get-char win :ignore-error t))
	  (bt:with-lock-held (conslock)
	    (if (not (equal keyin nil))
		(progn
		  (append-to-list memory (char-code keyin))
		  (setf has-keys t)
		  (if allow-int (cpuinterrupt (u4int int-num)))
		  (if autoecho (append-to-list consbuf (char-code keyin)))
		  (setf keyin nil))
		(progn
		  (if (not (equal consbuf nil))
		      (console-putch cons win (code-char (car consbuf))))
		  (setf consbuf (cadr consbuf)))))
	  (charms:move-cursor win curx cury)
	  (sleep 1/60))))))
