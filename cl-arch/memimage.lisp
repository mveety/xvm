(in-package :xvm)

(defun read-file-to-list (fname)
  (let ((rval nil)
	(tmp t))
    (with-open-file (infile fname :direction :input
			    :element-type 'unsigned-byte)
      (while (not (equal tmp nil))
	(setf tmp (read-byte infile nil nil))
	(unless (equal tmp nil)
	  (append-to-list rval tmp))))
    rval))

(defun calculate-checksum (lst)
  (let ((total 0))
    (dolist (x lst)
      (setf total (+ total x)))
    (mod total 128)))

(defun read-file-to-memimage (fname)
  (let* ((data (read-file-to-list fname))
	 (size (length data))
	 (checksum (calculate-checksum data)))
    (list 'memimage :size size :checksum checksum :data data)))

(defun load-list-to-memory (lst &optional (start-address 0))
  (let ((size (length lst))
	(written 0))
    (dolist (x lst)
      (when (memcheck (+ written start-address) :write)
	(memwrite (+ written start-address) x)
	(incf written)))
    (list :size size :written written)))

(defun load-list-to-buffer (lst buf &optional (start 0))
  (let ((max (length buf))
	(size (length lst))
	(written 0))
    (dolist (x lst)
      (when (< (+ start written) max)
	(setf (aref buf (+ start written)) x)
	(incf written)))
    (list :size size :written written)))

(defun load-memimage-to-buffer (memimage buf &optional (start 0))
  (let* ((plist (cdr memimage))
	 (status (load-list-to-buffer (getf plist :data) buf start)))
    (if (equal (getf plist :size) (getf status :written))
	t
	nil)))

(defun load-memimage-to-memory (memimage &optional (start 0))
  (let* ((plist (cdr memimage))
	 (status (load-list-to-memory (getf plist :data) start)))
    (if (equal (getf plist :size) (getf status :written))
	t
	nil)))

(defun check-memimage (memimage)
  (if (not (equal (car memimage) 'memimage))
      nil
      (let* ((plist (cdr memimage))
	     (check1 (getf plist :checksum))
	     (check2 (calculate-checksum (getf plist :data)))
	     (size1 (getf plist :size))
	     (size2 (length (getf plist :data))))
	(if (and (equal check1 check2)
		 (equal size1 size2))
	    t
	    nil))))

       
