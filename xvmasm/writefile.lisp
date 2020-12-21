(in-package :xvmasm)

(defun write-binary-file (fname bytestream)
  (with-open-file (fstream fname :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create
			   :element-type 'unsigned-byte)
    (setf *total-bytes* (length bytestream))
    (dolist (x bytestream)
      (write-byte x fstream))))

