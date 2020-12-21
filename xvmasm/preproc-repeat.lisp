(in-package :xvmasm)

(defun preproc-repeat (args)
  (let* ((ntimes (string-to-num (car args)))
	 (toks (cdr args))
	 (i 0))
    (dformat t "REPEAT: doing ~a ~a times~%" toks ntimes)
    (loop until (equal i ntimes) do
	 (add-tokens toks)
	 (incf i))
    (dformat t "i = ~a, ntimes = ~a~%" i ntimes)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-preproc-plugin "repeat" #'preproc-repeat 4))
