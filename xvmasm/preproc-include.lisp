(in-package :xvmasm)

(defun preproc-include (args)
  (dolist (x args)
    (let ((fname (subseq x 1 (- (length x) 1))))
      (add-file-to-inst-stream fname))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-preproc-plugin "include" #'preproc-include 2))
