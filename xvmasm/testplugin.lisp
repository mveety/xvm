(in-package :xvmasm)

(defun testplugin (args)
  (format t "testplugin: ~a~%" args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-preproc-plugin "testplugin" #'testplugin))
