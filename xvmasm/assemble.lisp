(in-package :xvmasm)

(defun assemble (fname &optional (outfile nil))
  (let ((*start-point* 0)
	(*token-stream* nil)
	(*instruction-stream* nil)
	(*stage2-inst-stream* nil)
	(*datastream* nil)
	(*labels* nil)
	(*defines* nil)
	(*curline* 0)
	(*curfile* "MAIN")
	(*loaded-files* nil))
    (add-file-to-inst-stream fname)
    (resolve-labels *instruction-stream*)
    (resolve-all-const-args *instruction-stream*)
    (emit-stage2-inst-stream *instruction-stream*)
    (setf *datastream* (generate-stage2-bytestream *stage2-inst-stream*))
    (if (not (equal outfile nil))
	(write-binary-file outfile *datastream*))
    (setf *total-files* (length *loaded-files*))
    *datastream*))
