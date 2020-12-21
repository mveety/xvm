(in-package :xvmasm)

(defun add-file-to-inst-stream (fname)
  (let* ((*curline* 1)
	 (*curfile* fname)
	 (line-tmp nil)
	 (tok-tmp nil))
    (with-open-file (fstream fname
			     :direction :input
			     :if-does-not-exist :error)
      (append-to-list *loaded-files* fname)
      (if (or *debugging* *print-files*)
	  (format t "processing ~a~%" fname))
      (loop while (not (equal (setf line-tmp (read-line fstream  nil nil)) nil))
	 do
	   (setf tok-tmp (tokenize line-tmp))
	   (if (not (equal tok-tmp nil))
	       (categorize-tokens (upcase-tokens tok-tmp)))
	   (incf *curline*)
	   (incf *total-lines*)))))

(defun add-line-to-inst-stream (string)
  (categorize-tokens (upcase-tokens (tokenize string))))

(defun add-lines-to-inst-stream (strings)
  (dolist (ln strings)
    (add-line-to-inst-stream ln)))

(defun add-tokens-to-inst-stream (toks)
  (categorize-tokens (upcase-tokens toks)))

(defun add-line (string) (add-line-to-inst-stream string))
(defun add-lines (&rest strings) (add-lines-to-inst-stream strings))
(defun add-tokens (toks) (add-tokens-to-inst-stream toks))
