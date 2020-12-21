(in-package :xvmasm)

(defparameter most-recent-label nil)

(defun process-label (tok)
  (let* ((stripped (subseq (car tok) 0 (- (length (car tok)) 1)))
	 (tmp (coerce stripped 'list))
	 (tmp2 nil))
    (if (equal (string-first stripped) ".")
	(progn
	  (if (equal most-recent-label nil)
	      (error "unable to use local label outside of block"))
	  (setf tmp2 (append (coerce most-recent-label 'list) tmp))
	  (setf tmp (coerce tmp2 'string))
	  (append-to-list *instruction-stream* `(:label ,tmp)))
	(progn
	  (setf most-recent-label stripped)
	  (append-to-list *instruction-stream* (list :label stripped))))))

(defun categorize-tokens (tok)
  (cond
    ((equal (string-last (car tok)) ":") (process-label tok))
    ((equal (string-first (car tok)) "%") (process-preproc tok))
    (t (append-to-list *instruction-stream* (process-instruction tok)))))

(defun resolve-labels (inst-stream)
  (let* ((curaddress *start-point*))
    (dolist (x inst-stream)
      (cond
	((or (equal (car x) :opcode)
	     (equal (car x) :data))
	 (setf curaddress (+ curaddress (getf x :size))))
	((equal (car x) :label)
	 (append-to-list *labels* (list (cadr x) curaddress)))))))
