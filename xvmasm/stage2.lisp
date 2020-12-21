(in-package :xvmasm)

(defparameter toplevel-label nil)

(defun find-label (name &optional (lbls *labels*))
  (let ((cur (car lbls)))
    (if (equal cur nil)
	nil
	(if (string-equal (car cur) name)
	    (cadr cur)
	    (find-label name (cdr lbls))))))

(defun find-define (name &optional (defs *defines*))
  (find-label name defs))

(defun resolve-label-name (str)
  (if (string-equal (subseq str 0 1) ".")
      (coerce (append (coerce toplevel-label 'list)
		      (coerce str 'list))
	      'string)
      str))

(defun resolve-const-arg (inst)
  (let ((tmp nil)
	(carg (getf (getf inst :args) :const)))
    (if (equal (type-of carg) 'cons)
	(cond
	  ((equal (car carg) 'define)
	   (setf tmp (find-define (cadr carg)))
	   (if (equal tmp nil)
	       (error "undefined macro variable: ~a" (cadr carg)))
	   (setf (getf (getf inst :args) :const) tmp))
	  ((equal (car carg) 'label)
	   (setf tmp (find-label (resolve-label-name (cadr carg))))
	   (if (equal tmp nil)
	       (error "undefined label: ~a" (cadr carg)))
	   (setf (getf (getf inst :args) :const) tmp))))))

(defun resolve-all-const-args (inst-stream)
  (dolist (x inst-stream)
    (cond
      ((equal (car x) :label) (if (not (find-in-string #\. (cadr x)))
				  (setf toplevel-label (cadr x))))
      ((equal (car x) :opcode) (resolve-const-arg x)))))

(defun emit-stage2-inst-stream (inst-stream)
  (dolist (x inst-stream)
    (if (not (equal (car x) :label))
	(progn
	  (if (equal (car x) :opcode)
	      (incf *total-instructions*))
	  (append-to-list *stage2-inst-stream* x)))))
