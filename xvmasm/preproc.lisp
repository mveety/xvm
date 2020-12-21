(in-package :xvmasm)

(defun preproc-begin (args)
  (let ((tmp 0)
	(num ""))
    (if (equal (car args) nil)
	(setf *start-point* 0)
	(progn
	  (if (and (string-equal (subseq (car args) 0 1) "0")
		   (string-equal (subseq (car args) 1 2) "X"))
	      (setf num (coerce
			 (append (coerce "#16r" 'list)
				 (coerce (subseq (car args) 2) 'list))
			 'string))
	      (setf num (car args)))
	  (setf tmp (read-from-string num))
	  (if (not (numberp tmp))
	      (error "begin: argument not a number"))
	  (setf *start-point* tmp)))))

(defun preproc-define (args)
  (if (< (length args) 2)
      (error "define: too few args"))
  (let ((name (car args))
	(value (string-to-num (cadr args))))
    (append-to-list *defines* `(,name ,value))))

(defun preproc-byte (args)
  (let ((res nil)
	(tmp 0)
	(len 0)
	(stmp nil))
    (dolist (x args)
      (setf stmp (string-to-num x)
	    (ldb (byte 8 0) tmp) stmp
	    len (+ len 1))
      (append-to-list res tmp))
    (append-to-list *instruction-stream* (list :data res :size len))))

(defun preproc-halfword (args)
  (let ((res nil)
	(high-byte 0)
	(low-byte 0)
	(stmp nil)
	(len 0))
    (dolist (x args)
      (setf stmp (string-to-num x)
	    (ldb (byte 8 0) high-byte) (ldb (byte 8 8) stmp)
	    (ldb (byte 8 0) low-byte) (ldb (byte 8 0) stmp))
      (append-to-list res low-byte)
      (append-to-list res high-byte)
      (setf len (+ len 2)
	    high-byte 0
	    low-byte 0))
    (append-to-list *instruction-stream* (list :data res :size len))))

(defun preproc-word (args)
  (let ((res nil)
	(hhb 0)
	(mhb 0)
	(mlb 0)
	(llb 0)
	(stmp nil)
	(len 0))
    (dolist (x args)
      (setf stmp (string-to-num x)
	    (ldb (byte 8 0) hhb) (ldb (byte 8 24) stmp)
	    (ldb (byte 8 0) mhb) (ldb (byte 8 16) stmp)
	    (ldb (byte 8 0) mlb) (ldb (byte 8 8) stmp)
	    (ldb (byte 8 0) llb) (ldb (byte 8 0) stmp))
      (setf res (append res (list llb mlb mhb hhb)))
      (setf len (+ len 4)
	    hhb 0
	    mhb 0
	    mlb 0
	    llb 0))
    (append-to-list *instruction-stream* (list :data res :size len))))

(defun preproc-string (args)
  (let ((res nil)
	(slist nil)
	(cstring nil)
	(len 0)
	(slen 0))
    (dolist (x args)
      (if (or (equal (length x) 0)
	      (equal (length x) 2))
	  (progn
	    (append-to-list res 0)
	    (setf len (+ len 1)))
	  (progn
	    (setf cstring (subseq x 1 (- (length x) 1))
		  slist (coerce cstring 'list)
		  slen (length cstring))
	    (dolist (y slist)
	      (append-to-list res (char-code y)))
	    (setf len (+ len slen)
		  slen 0
		  slist nil
		  cstring nil))))
    (append-to-list *instruction-stream* (list :data res :size len))))

(defun list-loaded-plugins (&key (newline t))
  (if *debugging*
      (progn
	(dolist (x *preprocessor-plugins*)
	  (format t "~a " (car x)))
	(if newline
	    (format t "~%")))))

(defun find-preproc-plugin (cmd &optional (plugins *preprocessor-plugins*))
  (if (equal plugins nil)
      nil
      (if (string-equal (car (car plugins)) cmd)
	  (car plugins)
	  (find-preproc-plugin cmd (cdr plugins)))))

(defun run-preproc-plugin (cmd args)
  (dformat t "looking for preprocessor plugin ~a~%" cmd)
  (let* ((plugin-entry (find-preproc-plugin cmd))
	 )
    (if (equal plugin-entry nil)
	(error "preprocessor directive not found: ~a~%" cmd))
    (dformat t "found plugin~%")
    (funcall (cadr plugin-entry) args)
    nil))

(defun plugin-exists-p (name &optional (pp *preprocessor-plugins*))
  (if (equal pp nil)
      nil
      (if (string-equal (car (car pp)) (string-upcase name))
	  t
	  (plugin-exists-p name (cdr pp)))))

(defun add-preproc-plugin (name fun &optional (revision 0))
  (let ((cleaned-name (string-upcase name)))
    (if (not (plugin-exists-p name))
	(progn
	  (dformat t "adding preprocessor plugin ~a~%" cleaned-name)
	  (append-to-list *preprocessor-plugins*
			  `(,cleaned-name ,fun ,revision))))))

(defun process-preproc (toks)
  (let* ((cmd (subseq (car toks) 1))
	 (args (cdr toks)))
    (cond
      ((string-equal cmd "BEGIN") (preproc-begin args))
      ((string-equal cmd "DEFINE") (preproc-define args))
      ((string-equal cmd "STRING") (preproc-string args))
      ((string-equal cmd "BYTE") (preproc-byte args))
      ((string-equal cmd "CHAR") (preproc-byte args))
      ((string-equal cmd "INT8") (preproc-byte args))
      ((string-equal cmd "HALFWORD") (preproc-halfword args))
      ((string-equal cmd "HALF") (preproc-halfword args))
      ((string-equal cmd "INT16") (preproc-halfword args))
      ((string-equal cmd "WORD") (preproc-word args))
      ((string-equal cmd "FULLWORD") (preproc-word args))
      ((string-equal cmd "INT32") (preproc-word args))
      (t (run-preproc-plugin cmd args)))))
