(in-package :xvm)

#|
problems with decoder microinstruction look up
a major problem i've had is the lookup of microinstructions because
popc (opcode 0x8e) was removed so all instructions after 0x8e are
offset by one.
|#
(defun slow-get-microinst (opcode)
  (let ((insts *inst-list*)
	(rval nil))
    (dolist (x insts)
      (unless rval
	(if (equal (getf x :opcode) opcode)
	    (setf rval x))))
    rval))

(defun fast-get-microinst (opcode)
  (if (<= opcode #x8d)
      (nth (- opcode #x80) *inst-list*)
      (nth (1- (- opcode #x80)) *inst-list*)))

(defun get-all-opcodes ()
  (let ((rval nil))
    (dolist (x *inst-list*)
      (append-to-list rval (getf x :opcode)))
    rval))

(defun test-microinsts ()
  (let ((opcodes (get-all-opcodes))
	(failures nil))
    (dolist (x opcodes)
      (if (not (eq (slow-get-microinst x)
		   (fast-get-microinst x)))
	  (append-to-list failures x)))
    failures))

#|
instruction decoding

this should break apart all possible instructions into their bits
based on the decoder microcode (insts.lisp, ../ILIST). this mess
does have some problems like how there is significant code replication
in the argument parsing section because of the argument reversing
problem in the processor's arch. this really should be fixed at some
point because it's slow for all implementations and makes absolutely
no sense. I don't know why I designed it this way, it was dumb. Either
way, this might be fixed before the arch is totally finalized and set
in stone.
|#

(defmacro flet-get-microinst (&body body)
  (if *fast-get-microinstruction*
      `(flet ((get-microinst (o) (fast-get-microinst o)))
	 ,@body)
      `(flet ((get-microinst (o) (slow-get-microinst o)))
	 ,@body)))

(flet-get-microinst
  #|
  wrt this flet:
  this flet allows you to switch between the two get-microinst.
  implementations. if you wish to use slow-get-microinst then
  you can just change the definition here and recompile things.
  both get-microinst implementations should be identical in
  behaviour other than speed. also note that switching to the slow
  implementation will significantly reduce execution speeds because
  it is called for every single instruction.
  |#
  (defun decode (cpu rinst)
    ;; the processor is included in this functions arguments in case
    ;; future changes require it. Also, it semantically makes sense
    ;; for all of the pipeline functions to require the processor as
    ;; and argument even if it's unused.
    (declare (ignore cpu))
    (let* ((len (getf rinst :length))
	   (dat (getf rinst :data))
	   (mods (ldb (byte 4 0) (car dat)))
	   (opcode (cadr dat))
	   (args (cddr dat))
	   (microinst (get-microinst opcode))
	   (rval nil))
      (if (equal microinst nil)
	  (error "undefined instruction ~a" opcode))
      (setf (getf rval :opcode) opcode
	    (getf rval :length) len
	    (getf rval :data) dat)
      (if (getf microinst :modwidth)
	  (setf (getf rval :width)
		(if (equal (ldb (byte 2 0) mods) 0)
		    nil
		    (ldb (byte 2 0) mods))))
      (if (getf microinst :modmem)
	  (setf (getf rval :mem) (ldb (byte 2 2) mods)))
      (case (getf microinst :regargs)
	(2 (setf (getf rval :arg0) (ldb (byte 4 4) (car args))
		 (getf rval :arg1) (ldb (byte 4 0) (car args))))
	(1 (if (getf microinst :reverse)
	       (case (getf microinst :constarg)
		 (4 (setf (getf rval :arg0) (le-list-to-u32int (subseq args 0 4))
			  (getf rval :arg1) (ldb (byte 4 4) (nth 4 args)))
		    )
		 (1 (setf (getf rval :arg0) (le-list-to-u8int (car args))
			  (getf rval :arg1) (ldb (byte 4 4) (cadr args)))
		    )
		 (0 (setf (getf rval :arg0) (ldb (byte 4 4) (car args)))
		    ))
	       (case (getf microinst :constarg)
		 (4 (setf (getf rval :arg1) (le-list-to-u32int (subseq args 1 5))
			  (getf rval :arg0) (ldb (byte 4 4) (nth 0 args)))
		    )
		 (1 (setf (getf rval :arg1) (le-list-to-u8int (cadr args))
			  (getf rval :arg0) (ldb (byte 4 4) (car args)))
		    )
		 (0 (setf (getf rval :arg0) (ldb (byte 4 4) (car args)))
		    ))))
	(0 (if (getf microinst :constarg)
	       (progn
		 (setf (getf rval :arg0) (le-list-to-uint args))
		 ))
	   ))
      rval))

  (defun get-microinst-def (fetch-data)
    (get-microinst (cadr (getf fetch-data :data)))))
