(in-package :xvm)

#|
system manager memory sizes and such.
note: if these are changed be sure to update the declaim form below this
defparameters block.
|#
(defparameters (*rom-start-addr* (megabytes 28))
	       (*bootrom-size* 512)
	       (*nvram-size* 200)
	       (*sysram-size* (* 1024 1024))
	       (*rom-size* (* 4 (* 1024 1024)))
	       (*sysrom-size* (- *rom-size*
	       		  (+ *bootrom-size*
	       		     *nvram-size*
	       		     *sysram-size*))))

;;; note: depending on your lisp implementation it might not like
;;; how big *sysrom* and *sysram* are. you might need to fiddle with
;;; this to make it work. it seems to work well on sbcl on windows
;;; freebsd. your milage may very.
(declaim (type (simple-array u8int) *bootrom*)
	 (type (simple-array u8int) *nvram*)
	 (type (simple-array u8int) *sysrom*)
	 (type (simple-array u8int) *sysram*))

(defun make-map-list (start size)
  (list start (1- (+ start size))))

(defmacro mml-prev (prevmap size)
  `(make-map-list (1+ (cadr ,prevmap)) ,size))

(defparameters (*bootrom-map* (make-map-list 0 *bootrom-size*))
               (*nvram-map* (mml-prev *bootrom-map* *nvram-size*))
               (*sysrom-map* (mml-prev *nvram-map* *sysrom-size*))
	       (*sysram-map* (mml-prev *sysrom-map* *sysram-size*))
	       (*rom-map* (make-map-list *rom-start-addr* *rom-size*))
	       ;; system manager variables and state
	       ;; these should probably be initialized when the sysman
	       ;; is initialized and mapped instead of now.
	       (*bootrom* (make-byte-array *bootrom-size*))
	       (*nvram* (make-byte-array *nvram-size*))
	       (*sysrom* (make-byte-array *sysrom-size*))
	       (*sysram* (make-byte-array *sysram-size*))
	       (*sm-mapped* t)
	       ;; main ram i/o protocol
	       (*mainram-read* #'default-reader)
	       (*mainram-write* #'default-writer)
	       (*mainram-check* #'default-checker))

(defmacro between (var start end)
  `(and (>= ,var ,start)
	(<= ,var ,end)))

(defmacro bl (var bounds-list)
  `(between ,var (car ,bounds-list) (cadr ,bounds-list)))

(declaim (ftype (function (u32int) (or u8int boolean))
		bootrom-read
		nvram-read
		sysrom-read
		sysram-read)
	 (ftype (function (u32int u8int) boolean)
		nvram-write
		sysram-write))

(defun bootrom-read (addr)
  (aref *bootrom* addr))

(defun nvram-read (addr)
  (aref *nvram* addr))

(defun sysrom-read (addr)
  (aref *sysrom* addr))

(defun sysram-read (addr)
  (aref *sysram* addr))

(defun sm-read (addr)
  (if (not *sm-mapped*) ; if the sm isn't mapped then passthrough to main ram
      (funcall *mainram-read* addr)
      (cond
	((bl addr *bootrom-map*) (bootrom-read addr))
	((bl addr *nvram-map*) (nvram-read (- addr (car *nvram-map*))))
	((bl addr *sysrom-map*) (sysrom-read (- addr (car *sysrom-map*))))
	((bl addr *sysram-map*) (sysram-read (- addr (car *sysram-map*)))))))

(defun nvram-write (addr val)
  (setf (aref *nvram* addr) val)
  t)

(defun sysram-write (addr val)
  (setf (aref *sysram* addr) val)
  t)

(defun passthrough-write (addr val)
  (funcall *mainram-write* addr val))

(defun sm-write (addr val)
  (if (not *sm-mapped*)
      (funcall *mainram-write* addr val)
      (cond
	((bl addr *nvram-map*)
	 (nvram-write (- addr (car *nvram-map*)) val))
	((bl addr *sysram-map*)
	 (sysram-write (- addr (car *sysram-map*)) val))
	(t (passthrough-write addr val)))))

(defun sm-check (addr operation)
  (if (not *sm-mapped*)
      (funcall *mainram-check* addr operation)
      (cond
	((equal operation :write)
	 (if (or (bl addr *nvram-map*)
		 (bl addr *sysram-map*))
	     t
	     (funcall *mainram-check* addr operation)))
	((equal operation :read) t))))

(defun add-sysman-to-ram ()
  (add-memmap-entry (car *rom-map*)
		    *rom-size*
		    #'sm-read
		    #'sm-write
		    (car *rom-map*)
		    #'sm-check))

(defun initialize-sysman (backing-reader backing-writer backing-checker)
  (add-sysman-to-ram)
  (setf *mainram-read* backing-reader
	*mainram-write* backing-writer
	*mainram-check* backing-checker))

(defun map-sysman ()
  (setf *sm-mapped* t))

(defun unmap-sysman ()
  (setf *sm-mapped* nil))
