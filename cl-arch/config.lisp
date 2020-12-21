(in-package :xvm)

;;; configuration for the emulator

(defparameter *ram-size* (megabytes 32))
(defparameter *ram-block-size* (megabytes 4))
;;; if you get array size errors set this to 'block
#+sbcl (defparameter *ram-driver* 'flat)
#+ccl (defparameter *ram-driver* 'block)
(defparameter *total-devices* 256)
(defparameter *error-on-device-remap* t)
(defparameter *error-on-memory-overlap* nil)
(defparameter *error-on-unmapped-device* t)
(defparameter *fast-get-microinstruction* t)
