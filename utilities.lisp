;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

(defun read-line-crlf (stream)
  "Read a line terminated with a CR/LF pair."
  (let ((line (read-line stream)))
    (assert (char= #\Return
                   (char line (1- (length line)))))
    ;; Using displaced arrays instead of subseq does not seem to offer
    ;; any advantages on SBCL.
    (subseq line 0 (1- (length line)))))

(defun format-crlf (stream fmt &rest args)
  (format stream "~?~C~C" fmt args #\Return #\Newline))

(defun octets-to-hex-string (octets)
  (string-downcase (format nil "~{~2,'0X~}" (coerce octets 'list))))

(defun string-to-hex-string (string)
  (octets-to-hex-string (babel:string-to-octets string :encoding :ascii)))

;;; Hex string handling

(defun read-line-and-split (stream)
  (cl-ppcre:split "\\s" (read-line-crlf stream)))

(defun parse-hex-string (string)
  (declare (type string string))
  (assert (evenp (length string)))
  (iter (with output = (make-array (truncate (length string) 2)
                                   :element-type '(unsigned-byte 8)))
        (for out-pos from 0 below (truncate (length string) 2))
        (declare (type fixnum out-pos))
        (flet ((dc (index)
                 (let ((c (digit-char-p (char string index) 16)))
                   (or c
                       (error "Bogus string")))))
          (declare (inline dc))
          (setf (aref output out-pos)
                (logand #xFF 
                        (logior (ash (dc (* out-pos 2)) 4)
                                (dc (1+ (* out-pos 2)))))))
        (finally (return output)) ))


;;; TODO Use compatibility layer.
;; From UFFI via CFFI 
(defun getenv (var)
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+(or cmu scl) (cdr (assoc (string var) ext:*environment-list* :test #'equalp
                             :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+(or mcl ccl) (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmu scl gcl lispworks lucid mcl ccl sbcl)
  (error "error getenv not implemented"))

;;; EOF


