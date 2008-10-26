;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

(defclass* dbus-connection ()
  (stream))

(defun accepted-methods (stream)
  (format-crlf stream "AUTH")
  (force-output stream)
  (destructuring-bind (response . rest)
      (read-line-and-split stream)
    (assert (string= "REJECTED" response))
    rest))


(defun dbus-connect (host port)
  "Returns an connection to the given `host' and `port'"
  (let* ((success nil)
         (stream (usocket:socket-stream (usocket:socket-connect 
                                         host port :element-type '(unsigned-byte 8))))
         (con (make-instance 'dbus-connection
                             :stream stream)))
    (unwind-protect 
         (progn
           ;; Send greeting (the NUL byte)
           (write-byte 0 stream)
           (force-output stream)
           ;; Now the ASCII authentication protocol can start.
           (let ((ascii-stream (flexi-streams:make-flexi-stream 
                                stream 
                                :external-format :ascii
                                :element-type 'character)))
             ;; Check which authentication methods are accepted and try the
             ;; ones we support.
             (let ((methods (accepted-methods ascii-stream)))
               (format t "Server supports authentication via:~{ ~A~}~%" methods)
               (or (when (find "DBUS_COOKIE_SHA1" methods :test #'string=) 
                     (try-cookie-sha1-auth ascii-stream))
                   (when (find "ANONYMOUS" methods :test #'string=) 
                     (try-anonymous-auth ascii-stream))
                   (error "Could not authenticate to server."))))
           (setq success t)
           con)
      ;; Cleanup
      (unless success
        (dbus-close con)))))

(defvar *endianness* :little-endian)

(defun read-uint32 (buf index)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf))
  (ecase *endianness*
    (:little-endian 
     (logior (aref buf index)
             (ash (aref buf (+ index 1)) 8)
             (ash (aref buf (+ index 2)) 16)
             (ash (aref buf (+ index 3)) 24)))
    (:big-endian
          (logior (ash (aref buf index) 24)
                  (ash (aref buf (+ index 1)) 16)
                  (ash (aref buf (+ index 2)) 8)
                  (aref buf (+ index 3))))))


(defun dbus-read-header (con)
  (let ((buf (make-array 12 :element-type '(unsigned-byte 8))))
    (assert (= 12 (read-sequence buf (stream-of con))))
    (let* ((*endianness*  (ecase (aref buf 0)
                            (108 :little-endian)
                            (66 :big-endian)))
           (msg-type (aref buf 1))
           (flags (aref buf 2))
           (protocol-version (aref buf 3))
           (body-length (read-uint32 buf 4))
           (serial (read-uint32 buf 8)))
      (assert (= protocol-version 1))
      (list *endianness* msg-type flags protocol-version body-length serial))))

(defun dbus-message (con)
  (let ((header (dbus-read-header con)))
    ;; XXX
    header))

(defun dbus-close (con)
  (close (stream-of con)))


;;; EOF
