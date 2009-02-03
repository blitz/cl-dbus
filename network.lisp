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


(defun dbus-connect (&optional (address :session))
  "Returns a DBUS connection to the given ADDRESS. ADDRESS is either a
symbol (:SESSION or :SYSTEM) or a string containing a DBUS
address. Defaults to :SESSION."
  (let* ((success nil)
         (stream (connect-via-address-string
                  (etypecase address
                    (symbol (ecase address
                              (:session (sb-posix:getenv "DBUS_SESSION_BUS_ADDRESS"))))
                    (string address))))
         (con (make-instance 'dbus-connection
                             :stream stream)))
    (unwind-protect
         (progn
           ;; Send greeting (the NUL byte)
           (write-byte 0 stream)
           (force-output stream)
           ;; Now the ASCII authentication protocol can start.
           (let ((ascii-stream stream))
             ;; Check which authentication methods are accepted and try the
             ;; ones we support.
             (let ((methods (accepted-methods ascii-stream)))
               (or (when (find "DBUS_COOKIE_SHA1" methods :test #'string=)
                     (try-cookie-sha1-auth ascii-stream))
                   (when (find "ANONYMOUS" methods :test #'string=)
                     (try-anonymous-auth ascii-stream))
                   (error "Could not authenticate to server.")))
             (format-crlf ascii-stream "BEGIN")
             (force-output ascii-stream))
           (setq success t)
           con)
      ;; XXX
      ;; We need to call org.freedesktop.DBus.Hello after connecting to get
      ;; a name on the bus. This method takes no arguments and returns a
      ;; string.

      ;; Cleanup
      (unless success
        (dbus-close con)))))

;;; Method calls

(defvar *endianness* :little-endian)
(defvar *byte-counter*)

(defun dbus-read-alignment (stream alignment)
  "Skip bytes to achieve an alignment of ALIGNMENT bytes."
  (let ((mod (nth-value 1 (truncate *byte-counter* alignment))))
    (unless (zerop mod)
      (iter (repeat (- alignment mod))
            (dbus-read-byte stream)))))

(defun dbus-read-byte (stream)
  (incf *byte-counter* 1)
  (read-byte stream))

(defun dbus-read-uint16 (stream)
  (dbus-read-alignment stream 2)
  (ecase *endianness*
    (:little-endian
     (logior (dbus-read-byte stream)
             (ash (dbus-read-byte stream) 8)))
    (:big-endian
     (logior (ash (dbus-read-byte stream) 8)
             (dbus-read-byte stream)))))

(defun dbus-read-uint32 (stream)
  (dbus-read-alignment stream 4)
  (ecase *endianness*
    (:little-endian
     (logior (dbus-read-byte stream)
             (ash (dbus-read-byte stream) 8)
             (ash (dbus-read-byte stream) 16)
             (ash (dbus-read-byte stream) 24)))
    (:big-endian
          (logior (ash (dbus-read-byte stream) 24)
                  (ash (dbus-read-byte stream) 16)
                  (ash (dbus-read-byte stream) 8)
                  (dbus-read-byte stream)))))

(defun dbus-read-string (stream)
  (let ((length (dbus-read-uint32 stream)))
    (when (> length (* 1024 1024))
      (cerror "This is okay. Continue." "Incredibly large string (~,2F MB) in DBUS request."
              (/ length 1024 1024)))
    (let* ((buf (make-array length :element-type '(unsigned-byte 8)))
           (bytes (read-sequence buf stream)))
      (assert (= bytes length))
      (incf *byte-counter* bytes)
      (assert (zerop (dbus-read-byte stream)))
      (sb-ext:octets-to-string buf :external-format :utf8))))

(defun dbus-read-signature (stream)
  (let* ((length (dbus-read-byte stream))
         (buf (make-array length :element-type '(unsigned-byte 8)))
         (bytes (read-sequence buf stream)))
    (assert (= length bytes))
    (assert (zerop (dbus-read-byte stream)))
    (sb-ext:octets-to-string buf :external-format :utf8)))

;;; Message type
(defconstant +method-call+ 1)
(defconstant +method-return+ 2)
(defconstant +error+ 3)
(defconstant +signal+ 4)

;;; Header fields
(defconstant +path+ 1)
(defconstant +interface+ 2)
(defconstant +member+ 3)
(defconstant +error-name+ 4)
(defconstant +reply-serial+ 5)
(defconstant +destination+ 6)
(defconstant +sender+ 7)
(defconstant +signature+ 8)

;;; Header flags
(defconstant +no-reply-expected+ #x1)
(defconstant +no-auto-start+ #x2)

(defconstant +dbus-major-version+ 1)

(defun dbus-write-byte (stream byte)
  (declare (type (unsigned-byte 8) byte))
  (incf *byte-counter*)
  (write-byte byte stream))

(defun dbus-write-alignment (stream alignment)
  "Insert padding to achieve an alignment of ALIGNMENT bytes."
  (let ((mod (nth-value 1 (truncate *byte-counter* alignment))))
    (unless (zerop mod)
      (format t "Emitting ~A byte(s) alignment.~%" (- alignment mod))
      (iter (repeat (- alignment mod))
            (dbus-write-byte stream 0)))))


(defun dbus-write-uint32 (stream uint32)
  (declare (type (unsigned-byte 32) uint32))
  (format t "Emitting UINT32: #x~X~%"  uint32)
  (dbus-write-alignment stream 4)
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (ecase *endianness*
      (:little-endian
       (setf (aref buf 0) (logand uint32 #xFF)
             (aref buf 1) (logand (ash uint32 -8) #xFF)
             (aref buf 2) (logand (ash uint32 -16) #xFF)
             (aref buf 3) (logand (ash uint32 -24) #xFF)))
      (:big-endian
       (setf (aref buf 0) (logand (ash uint32 -24) #xFF)
             (aref buf 1) (logand (ash uint32 -16) #xFF)
             (aref buf 2) (logand (ash uint32 -8) #xFF)
             (aref buf 3) (logand uint32 #xFF))))
    (write-sequence buf stream)
    (incf *byte-counter* 4)))

(defun dbus-write-signature (stream sig)
  (format t "Emitting signature: ~A~%" sig)
  ;; XXX Merge with dbus-write-string?
  (let ((buf (flex:string-to-octets sig)))
    (dbus-write-byte stream (length buf))
    (write-sequence buf stream)
    (incf *byte-counter* (length buf))
    (dbus-write-byte stream 0)))

(defun dbus-write-string (stream string)
  (format t "Emitting string: ~A~%" string)
  (let ((buf (flex:string-to-octets string)))
    (dbus-write-uint32 stream (length buf))
    (write-sequence buf stream)
    (incf *byte-counter* (length buf))
    (dbus-write-byte stream 0)))

(defmacro dbus-with-array-write ((stream-var output-stream alignment) &body body)
  (let ((out-var (gensym "OUT-VAR"))
        (ali-var (gensym "ALI-VAR")))
    `(let ((,out-var ,output-stream)
           (,ali-var ,alignment)
           (buf (flexi-streams:with-output-to-sequence (,stream-var)
                  (let ((*byte-counter* 0))
                  ,@body))))
       (format t "BC before array = ~A~%" *byte-counter*)
       (dbus-write-uint32 ,out-var (length buf))
       (dbus-write-alignment stream ,ali-var)
       (format t "Writing array body with ~A bytes.~%" (length buf))
       (write-sequence buf ,out-var)
       (incf *byte-counter* (length buf))
       (format t "BC after array = ~A~%" *byte-counter*)
       )))

(defun dbus-marshal-to-buffer (signature arguments)
  (flexi-streams:with-output-to-sequence (out)
    (with-input-from-string (sig signature)
      )
    )
  )


; method call sender=:1.4 -> dest=org.freedesktop.DBus path=/org/freedesktop/DBus; interface=org.freedesktop.DBus; member=Hello


(defun dbus-method-call (con &key path interface member signature arguments
                         destination
                         (serial 1)
                         (reply-expected t)
                         (auto-start t))
  (assert (and member path))
  (let ((stream (stream-of con))
        (body (dbus-marshal-to-buffer signature arguments))
        (*byte-counter* 0))
    (dbus-write-byte stream (ecase *endianness*
                              (:little-endian (char-code #\l))
                              (:big-endian (char-code #\B))))
    (dbus-write-byte stream +method-call+)
    (dbus-write-byte stream (logior (if reply-expected 0 +no-reply-expected+)
                                    (if auto-start 0 +no-auto-start+)))
    (dbus-write-byte stream +dbus-major-version+)
    (dbus-write-uint32 stream (length body))
    (dbus-write-uint32 stream serial)
    (assert (= *byte-counter* 12))
    (dbus-with-array-write (array-stream stream 8)
      ;; PATH
      (format t "PATH (~A)~%" *byte-counter*)
      (dbus-write-byte array-stream +path+)
      (dbus-write-signature array-stream "o")
      (dbus-write-string array-stream path)
      ;; MEMBER
      (format t "MEMBER~%")
      (dbus-write-alignment array-stream 8)
      (dbus-write-byte array-stream +member+)
      (dbus-write-signature array-stream "s")
      (dbus-write-string array-stream member)
      ;; Interface
      (when interface
        (format t "INTERFACE~%")
        (dbus-write-alignment array-stream 8)
        (dbus-write-byte array-stream +interface+)
        (dbus-write-signature array-stream "s")
        (dbus-write-string array-stream interface))
      (when destination
        (format t "DESTINATION~%")
        (dbus-write-alignment array-stream 8)
        (dbus-write-byte array-stream +destination+)
        (dbus-write-signature array-stream "s")
        (dbus-write-string array-stream destination))
      ;; SIGNATURE
      (when (and signature (> (length signature) 0))
        (format t "SIGNATURE~%")
        (dbus-write-alignment array-stream 8)
        (dbus-write-byte array-stream +signature+)
        (dbus-write-signature array-stream "g")
        (dbus-write-signature array-stream signature)))

    (format t "BC = ~A~%" *byte-counter*)
    (dbus-write-alignment stream 8)
    (write-sequence body stream)
    (force-output stream)))

#+ ignore
(defun dbus-read-header (con)
  (let ((buf (make-array 12 :element-type '(unsigned-byte 8))))
    (assert (= 12 (read-sequence buf (stream-of con))))
    (let* ((*endianness*  (ecase (aref buf 0)
                            (108 :little-endian)
                            (66 :big-endian)))
           (msg-type (aref buf 1))
           (flags (aref buf 2))
           (protocol-version (aref buf 3))
           (body-length (dbus-read-uint32 buf 4))
           (serial (dbus-read-uint32 buf 8)))
      (assert (= protocol-version 1))
      (list *endianness* msg-type flags protocol-version body-length serial))))

(defun dbus-close (con)
  (close (stream-of con)))


;;; EOF
