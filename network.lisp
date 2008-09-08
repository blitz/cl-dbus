;;; -*- Mode: Lisp -*-

(in-package :blitz.desktop.dbus)

(defclass* dbus-connection ()
  (stream))

(defun read-line-crlf (stream)
  "Read a line terminated with a CR/LF pair."
  (let ((line (read-line stream)))
    (assert (char= #\Return
                   (char line (1- (length line)))))
    ;; Using displaced arrays instead of subseq does not seem to offer
    ;; any advantages.
    (subseq line 0 (1- (length line)))))

(defun format-crlf (stream fmt &rest args)
  (format stream "~?~C~C" fmt args #\Return #\Newline))

(defun accepted-methods (stream)
  (format-crlf stream "AUTH")
  (force-output stream)
  (let* ((line (read-line-crlf stream))
         (splitted (cl-ppcre:split "\\s" line)))
    (assert (string= "REJECTED" (first splitted)))
    (rest splitted)))

(defun try-cookie-sha1-auth (stream)
  ;; XXX
  ;;(ironclad:digest-file nil nil )
  nil
  )

(defun try-anonymous-auth (stream)
  ;; XXX
  nil
  )

;;; XXX This probably needs a better name.
(defun connect-ascii (host port)
  "Returns an ASCII stream to the given `host' and `port'"
  (let* ((stream (usocket:socket-stream (usocket:socket-connect 
                                         host port :element-type '(unsigned-byte 8))))
         (con (make-instance 'dbus-connection
                             :stream stream)))
    ;; Send greeting (the NUL byte)
    (write-byte 0 stream)
    (force-output stream)
    ;; Now the ASCII authentication protocol can start.
    (let ((ascii-stream (flexi-streams:make-flexi-stream 
                         stream 
                         :external-format :ascii
                         :element-type 'character)))
      ;; We only support ANONYMOUS authentication (which is no
      ;; authentication at all). So at least check whether it's there.
      (let ((methods (accepted-methods ascii-stream)))
        (format t "Server supports authentication via:~{ ~A~}" methods)
        (or (try-cookie-sha1-auth ascii-stream)
            (try-anonymous-auth ascii-stream)
            (progn (close-dbus con)
                   (error "Could not authenticate to server.")))))
    con))


(defun close-dbus (con)
  (close (stream-of con)))




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

;;; EOF