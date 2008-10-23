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
    ;; any advantages on SBCL.
    (subseq line 0 (1- (length line)))))

(defun format-crlf (stream fmt &rest args)
  (format t "~?~%" fmt args)
  (format stream "~?~C~C" fmt args #\Return #\Newline))

(defun read-line-and-split (stream)
  (cl-ppcre:split "\\s" (read-line-crlf stream)))

(defun accepted-methods (stream)
  (format-crlf stream "AUTH")
  (force-output stream)
  (destructuring-bind (response . rest)
      (read-line-and-split stream)
    (assert (string= "REJECTED" response))
    rest))

;;; Hex string handling

(defun octets-to-hex-string (octets)
  (string-downcase (format nil "~{~2,'0X~}" (coerce octets 'list))))

(defun string-to-hex-string (string)
  (octets-to-hex-string (sb-ext:string-to-octets string :external-format :ascii)))

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

;;; SHA1 cookie auth

(defun map-user-cookie-file (context fn)
  "Call fn with all cookies in the given DBUS context. `fn' is given
the cookie ID, the cookie creation time in seconds since the UNIX
epoch and the cookie itself (as string of hex digits)."
  (with-open-file (cookie-stream (merge-pathnames (make-pathname :directory '(:relative ".dbus-keyrings")
                                                                 :name context)
                                                  (user-homedir-pathname)))
    (loop 
       for line = (read-line cookie-stream nil nil)
       while line
       do (destructuring-bind (cookie-id-str cookie-creation-str cookie-hex-str)
              (cl-ppcre:split "\\s" line)
            (funcall fn 
                     (parse-integer cookie-id-str)
                     (parse-integer cookie-creation-str)
                     cookie-hex-str)))))

(defun random-challenge (&optional (length 16))
  (loop with array = (make-array length :element-type '(unsigned-byte 8))
       for i from 0 below length
       do (setf (aref array i) (random 256))
       finally (return array)))

(defun try-cookie-sha1-auth (stream)
  ;; Send the username we want to authenticate as.
  (let ((user (sb-ext:posix-getenv "USER")))
    (assert (stringp user) (user) "Couldn't find out username?! USER is not set.")
    (format-crlf stream "AUTH DBUS_COOKIE_SHA1 ~A"
                 (string-to-hex-string user)))
  (force-output stream)
  ;; The server sends the name of its "cookie context", a
  ;; space character; the integer ID of the secret cookie the client
  ;; must demonstrate knowledge of; a space character; then a
  ;; hex-encoded randomly-generated challenge string.
  (destructuring-bind (response &optional data)
      (read-line-and-split stream)
    (when (string/= response "DATA")
      (error "Got unexpected result: ~A ~A" response data))
    (destructuring-bind (context secret-cookie-id-str hex-challenge-str)
        (cl-ppcre:split "\\s" (sb-ext:octets-to-string (parse-hex-string data)
                                                       :external-format :ascii))
      (format t "Got: ~A ~A ~A~%" context secret-cookie-id-str hex-challenge-str)
      (let ((secret-cookie-id (parse-integer secret-cookie-id-str)))
        ;; Try to find the requested cookie.
        (let ((cookie-data-str (block data
                                 (map-user-cookie-file
                                  context
                                  (lambda (cookie-id creation-time cookie-data-str)
                                    (declare (ignore creation-time))
                                    (when (= cookie-id secret-cookie-id)
                                      (return-from data cookie-data-str))))
                                 (error "Invalid cookie id from server."))))
          ;; We've found our cookie. Now
          ;; digest and and be done with
          ;; it. :)
          (let ((my-challenge-str (octets-to-hex-string (random-challenge))))
            (let* ((hashed-str (format nil "~A:~A:~A"
                                       hex-challenge-str
                                       my-challenge-str
                                       cookie-data-str))
                   (digest (octets-to-hex-string (ironclad:digest-sequence 'ironclad:sha1
                                                                           (sb-ext:string-to-octets
                                                                            hashed-str)))))
              ;; Format our answer
              (format-crlf stream "DATA ~A20~A"
                           (to-hex-string my-challenge-str)
                           (string-to-hex-string digest)))
            (force-output stream))))))
  ;; Now check if we get a positive reply.
  (destructuring-bind (response . rest)
      (read-line-and-split stream)
    (cond
      ((string= response "OK")
       ;; Yay, we are authenticated. REST containts the hex-encoded
       ;; server GUID, but I guess we don't need that.
       t)
      ((string= response "REJECTED")
       (format t "~&DBUS_COOKIE_SHA1 authentication failed:~{ ~A~}" rest)
       nil)
      (t 
       (error "Unexpected reply from DBUS server: ~A~{ ~A~}" response rest)))))

;;; Anonymous auth

(defun try-anonymous-auth (stream)
  ;; XXX
  nil
  )

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
