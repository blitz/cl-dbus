;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

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
  (let ((user (file-author (user-homedir-pathname))))
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
        (cl-ppcre:split "\\s" (babel:octets-to-string (parse-hex-string data)
                                                       :encoding :ascii))
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
                   (digest (octets-to-hex-string (ironclad:digest-sequence :sha1
                                                                           (babel:string-to-octets
                                                                            hashed-str)))))
              ;; Format our answer
              (format-crlf stream "DATA ~A20~A"
                           (string-to-hex-string my-challenge-str)
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

;;; EOF
