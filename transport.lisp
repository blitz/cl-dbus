;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

;;; Parsing the server address

(defstruct server-address
  type
  values)

(defun server-address-value (address key)
  (gethash key (server-address-values address)))

(defun unescape (string)
  "String is an ASCII string with every non-ASCII (and some other)
chars escaped by URI-style %XX sequences, with XX being hex-digits."
  (with-input-from-string (in string)
    (iter (with output = (make-array 0 
                                     :element-type '(unsigned-byte 8)
                                     :fill-pointer t
                                     :adjustable t))
          (for char = (read-char in nil nil))
          (while char)
          (case char
            (#\% (let ((digit1 (digit-char-p (read-char in) 16))
                       (digit2 (digit-char-p (read-char in) 16)))
                   (assert (and digit1 digit2)
                           (digit1 digit2)
                           "Got non-digits (~A ~A) after escape char..." digit1 digit2)
                   (vector-push-extend (logior (ash digit1 4) digit2) output)))
            (t (vector-push-extend (char-code char) output)))
          (finally (return (sb-ext:octets-to-string output :external-format :utf8))))))

(defun parse-key-value-string (kv-string)
  (iter (for part in (split "," kv-string))
        (multiple-value-bind (match? matches)
            (cl-ppcre:scan-to-strings "^([^=]+)=([^=]*)$" part)
          (unless match?
            (error "Malformed key-value pair: ~S" part))
          (collect (cons (unescape (aref matches 0))
                         (unescape (aref matches 1)))))))

(defun server-address-from-string (string)
  (multiple-value-bind (match? matches)
      (cl-ppcre:scan-to-strings "^([^:]+):(.*)$" string)
    (unless match?
      (error "Malformed DBUS server address ~S" string))
    (make-server-address :type (aref matches 0)
                         :values (parse-key-value-string (aref matches 1)))))

(defun parse-server-address-list (string)
  (mapcar #'server-address-from-string
          (split ";" string)))

(defvar *transports* nil
  "List of fu")

;;; EOF
