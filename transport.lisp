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
  (cdr (find key (server-address-values address) :key #'car :test #'string=)))

(defun unescape (string)
  "String is an ASCII string with every non-ASCII (and some other)
chars escaped by URI-style %XX sequences, with XX being
hex-digits. Returns the unescaped string."
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
  "Parse a DBUS address key-value pair (foo=bar) taking care of the
strange escaping rules described in the standard. Returns a cons of
two strings."
  (iter (for part in (split "," kv-string))
        (multiple-value-bind (match? matches)
            (cl-ppcre:scan-to-strings "^([^=]+)=([^=]*)$" part)
          (unless match?
            (error "Malformed key-value pair: ~S" part))
          (collect (cons (unescape (aref matches 0))
                         (unescape (aref matches 1)))))))

(defun server-address-from-string (string)
  "Parse a single server address and return a corresponding
SERVER-ADDRES structure."
  (multiple-value-bind (match? matches)
      (cl-ppcre:scan-to-strings "^([^:]+):(.*)$" string)
    (unless match?
      (error "Malformed DBUS server address ~S" string))
    (make-server-address :type (aref matches 0)
                         :values (parse-key-value-string (aref matches 1)))))

(defun parse-server-address-list (string)
  "Parse a string as given in the environment variable
DBUS_SESSION_BUS_ADDRESS which can be a list of several server
addresses. Returns a list of SERVER-ADDRESS structures."
  (mapcar #'server-address-from-string (split ";" string)))

;;; Run-time selection of transports

(defvar *transports* nil
  "List of supported transports.")

(defun register-transport (name fn)
  (setf *transports* (cons (cons name fn)
                           (remove name *transports* :key #'car :test #'string=))))

(defmacro deftransport (name (address) &body body)
  "Register a transport for NAME. ADDRESS is the name of a
  SERVER-ADDRESS structure. BODY should be code that takes the
  SERVER-ADDRESS and returns a stream or NIL."
  `(register-transport ,(etypecase
                         name
                         (symbol (string-downcase (string name)))
                         (string name))
                       (lambda (,address)
                         ,@body)))

;;; Now finally the function that wraps all of this.

(defun connect-via-address-string (string)
  "Takes a string containing a DBUS server address (or multiple) and
  returns a stream to the bus."
  (or (iter (for address in (parse-server-address-list string))
            (for transport-connector = (find (server-address-type address) *transports*
                                             :key #'car :test #'string=))
            (if transport-connector
                (thereis (funcall (cdr transport-connector) address))
                (warn "Unknown connection type: ~A" (server-address-type address))))
      (error "Unable to connect to DBUS.")))

;;; EOF
