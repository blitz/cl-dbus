;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

;;; TODO Extend usocket to support abstract sockets and rewrite this
;;; to use usocket.
#+(and sbcl linux)
(deftransport unix (address)
  (handler-case 
      (let ((abstract (server-address-value address "abstract"))
            (path (server-address-value address "path")))
        (let ((socket (make-instance (if abstract
                                         'sb-bsd-sockets:local-abstract-socket
                                         'sb-bsd-sockets:local-socket)
                                     :type :stream)))
          (sb-bsd-sockets:socket-connect socket (or abstract path))
          (sb-bsd-sockets:socket-make-stream socket 
                                             :element-type '(unsigned-byte 8)
                                             :input t
                                             :output t)))
    (sb-bsd-sockets:socket-error (c)
      (warn "Unable to connect to dbus via unix transport, because of ~A." c)
      nil)))

;;; EOF
