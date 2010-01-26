;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;;
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

(deftransport tcp (address)
  (handler-case
      (let ((host (server-address-value address "host"))
            (port (parse-integer (server-address-value address "port"))))
        (iolib.sockets:make-socket
         :remote-host host
         :remote-port port))
    (t (c)
      (warn "Unable to connect to to dbus via unix transport: ~A" c)
      nil)))
;;; EOF
