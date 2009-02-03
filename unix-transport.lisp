;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;;
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

(deftransport unix (address)
  (handler-case
      (let ((abstract (server-address-value address "abstract"))
            (path (server-address-value address "path")))
        (iolib.sockets:make-socket
         :address-family :local
         :external-format :ascii
         :remote-filename (iolib.sockets:ensure-address
                           (or abstract path)
                           :family :local
                           :abstract (if abstract
                                         t
                                         nil))))
    (t (c)
      (warn "Unable to connect to to dbus via unix transport: ~A" c)
      nil)))

;;; EOF
