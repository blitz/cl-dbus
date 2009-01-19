;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

(defun try-to-send-hello ()
  (let ((con (dbus-connect "unix:path=/tmp/dbus-test,guid=7ec02b0af9c2b6e23f10880449051a5a")))
    (unwind-protect
         (dbus-method-call con
                           :path "/org/freedesktop/DBus"
                           :destination "org.freedesktop.DBus"
                           :interface "org.freedesktop.DBus"
                           :member "Hello"
                           :signature ""
                           :arguments ())
      (dbus-close con))))

;;; EOF
