;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(defpackage :blitz.desktop.dbus
  (:use :common-lisp :iterate :defclass-star)
  (:import-from :cl-ppcre "SPLIT" "SCAN-TO-STRINGS")
  (:export))
(in-package :blitz.desktop.dbus)

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

;;; EOF
