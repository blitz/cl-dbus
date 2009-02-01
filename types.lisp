;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

(defclass* dbus-type ()
  (name char alignment))

(defclass* dbus-compound-type (dbus-type)
  (element-type))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro define-dbus-type (name type-char alignment &optional (superclass 'dbus-type) slots)
    `(defclass* ,(intern (format nil "DBUS-~A" name) (find-package :blitz.desktop.dbus)) 
         (,superclass)
       ,slots
       (:default-initargs
           :name ',name
           :char ',type-char
           :alignment ,alignment))))

(define-dbus-type boolean #\b 4)

(define-dbus-type uint8 #\y 1)
(define-dbus-type int16 #\n 2)
(define-dbus-type uint16 #\q 2)
(define-dbus-type int32 #\i 4)
(define-dbus-type uint32 #\u 4)
(define-dbus-type int64 #\x 8)
(define-dbus-type uint64 #\t 8)
(define-dbus-type double #\d 8)

(define-dbus-type string #\s 4)
(define-dbus-type signature #\g 1)
(define-dbus-type object-path #\o 4)

(define-dbus-type variant #\v 1)

(define-dbus-type struct #\r 8 dbus-compound-type)
(define-dbus-type array #\a 4 dbus-compound-type)
(define-dbus-type dict-entry #\e 8 dbus-compound-type)

(defgeneric marshall (type object vector start)
  (:documentation "Marshalls OBJECT (interpreted as TYPE) into
  VECTOR (starting at START). Returns VECTOR and an index pointing
  after the written data."))

(defun dbus-write-byte (vector start byte &optional (repeat 1))
  (iter (repeat repeat)
        (for i upfrom 0)
        (setf (aref vector (+ start i)) byte)))

(defmethod marshall :around ((type dbus-type) object vector start)
  (let* ((alignment (alignment-of type))
         (mod (nth-value 1 (truncate start alignment))))
    (dbus-write-byte vector start 0 (- alignment mod))
    (call-next-method type object vector (+ start (- alignment mod)))))

(defmethod marshall ((type dbus-boolean) object vector start)
  ;; Endianness?
  ;; XXXX
  )
;;; EOF
