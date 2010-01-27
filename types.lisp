;;; -*- Mode: Lisp -*-
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; This file is part of CL-DBUS. Look into LICENSE for license terms.

(in-package :blitz.desktop.dbus)

(defvar *name-to-type* (make-hash-table))

(defstruct dbus-type
  name char alignment)

(defmethod print-object ((o dbus-type) s)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (o s :type t :identity nil)
        (format s "~A(~A)" (dbus-type-name o) (dbus-type-alignment o)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro define-dbus-type (name type-char alignment)
    `(setf (gethash ',type-char *name-to-type*)
           (make-dbus-type :name ',name
                           :char ',type-char
                           :alignment ',alignment))))

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

;; (define-dbus-type struct #\r 8 dbus-compound-type)
;; (define-dbus-type array #\a 4 dbus-compound-type)
;; (define-dbus-type dict-entry #\e 8 dbus-compound-type)

(defgeneric marshall (type object vector start)
  (:documentation "Marshalls OBJECT (interpreted as TYPE) into
  VECTOR (starting at START). Returns VECTOR and an index pointing
  after the written data. VECTOR is destructively modified!"))

(defun dbus-write-byte (vector byte start &optional (repeat 1))
  "Writes BYTE REPEAT-times into VECTOR starting at START. Returns the
new position in VECTOR."
  (iter (repeat repeat)
        (for i upfrom 0)
        (setf (aref vector (+ start i)) byte))
  (+ start repeat))

(defun dbus-write-sequence (destination source start)
  "Writes SOURCE into DESTINATION (both vectors) starting at
  START. Returns the new position in DESTINATION."
  (when (> (+ (length source) start)
           (length destination))
    (error "Vector too small."))
  (setf (subseq destination start) source)
  (+ start (length source)))

;; (defmethod marshall :around ((type dbus-type) object vector start)
;;   ;; Alignment is handled in this around method. 
;;   (let* ((alignment (alignment-of type))
;;          (mod (nth-value 1 (truncate start alignment))))
;;     (format t "~A ~A~%" alignment mod)
;;     (values 
;;      (call-next-method type object vector 
;;                        (dbus-write-byte vector 0 start 
;;                                         (mod (- alignment mod) alignment)))
;;      vector)))

;; (defmethod marshall ((type dbus-boolean) object vector start)
;;   (dbus-write-sequence vector
;;                        (if object
;;                            #+ little-endian #(1 0 0 0)
;;                            #+ big-endian #(0 0 0 1)
;;                            #(0 0 0 0))
;;                        start))

;;; TODO Parse signature string into list of dbus-type structures.
;;;      How to represent arrays and structs?

;;; TODO

;;; EOF
