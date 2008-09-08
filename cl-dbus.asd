;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ASDF -*-

(defsystem cl-dbus
  :components ((:file "packages")
               (:file "network" :depends-on ("packages")))
  :depends-on (iterate flexi-streams defclass-star usocket cl-ppcre
                       ironclad))

;;; EOF
