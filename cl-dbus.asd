;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ASDF -*-

(defsystem cl-dbus
  :components ((:file "packages")
               (:file "transport" :depends-on ("packages"))
               (:file "unix-transport" :depends-on ("packages" "transport"))
               (:file "utilities" :depends-on ("packages"))
               (:file "cookie-sha1-auth" :depends-on ("packages" "utilities"))
               (:file "anonymous-auth" :depends-on ("packages" "utilities"))
               (:file "network" :depends-on ("packages" "cookie-sha1-auth" "anonymous-auth"
                                                        "transport")))
  :depends-on (iterate flexi-streams defclass-star usocket cl-ppcre
                       ironclad))

;;; EOF
