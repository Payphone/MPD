;;;; package.lisp

(defpackage #:mpd
  (:use #:cl #:mpd.connection))

(defpackage #:mpd.connection
  (:use #:cl #:iolib)
  (:export #:response->plist
           #:initialize-connection
           #:close-connection
           #:receive-command
           #:OK
           #:send-command
           #:send-commands
           #:kill
           #:password
           #:ping
           #:tagtypes
           #:tagtypes-disable
           #:tagtypes-enable
           #:tagtypes-clear
           #:tagtypes-all
           #:idle
           #:clear-error))

(in-package :mpd.connection)
