;;;; mpd.asd

(asdf:defsystem #:mpd
  :description "A library for interacting with MPD."
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :depends-on (#:iolib #:split-sequence #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "connection")
               (:file "playback")
               (:file "playlist")
               (:file "database")
               (:file "query")
               (:file "misc")))
