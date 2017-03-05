;;;; mpd.asd

(asdf:defsystem #:mpd
  :description "A library for interacting with MPD."
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :depends-on (#:iolib #:split-sequence)
  :serial t
  :components ((:file "connection")
               (:file "playback")
               (:file "playlist")
               (:file "database")
               (:file "misc")
               (:file "package")))
