;;;; package.lisp

(defpackage #:mpd
  (:use #:cl)
  (:export ;; Classes and their methods
           #:song
           #:filename
           #:last-modified
           #:artist
           #:album-artist
           #:title
           #:album
           #:track
           #:date
           #:composer
           #:performer
           #:comment
           #:genre
           #:disc
           #:current-time
           #:duration
           #:position
           #:id

           #:status
           #:volume
           #:repeatp
           #:randomp
           #:singlep
           #:consumep
           #:playlist
           #:playlist-length
           #:mix-ramp-db
           #:state
           #:song
           #:songid
           #:current-time
           #:elapsed
           #:bitrate
           #:duration
           #:audio
           #:next-song
           #:next-song-id

           #:statistics
           #:uptime
           #:play-time
           #:artists
           #:albums
           #:songs
           #:db-playtime
           #:db-update

           ;; General functions
           #:initialize-connection
           #:close-connection
           #:current-song
           #:status
           #:statistics))
