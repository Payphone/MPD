;;;; package.lisp

(defpackage #:mpd
  (:use #:cl #:iolib)
  (:export ;; Connection Settings
           #:response->plist
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
           #:clear-error

           ;; Playback
           #:playback-consume
           #:playback-crossfade
           #:playback-mixrampdb
           #:playback-mixrampdelay
           #:playback-random
           #:playback-set-volume
           #:playback-single
           #:replay-gain-mode
           #:replay-gain-status
           #:playback-next
           #:playback-pause
           #:playback-play
           #:playback-play-id
           #:playback-previous
           #:playback-seek
           #:playback-seek-id
           #:playback-seek-current
           #:playback-stop

           ;; Playlist
           #:playlist-add-song
           #:playlist-add-song-id
           #:playlist-clear-songs
           #:playlist-delete-song
           #:playlist-delete-song-id
           #:playlist-move
           #:playlist-move-song-id
           #:playlist-song-list
           #:playlist-find-song
           #:playlist-song-id
           #:playlist-song-info
           #:playlist-search-song
           #:playlist-changes-song
           #:playlist-changes-position-id
           #:playlist-priority
           #:playlist-priority-id
           #:playlist-range-id
           #:playlist-shuffle
           #:playlist-swap
           #:playlist-swap-id
           #:playlist-add-tag-id
           #:playlist-clear-tag-id
           #:playlist-list-songs
           #:playlist-list-info
           #:playlist-list-playlists
           #:playlist-load
           #:playlist-add
           #:playlist-clear
           #:playlist-delete
           #:playlist-move
           #:playlist-rename
           #:playlist-remove
           #:playlist-save

           ;; Database
           #:database-count
           #:database-find
           #:database-find-add
           #:database-list
           #:database-list-all
           #:database-list-all-info
           #:database-list-files
           #:database-list-info
           #:database-read-comments
           #:database-search
           #:database-search-add
           #:database-search-add-playlist
           #:database-update
           #:database-rescan

           ;; Query
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
           #:query-song

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
           #:song-id
           #:current-time
           #:elapsed
           #:bitrate
           #:duration
           #:audio
           #:next-song
           #:next-song-id
           #:query-status

           #:statistics
           #:uptime
           #:play-time
           #:artist
           #:albums
           #:songs
           #:db-playtime
           #:db-update
           #:query-statistics

           ;; Misc
           #:mount
           #:unmount
           #:mount-list
           #:neighbor-list
           #:sticker-get
           #:sticker-set
           #:sticker-delete
           #:sticker-list
           #:sticker-find
           #:partition-list
           #:partition-new
           #:output-disable
           #:output-enable
           #:output-toggle
           #:output-list
           #:internal-config
           #:internal-command-list
           #:internal-not-command-list
           #:internal-url-handler-list
           #:internal-decoder-list
           #:client-subscribe
           #:clibent-unsubscribe
           #:client-channel-list
           #:client-read-message-list
           #:client-send-message))
