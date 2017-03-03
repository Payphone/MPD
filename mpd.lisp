;;;; mpd.lisp

(in-package #:mpd)

(defclass song ()
  ((filename :initarg :filename :accessor filename)
   (last-modified :initarg :last-modified :accessor last-modified)
   (artist :initarg :artist :accessor artist)
   (album-artist :initarg :album-artist :accessor album-artist)
   (title :initarg :title :accessor title)
   (album :initarg :album :accessor album)
   (track :initarg :track :accessor track)
   (date :initarg :date :accessor date)
   (composer :initarg :composer :accessor composer)
   (performer :initarg :performer :accessor performer)
   (comment :initarg :comment :accessor comment)
   (genre :initarg :genre :accessor genre)
   (disc :initarg :disc :accessor disc)
   (current-time :initarg :current-time :accessor current-time)
   (duration :initarg :duration :accessor duration)
   (position :initarg :position :accessor pos)
   (id :initarg :id :accessor id)))

(defclass status ()
  ((volume :initarg :volume :accessor volume)
   (repeatp :initarg :repeatp :accessor repeatp)
   (randomp :initarg :randomp :accessor randomp)
   (singlep :initarg :singlep :accessor singlep)
   (consumep :initarg :consumep :accessor consumep)
   (playlist :initarg :playlist :accessor playlist)
   (playlist-length :initarg :playlist-length :accessor playlist-length)
   (mix-ramp-db :initarg :mix-ramp-db :accessor mix-ramp-db)
   (state :initarg :state :accessor state)
   (song :initarg :song :accessor song)
   (songid :initarg :songid :accessor songid)
   (current-time :initarg :current-time :accessor current-time)
   (elapsed :initarg :elapsed :accessor elapsed)
   (bitrate :initarg :bitrate :accessor bitrate)
   (duration :initarg :duration :accessor duration)
   (audio :initarg :audio :accessor audio)
   (next-song :initarg :next-song :accessor next-song)
   (next-song-id :initarg :next-song-id :accessor next-song-id)))

(defclass statistics ()
  ((uptime :initarg :uptime :accessor uptime)
   (play-time :initarg :play-time :accessor play-time)
   (artists :initarg :artists :accessor artists)
   (albums :initarg :albums :accessor albums)
   (songs :initarg :songs :accessor songs)
   (db-playtime :initarg :db-playtime :accessor db-playtime)
   (db-update :initarg :db-update :accessor db-update)))

(defun response->plist (response)
  "Creates a parameter list from a MPD response string. Since MPD returns items
  in the format 'type: content' it can be converted to a parameter list easily
  by interning the type and storing content as a string."
  (loop for line in response
     for item = (split-sequence:split-sequence #\colon line)
     collect (intern (string-upcase (first item)))
     collect (string-left-trim '(#\Space) (second item))))

(defun initialize-connection (address port)
  "Connects to the MPD address and returns both the socket and the MPD version."
  (let ((socket (make-socket :connect :active
                             :address-family :internet
                             :type :stream
                             :external-format '(:utf-8 :eol-style :crlf)
                             :ipv6 nil))
        (welcome "OK MPD "))
    (connect socket (lookup-hostname address) :port port :wait t)
    (values socket (string-left-trim welcome (read-line socket)))))

(defun close-connection (socket)
  "Closes the connection."
  (shutdown socket :write t :read t))

(defun receive-command (socket)
  "Reads from a socket until 'OK' is reached."
  (loop for line = (read-line socket nil)
     until (and (length= line 2) (string= "OK" line))
     collect line))

(defun send-command (command socket)
  "Sends a command to a socket and return the reply."
  (format socket "~A~%" command)
  (force-output socket)
  (receive-command socket))

(defun current-song (socket)
  "Returns an instance of the current song."
  (let ((song (response->plist (send-command "currentsong" socket))))
    (make-instance 'song
                   :filename      (getf song 'file)
                   :last-modified (getf song 'last-modified)
                   :artist        (getf song 'artist)
                   :album-artist  (getf song 'albumartist)
                   :title         (getf song 'title)
                   :album         (getf song 'album)
                   :track         (getf song 'track)
                   :date          (getf song 'date)
                   :composer      (getf song 'composer)
                   :performer     (getf song 'performer)
                   :comment       (getf song 'comment)
                   :genre         (getf song 'genre)
                   :disc          (getf song 'disc)
                   :current-time  (getf song 'time)
                   :duration      (getf song 'duration)
                   :position      (getf song 'position)
                   :id            (getf song 'id))))

(defun status (socket)
  "Returns an instance of the current status."
  (let ((status (response->plist (send-command "status" socket))))
    (make-instance 'status
                   :volume          (getf status 'volume)
                   :repeatp         (getf status 'repeatp)
                   :randomp         (getf status 'randomp)
                   :singlep         (getf status 'singlep)
                   :consumep        (getf status 'consumep)
                   :playlist        (getf status 'playlist)
                   :playlist-length (getf status 'playlistlength)
                   :mix-ramp-db     (getf status 'mix-ramp-db)
                   :state           (getf status 'state)
                   :song            (getf status 'song)
                   :songid          (getf status 'songid)
                   :current-time    (getf status 'current-time)
                   :elapsed         (getf status 'elapsed)
                   :bitrate         (getf status 'bitrate)
                   :duration        (getf status 'duration)
                   :audio           (getf status 'audio)
                   :next-song       (getf status 'next-song)
                   :next-song-id    (getf status 'next-song-id))))

(defun statistics (socket)
  "Returns an instance of the current statistics."
  (let ((stats (response->plist (send-command "stats" socket))))
    (make-instance 'statistics
                   :uptime      (getf stats 'uptime)
                   :play-time   (getf stats 'play-time)
                   :artists     (getf stats 'artists)
                   :albums      (getf stats 'albums)
                   :songs       (getf stats 'songs)
                   :db-playtime (getf stats 'db-playtime)
                   :db-update   (getf stats 'db-update))))
