;;; playlist.lisp

(in-package #:mpd)

;; Current Playlist

(defun playlist-add-song (socket uri)
  "Adds the file URI to the playlist (directories add recursively). URI can also
  be a single file."
  (send-command socket "add" (quote-string uri)))

(defun playlist-add-song-id (socket uri &optional position)
  "Adds a song to the playlist (non-recursive) and returns the song id."
  (send-command socket "addid" uri (or position "")))

(defun playlist-clear-songs (socket)
  "Clears the current playlist."
  (send-command socket "clear"))

(defun playlist-delete-song (socket position)
  "Deletes a song from the playlist. Position can either be a file or
  a range start:end."
  (send-command socket "delete" position))

(defun playlist-delete-song-id (socket song-id)
  "Deletes the song song-id from the playlist."
  (send-command socket "deleteid" song-id))

(defun playlist-move-song (socket from to)
  "Moves the song at FROM or range of songs at START:END to TO in the playlist."
  (send-command socket "move" from to))

(defun playlist-move-song-id (socket from to)
  "Moves the song with FROM (songid) to TO (playlist index) in the playlist. If
  TO is negative, it is relative to the current song in the playlist (if there
  is one)."
  (send-command socket "moveid" from to))

(defun playlist-song-list (socket)
  "Displays the current playlist."
  (send-command socket "playlist"))

(defun playlist-find-song (socket tag needle)
  "Finds songs in the current playlist with strict matching."
  (send-command socket "find" tag needle))

(defun playlist-song-id (socket song-id)
  "Displays a list of songs in the playlist. song-id is optional and specifies a
  single song to display info for."
  (send-command socket "playlistid" song-id))

(defun playlist-song-info (socket &optional position)
  "Displays a list of all songs in the playlist, or if the optional argument is
  given, displays information only for the song SONGPOS or the range of songs
  START:END."
  (response->plist (send-command socket "playlistinfo" (or position ""))))

(defun playlist-search-song (socket tag needle)
  "Searches case-insensitively for partial matches in the current playlist."
  (send-command socket "playlistsearch" tag needle))

(defun playlist-changes-song (socket version &optional positions)
  "Displays changed songs currently in the playlist since VERSION. Start and end
  positions may be given to limit the output to changes in the given range."
  (send-command socket "plchanges" version (or positions "")))

(defun playlist-changes-position-id (socket version &optional positions)
  "Displays changed songs currently in the playlist since VERSION. This
  function only returns the position and the id of the changed song, not the
  complete metadata. This is more bandwidth efficient."
  (send-command socket "plchangesposid" version (or positions "")))

(defun playlist-priority (socket priority &rest positions)
  (send-command socket "prio" priority (format nil "~{~A ~}" positions)))

(defun playlist-priority-id (socket priority &rest ids)
  "Same as priority, but address the songs with their id."
  (send-command socket "prioid" priority (format nil "~{~A ~}" ids)))

(defun playlist-range-id (socket id &rest positions)
  "Specifies the portion of the song that shall be played. START and END are
  offsets in seconds (fractional seconds allowed); both are optional. Omitting
  both (i.e. sending just ':') means 'remove the range, play everything'. A song
  that is currently playing cannot be manipulated this way."
  (send-command socket "rangeid" id (format nil "~{~A ~}" positions)))

(defun playlist-shuffle (socket &optional positions)
  "Shuffles the current playlist. START:END is optional and specifies a range of
  songs."
  (send-command socket "shuffle" (or positions "")))

(defun playlist-swap (socket song1 song2)
  "Swaps the positions of song1 and song2."
  (send-command socket "swap" song1 song2))

(defun playlist-swap-id (socket song1 song2)
  "Swaps the positions of song1 and song2 (both song ids)."
  (send-command socket "swapid" song1 song2))

(defun playlist-add-tag-id (socket song-id tag value)
  "Adds a tag to the specified song. Editing song tags is only possible for
  remote songs. This change is volatile: it may be overwritten by tags received
  from the server, and the data is gone when the song gets removed from the
  queue."
  (send-command socket "addtagid" song-id tag value))

(defun playlist-clear-tag-id (socket song-id &optional tag)
  "Removes tags from the specified song. If TAG is not specified, then all tag
  values will be removed. Editing song tags is only possible for remote songs."
  (send-command socket "cleartagid" song-id (or tag "")))

;; Stored Playlists

(defun playlist-list-songs (socket name)
  "Lists the songs in the playlist."
  (send-command socket "listplaylist" name))

(defun playlist-list-info (socket name)
  "Lists the songs with metadata in the playlist."
  (send-command socket "listplaylistinfo" name))

(defun playlist-list-playlists (socket name)
  "Prints a list of the playlist directory."
  (send-command socket "listplaylists" name))

(defun playlist-load (socket name &optional positions)
  "Loads the playlist into the current queue. Playlist plugins are supported. A
  range may be specified to load only a part of the playlist."
  (send-command socket "load" (quote-string name) (or positions "")))

(defun playlist-add (socket name uri)
  "Adds URI to the playlist NAME.m3u."
  (send-command socket "playlistadd" name uri))

(defun playlist-clear (socket name)
  "Clears the playlist NAME.m3u."
  (send-command socket "playlistclear" name))

(defun playlist-delete (socket name song-position)
  "Deletes song-position from the playlist NAME.m3u."
  (send-command socket "playlistdelete" name song-position))

(defun playlist-move (socket name from to)
  "Moves the song at position FROM in the playlist NAME.m3u to the position TO."
  (send-command socket "playlistmove" from to))

(defun playlist-rename (socket name new-name)
  "Renames the playlist NAME.m3u to NEW-NAME.m3u"
  (send-command socket "rename" name new-name))

(defun playlist-remove (socket name)
  "Removes the playlist name.m3u to NEW_NAME.m3u."
  (send-command socket "rm" name))

(defun playlist-save (socket name)
  "Saves the current playlist to NAME.m3u in the playlist directory."
  (send-command socket "save" name))
