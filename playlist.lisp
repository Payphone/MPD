;;; playlist.lisp

(defpackage #:mpd.playlist
  (:use #:cl #:mpd.connection))

(in-package :mpd.playlist)

;; Current Playlist

(defun playlist-add (socket uri)
  "Adds the file URI to the playlist (directories add recursively). URI can also
  be a single file.")

(defun playlist-add-id (socket uri &optional position)
  "Adds a song to the playlist (non-recursive) and returns the song id.")

(defun playlist-clear (socket)
  "Clears the current playlist.")

(defun playlist-delete (socket &key position start end)
  "Deletes a song from the playlist.")

(defun playlist-delete-id (socket song-id)
  "Delets the song SONGID from the playlist.")

(defun playlist-move (socket to &key from start end)
  "Moves the song at FROM or range of songs at START:END to TO in the playlist.")

(defun playlist-move-id (socket from to)
  "Moves the song with FROM (songid) to TO (playlist index) in the playlist. If
  TO is negative, it is relative to the current song in the playlist (if there
  is one).")

(defun playlist (socket)
  "Displays the current playlist.")

(defun playlist-find (socket tag needle)
  "Finds songs in the current playlist with strict matching.")

(defun playlist-id (socket song-id)
  "Displays a list of songs in the playlist. SONGID is optional and specifies a
  single song to display info for.")

(defun playlist-info (socket &key position start end))

(defun playlist-search (socket tag needle)
  "Searches case-insensitively for partial matches in the current playlist.")

(defun playlist-changes (socket version &key start end))

(defun playlist-changes-position-id (socket version &key start end)
  "Displays changed songs currently in the playlist since VERSION. This
  function only returns the position and the id of the changed song, not the
  complete metadata. This is more bandwidth efficient.")

(defun playlist-priority (socket priority))

(defun playlist-priority-id (socket priority id)
  "Same as priority, but address the songs with their id.")

(defun playlist-range-id (socket id &key start end)
  "Specifies the portion of the song that shall be played. START and END are
  offsets in seconds (fractional seconds allowed); both are optional. Omitting
  both (i.e. sending just ':') means 'remove the range, play everything'. A song
  that is currently playing cannot be manipulated this way.")

(defun playlist-shuffle (socket &key start end)
  "Shuffles the current playlist. START:END is optional and specifies a range of
  songs.")

(defun playlist-swap (socket song1 song2)
  "Swaps the positions of song1 and song2.")

(defun playlist-swap-id (socket song1 song2)
  "Swaps the positions of song1 and song2 (both song ids).")

(defun playlist-add-tag-id (socket song-id tag value)
  "Adds a tag to the specified song. Editing song tags is only possible for
  remote songs. This change is volatile: it may be overwritten by tags received
  from the server, and the data is gone when the song gets removed from the
  queue.")

(defun playlist-clear-tag-id (socket song-id &optional tag)
  "Removes tags from the specified song. If TAG is not specified, then all tag
  values will be removed. Editing song tags is only possible for remote songs.")

;; Stored Playlists

(defun playlist-list-songs (socket name)
  "Lists the songs in the playlist. Playlist plugins are supported.")

(defun playlist-list-info (socket name)
  "Lists the songs with metadata in the playlist.")

(defun playlist-list (socket name)
  "Prints a list of the playlist directory.")

(defun playlist-load (socket name &optional start end)
  "Loads the playlist into the current queue. Playlist plugins are supported. A
  range may be specified to load only a part of the playlist.")

(defun playlist-add (socket name uri)
  "Adds URI to the playlist NAME.m3u.")

(defun playlist-clear (socket name)
  "Clears the playlist NAME.m3u.")

(defun playlist-delete (socket name song-position)
  "Deletes SONGPOS from the playlist NAME.m3u.")

(defun playlist-move (socket name from to)
  "Moves the song at position FROM in the playlist NAME.m3u to the position TO.")

(defun playlist-rename (socket name new-name)
  "Renames the playlist NAME.m3u to NEW_NAME.m3u.")

(defun playlist-remove (socket name)
  "Removes the playlist name.m3u to NEW_NAME.m3u.")

(defun playlist-save (socket name)
  "Saves the current playlist to NAME.m3u in the playlist directory.")
