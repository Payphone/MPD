;;; database.lisp

(in-package #:mpd)

;; The Music Database

(defun database-count (socket tag needle &key group-type)
  "Counts the number of songs and their total playtime in the db matching tag
  exactly."
  (response->plist
   (send-command socket "count" tag needle (if group-type "group" "")
                 (or group-type ""))))

(defun database-find (socket type what &key sort-type positions)
  "Finds songs in the db that are exactly WHAT. TYPE can be any supported tag,
  or one of the special parameters: any, file, base, or modified-since."
  (send-command socket "find" type what
                (if sort-type (concatenate 'string "sort " sort-type) "")
                (if positions (concatenate 'string "window " positions) "")))

(defun database-find-add (socket type what)
  "Finds songs in the db that are exactly WHAT and adds them to current
  playlist."
  (send-command socket "findadd" type what))

(defun database-list (socket type &key filter-type filter-what group-type)
  "Lists unique tags values of the specified type. TYPE can be any tag supported
  by MPD or file."
  (response->plist
   (send-command socket "list" type (or filter-type "") (or filter-what "")
                 (if group-type (concatenate 'string "group " group-type) ""))))

(defun database-list-all (socket uri)
  "Lists all songs and directories in URI."
  (response->plist
   (send-command socket "listall" uri)))

(defun database-list-all-info (socket uri)
  "Same as listall, except it also returns metadata info in the same format as
  lsinfo."
  (response->plist
   (send-command socket "listallinfo" uri)))

(defun database-list-files (socket uri)
  "Lists the contents of the directory URI, including files are not recognized
  by MPD."
  (response->plist
   (send-command socket "listfiles" (quote-string uri))))

(defun database-list-info (socket uri)
  "Lists the contents of the directory URI."
  (response->plist
   (send-command socket "lsinfo" uri)))

(defun database-read-comments (socket uri)
  "Read 'comments' (i.e. key-value pairs) from the file specified by 'URI'."
  (send-command socket "readcomments" uri))

(defun database-search (socket type what &key sort-type positions)
  "Searches for any song that contains WHAT."
  (send-command socket "search" type what
                (if sort-type (concatenate 'string "sort " sort-type) "")
                (if positions (concatenate 'string "window " positions) "")))

(defun database-search-add (socket type what)
  "Searches for any song that contains WHAT in tag TYPE and adds them to current
  playlist."
  (send-command socket "searchadd" type what))

(defun database-search-add-playlist (socket name type what)
  "Searches for any song that contains WHAT in tag TYPE and adds them to the
  playlist named NAME."
  (send-command socket "searchaddpl" name type what))

(defun database-update (socket uri)
  "Updates the music database: find new files, remove deleted files, update
  modified files."
  (send-command socket "update" uri))

(defun database-rescan (socket uri)
  "Same as update, but also rescans unmodified files."
  (send-command socket "rescan" uri))
