;;; misc.lisp

(in-package #:mpd)

;; Mounts and Neighbors

(defun mount (socket path uri)
  "Mount the specified remote storage URI at the given path."
  (send-command socket "mount" path uri))

(defun unmount (socket path)
  "Unmounts the specified path."
  (send-command socket "unmount" path))

(defun mount-list (socket)
  "Queries a list of all mounts."
  (response->plist (send-command socket "listmounts")))

(defun neighbor-list (socket)
  "Queries a list of 'neighbors'."
  (response->plist (send-command socket "listneighbors")))

;; Stickers

(defun sticker-get (socket type uri name)
  "Reads a sticker value for the specified object."
  (send-command socket "sticker get" type (quote-string uri) name))

(defun sticker-set (socket type uri name value)
  "Adds a sticker value to the specified object."
  (send-command socket "sticker set" type (quote-string uri) name value))

(defun sticker-delete (socket type uri name)
  "Deletes a sticker value from the specified object."
  (send-command socket "sticker delete" type (quote-string uri) name))

(defun sticker-list (socket type uri)
  "Lists the stickers for the specified object."
  (send-command socket "sticker list" type (quote-string uri)))

(defun sticker-find (socket type uri name &optional value)
  "Searches the sticker database for stickers with the specified name, below the
  specified directory."
  (if value
      (send-command socket "sticker find" (quote-string uri) name "=" value)
      (send-command socket "sticker find" (quote-string uri) name)))

;; Partition Commands

(defun partition-list (socket)
  "Print a list of partitions."
  (response->plist (send-command socket "listpartitions")))

(defun partition-new (socket name)
  "Create a new partition."
  (send-command socket "newpartition" name))

;; Audio Output Devices

(defun output-disable (socket id)
  "Turns an output off."
  (send-command socket "disableoutput" id))

(defun output-enable (socket id)
  "Turns an output on."
  (send-command socket "enableoutput" "id"))

(defun output-toggle (socket id)
  "Turns an output on or off, depending on the current state."
  (send-command socket "toggleoutput" id))

(defun output-list (socket)
  "Shows information about all outputs."
  (response->plist (send-command socket "outputs")))

(defun internal-config (socket)
  "Dumps configuration values that may be interesting for the client."
  (send-command socket "config"))

(defun internal-command-list (socket)
  "Shows which commands the current user has access to."
  (send-command socket "commands"))

(defun internal-not-command-list (socket)
  "Shows which commands the current user does not have access to"
  (send-command socket "notcommands"))

(defun internal-url-handler-list (socket)
  "Gets a list of available URL handlers."
  (send-command socket "urlhandlers"))

(defun internal-decoder-list (socket)
  "Print a list of decoder plugins, followed by their support suffixes and MIME
  types."
  (send-command socket "decoders"))

;; Client to Client

(defun client-subscribe (socket name)
  "Subscribe to a channel."
  (send-command "subscribe" name))

(defun client-unsubscribe (socket name)
  "Unsubscribe from a channel."
  (send-command "unsubscribe" name))

(defun client-channel-list (socket)
  "Obtain a list of all channels."
  (send-command socket "channels"))

(defun client-read-message-list (socket)
  "Reads messages for this client. The response is a list of 'channel:' and
  'message:' lines."
  (send-command socket "readmessages"))

(defun client-send-message (socket channel text)
  "Send a message to the specified channel."
  (send-command socket "sendmessage" channel text))
