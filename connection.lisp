;;; connection.lisp

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

(defun response->plist (response)
  "Creates a parameter list from a MPD response string. Since MPD returns items
  in the format 'type: content\n' it can be converted to a parameter list easily
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
    (connect socket (lookup-hostname address) :port port :wait t :timeout 5)
    (values socket (string-left-trim welcome (read-line socket)))))

(defun close-connection (socket)
  "Closes the connection for reading and writing."
  (send-command socket "close")
  (shutdown socket :write t :read t))

(defun receive-command (socket &key (include-ok t))
  "Reads from a socket until 'OK' or 'ACK' is reached. "
  (labels ((rec (socket acc)
             (let ((line (read-line socket nil)))
               (cond ((string= "OK" line)
                      (if include-ok
                          (if acc (values acc 'OK) 'OK)
                          acc))

                     ((string= "ACK" (subseq line 0 3)) (cons line acc))
                     (t (rec socket (cons line acc)))))))
    (rec socket nil)))

(defun send-command (socket command &key (include-ok t))
  "Sends a command to a socket and returns the reply."
  (format socket "~A~%" command)
  (force-output socket)
  (receive-command socket :include-ok include-ok))

(defun send-commands (socket &rest commands)
  "Sends a batch of commands and returns the reply."
  (format socket "command_list_begin~%~{~A~}~%command_list_end~%" commands)
  (force-output socket)
  (receive-command socket))

(defun kill (socket)
  "Kills MPD."
  (send-command socket "kill"))

(defun password (socket password)
  "This is used for authentication with the server. PASSWORD is simply the
  plaintext password."
  (send-command socket (format nil "password ~A" password)))

(defun ping (socket)
  "Returns 't' if successful ping."
  (send-command socket "ping" :include-ok t))

(defun tagtypes (socket)
  "Shows a list of available tag types."
  (response->plist (send-command socket "tagtypes")))

(defun tagtypes-disable (socket &rest tags)
  "Remove one or more tags from the list of tag types the client is interested
  in. These will be omitted from responses to this client."
  (send-command socket (format nil "tagtypes disable ~{~A ~}" tags)))

(defun tagtypes-enable (socket &rest tags)
  "Re-enable one or more tags from the list of tag types for this client. These
  will no longer be hidden from responses to this client."
  (send-command socket (format nil "tagtypes enable ~{~A ~}" tags)))

(defun tagtypes-clear (socket)
  "Clear the list of tag types this client is interested in. This means that MPD
  will not send any tags to this client."
  (send-command socket "tagtypes clear"))

(defun tagtypes-all (socket)
  "Announce that this client is interested in all tag types."
  (send-command socket "tagtypes all"))

(defun idle (socket &rest subsystems)
  "Waits until there is a noteworthy change on one or more of MPD's subsystems,
  and returns the change as a parameter list."
  (response->plist
   (send-command socket (format nil "idle ~{~A ~}" subsystems))))

(defun clear-error (socket)
  "Clears the current error message in status."
  (send-command socket "clearerror"))
