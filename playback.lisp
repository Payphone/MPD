;;; playback.lisp

(in-package #:mpd)

;; Utilties
;; MPD uses 1 and 0 to represent booleans

(defun binary->boolean (integer)
  "Converts a 1 or 0 to t or nil respectively."
  (if (= integer 1) t))

(defun boolean->binary (boolean)
  "Converts a t or nil to 1 or 0 respectively."
  (if boolean 1 0))

;; Playback Options

(defun playback-consume (socket state)
  "Sets the consume state, either t or nil."
  (declare (type boolean state))
  (send-command socket "consume" (boolean->binary (the boolean state))))

(defun playback-crossfade (socket seconds)
  "Sets crossfading between songs in seconds."
  (declare (type unsigned-byte seconds))
  (send-command socket "crossfade" (the unsigned-byte seconds)))

(defun playback-mixrampdb (socket decibels)
  "Sets the threshold at which songs will be overlapped."
  (declare (type signed-byte decibels))
  (send-command socket "mixrampdb" (the signed-byte decibels)))

(defun playback-mixrampdelay (socket seconds)
  "Additional time subtracted from the overlap calculated by mixrampdb."
  (declare (type unsigned-byte seconds))
  (send-command socket "mixrampdelay" (the unsigned-byte seconds)))

(defun playback-random (socket state)
  "Sets the random state to t or nil."
  (declare (type boolean state))
  (send-command socket "random" (boolean->binary (the boolean state))))

(defun playback-set-volume (socket volume)
  "Sets the volume, the range of volume is 0-100."
  (declare (type signed-byte volume))
  (send-command socket "setvol" (the signed-byte volume)))

(defun playback-single (socket state)
  "Sets the single state to t or nil."
  (declare (type boolean state))
  (send-command socket "single" (boolean->binary (the boolean state))))

(defun replay-gain-mode (socket gain-mode)
  "Sets the replay gain mode. Mode must be one of off, track, ablum, or auto."
  (send-command socket "replay_gain_mode" mode))

(defun replay-gain-status (socket)
  "Prints replay gain options."
  (response->plist (send-command socket "replay_gain_status")))

;; Controlling Playback

(defun playback-next (socket)
  "Plays the next song in the playlist."
  (send-command socket "next"))

(defun playback-pause (socket state)
  "Sets the pause state to t or nil"
  (declare (type boolean state))
  (send-command socket "pause" (boolean->binary (the boolean state))))

(defun playback-play (socket &optional song-position)
  "Begins playing the playlist at song number song-position."
  (send-command socket "play" (or song-position "")))

(defun playback-play-id (socket &optional play-id)
  "Begins playing the playlist at song SONGID."
  (send-command socket "playid" (or play-id "")))

(defun playback-previous (socket)
  "Plays previous song in the playlist."
  (send-command socket "previous"))

(defun playback-seek (socket song-position time)
  "Seeks to the position time (in seconds) of entry song-position in the
  playlist."
  (declare (type unsigned-byte song-position time))
  (send-command socket "seek" (the unsigned-byte song-position)
                (the unsigned-byte time)))

(defun playback-seek-id (socket song-id time)
  "Seeks to the position time (in seconds) of song song-id."
  (declare (type unsigned-byte song-id time))
  (send-command socket "seekid" (the unsigned-byte song-id)
                (the unsigned-byte time)))

(defun playback-seek-current (socket time)
  "Seeks to the position time (in seconds) within the current song. If prefixed
  by '+' or '-', then the time is relative to the current playing position."
  (declare (type unsigned-byte time))
  (send-command socket "seekcur" (the unsigned-byte time)))

(defun playback-stop (socket)
  "Stops playing."
  (send-command socket "stop"))
