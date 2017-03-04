;;; playback.lisp

(defpackage #:mpd.playback
  (:use #:cl #:mpd.connection)
  (:export #:playback-consume
           #:playback-crossfade
           #:playback-mixrampdb
           #:playback-mixrampdelay
           #:playback-random
           #:playback-setvol
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
           #:playback-stop))

(in-package :mpd.playback)

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
  (send-command socket (format nil "consume ~A" (boolean->binary state))))

(defun playback-crossfade (socket seconds)
  "Sets crossfading between songs in seconds."
  (send-command socket (format nil "crossfade ~A" seconds)))

(defun playback-mixrampdb (socket decibels)
  "Sets the threshold at which songs will be overlapped."
  (send-command socket (format nil "mixrampdb ~A" decibels)))

(defun playback-mixrampdelay (socket seconds)
  "Additional time subtracted from the overlap calculated by mixrampdb."
  (send-command socket (format nil "mixrampdelay ~A" seconds)))

(defun playback-random (socket state)
  "Sets the random state to t or nil."
  (send-command socket (format nil "random ~A" (boolean->binary state))))

(defun playback-setvol (socket volume)
  "Sets the volume, the range of volume is 0-100."
  (if (and (>= volume 0) (<= volume 100))
      (send-command socket (format nil "random ~A" volume))
      (error "Invalid volume ~A, must an integer between 1 and 100." volume)))

(defun playback-single (socket state)
  "Sets the single state to t or nil."
  (send-command socket (format nil "single ~A" (boolean->binary state))))

(defun replay-gain-mode (socket gain-mode)
  "Sets the replay gain mode. Mode must be one of off, track, ablum, or auto."
  (let ((modes '(off track album auto))
        (mode (string-downcase gain-mode)))
    (if (member mode modes)
        (send-command socket (format nil "replay_gain_mode ~A" mode))
        (error "Invalid gain mode ~A, must be one of ~A" mode modes))))

(defun replay-gain-status (socket)
  "Prints replay gain options."
  (response->plist (send-command socket "replay_gain_status")))

;; Controlling Playback

(defun playback-next (socket)
  "Plays the next song in the playlist."
  (send-command socket "next"))

(defun playback-pause (socket state)
  "Sets the pause state to t or nil"
  (send-command socket (format nil "pause ~A" (boolean->binary state))))

(defun playback-play (socket &optional song-position)
  "Begins playing the playlist at song number song-position."
  (send-command socket (format nil "play ~A"(or song-position ""))))

(defun playback-play-id (socket &optional play-id)
  "Begins playing the playlist at song SONGID."
  (send-command socket (format nil "playid ~A" (or play-id ""))))

(defun playback-previous (socket)
  "Plays previous song in the playlist."
  (send-command socket "previous"))

(defun playback-seek (socket song-position time)
  "Seeks to the position time (in seconds) of entry song-position in the
  playlist."
  (send-command socket "seek ~A ~A" song-position time))

(defun playback-seek-id (socket song-id time)
  "Seeks to the position time (in seconds) of song song-id."
  (send-command socket "seekid ~A ~A" song-id time))

(defun playback-seek-current (socket time)
  "Seeks to the position time (in seconds) within the current song. If prefixed
  by '+' or '-', then the time is relative to the current playing position."
  (send-command socket "seekcur ~A" time))

(defun playback-stop (socket)
  "Stops playing."
  (send-command socket "stop"))
