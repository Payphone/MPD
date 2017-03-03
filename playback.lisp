;;; playback.lisp

(in-package :mpd)

;; Playback Options

(defun playback-consume (socket state))
(defun playback-crossfade (socket seconds))
(defun playback-mixrampdb (socket decibels))
(defun playback-mixrampdelay (socket seconds))
(defun playback-random (socket state))
(defun playback-setvol (socket vol))
(defun playback-single (socket state))
(defun replay-gain-mode (socket mode))
(defun replay-gain-status (socket))

;; Controlling Playback

(defun binary-boolean (integer)
  (if (= integer 1) t))

(defun playback-next (socket)
  "Plays next song in the playlist.")
(defun pause/resume (socket)
  "Toggles pause/resumes playing, PAUSE is 0 or 1. ")
(defun playback-play (socket &optional songpos)
  "Begins playing the playlist at song number SONGPOS.")
(defun playback-play-id (socket &optional playid)
  "Begins playing the playlist at song SONGID.")
(defun playback-previous (socket)
  "Plays previous song in the playlist.")
(defun playback-seek (socket songpos time)
  "Seeks to the position TIME (in seconds; fractions allowed) of entry SONGPOS
  in the playlist.")
(defun playback-seek-id (socket songid time)
  "Seeks to the position TIME (in seconds; fractions allowed) of song SONGID.")
(defun playback-seek-current (socket time)
  "Seeks to the position TIME (in seconds; fractions allowed) within the current
  song. If prefixed by '+' or '-', then the time is relative to the current
  playing position.")
(defun playback-stop (socket)
  "Stops playing.")
