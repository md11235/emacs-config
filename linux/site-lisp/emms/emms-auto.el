;;; -*-emacs-lisp-*-

(defvar generated-autoload-file)
(defvar command-line-args-left)
(defun generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

(provide 'emms-auto)
;;; Generated autoloads follow (made by autoload.el).

;;;### (autoloads (emms-lyrics-toggle emms-lyrics-disable emms-lyrics-enable)
;;;;;;  "emms-lyrics" "emms-lyrics.el" (17696 30671))
;;; Generated autoloads from emms-lyrics.el

(autoload (quote emms-lyrics-enable) "emms-lyrics" "\
Enable displaying emms lyrics.

\(fn)" t nil)

(autoload (quote emms-lyrics-disable) "emms-lyrics" "\
Disable displaying emms lyrics.

\(fn)" t nil)

(autoload (quote emms-lyrics-toggle) "emms-lyrics" "\
Toggle displaying emms lyrics.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-playing-time-toggle emms-mode-line-disable
;;;;;;  emms-mode-line-enable) "emms-mode-line" "emms-mode-line.el"
;;;;;;  (17544 56145))
;;; Generated autoloads from emms-mode-line.el

(autoload (quote emms-mode-line-enable) "emms-mode-line" "\
Turn on `emms-mode-line'.

\(fn)" t nil)

(autoload (quote emms-mode-line-disable) "emms-mode-line" "\
Turn off `emms-mode-line'.

\(fn)" t nil)

(autoload (quote emms-playing-time-toggle) "emms-mode-line" "\
Toggle `emms-mode-line'.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-player-mpd-show emms-player-mpd-connect)
;;;;;;  "emms-player-mpd" "emms-player-mpd.el" (17733 28639))
;;; Generated autoloads from emms-player-mpd.el

(autoload (quote emms-player-mpd-connect) "emms-player-mpd" "\
Connect to MusicPD and retrieve its current playlist.

Afterward, the status of MusicPD will be tracked.

This also has the effect of changing the current EMMS playlist to
be the same as the curreent MusicPD playlist.  Thus, this
function is useful to call if the contents of the EMMS playlist
buffer get out-of-sync for some reason.

\(fn)" t nil)

(autoload (quote emms-player-mpd-show) "emms-player-mpd" "\
Describe the current EMMS track in the minibuffer.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description.

This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS.

\(fn &optional INSERTP CALLBACK)" t nil)

;;;***

;;;### (autoloads (emms-playing-time-toggle emms-playing-time-disable
;;;;;;  emms-playing-time-enable) "emms-playing-time" "emms-playing-time.el"
;;;;;;  (17724 10064))
;;; Generated autoloads from emms-playing-time.el

(autoload (quote emms-playing-time-enable) "emms-playing-time" "\
Enable displaying emms playing time on mode line.

\(fn)" t nil)

(autoload (quote emms-playing-time-disable) "emms-playing-time" "\
Disable displaying emms playing time on mode line.

\(fn)" t nil)

(autoload (quote emms-playing-time-toggle) "emms-playing-time" "\
Toggle displaying emms playing time on mode line.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-playlist-mode) "emms-playlist-mode" "emms-playlist-mode.el"
;;;;;;  (17724 10064))
;;; Generated autoloads from emms-playlist-mode.el

(autoload (quote emms-playlist-mode) "emms-playlist-mode" "\
A major mode for Emms playlists.
\\{emms-playlist-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-score-toggle emms-score-disable emms-score-enable)
;;;;;;  "emms-score" "emms-score.el" (17637 9295))
;;; Generated autoloads from emms-score.el

(autoload (quote emms-score-enable) "emms-score" "\
Turn on emms-score.

\(fn)" t nil)

(autoload (quote emms-score-disable) "emms-score" "\
Turn off emms-score.

\(fn)" t nil)

(autoload (quote emms-score-toggle) "emms-score" "\
Toggle emms-score.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-default-players emms-devel emms-all emms-standard
;;;;;;  emms-minimalistic) "emms-setup" "emms-setup.el" (17692 33614))
;;; Generated autoloads from emms-setup.el

(autoload (quote emms-minimalistic) "emms-setup" "\
An Emms setup script.
Invisible playlists and all the basics for playing media.

\(fn)" nil nil)

(autoload (quote emms-standard) "emms-setup" "\
An Emms setup script.
Everything included in the `emms-minimalistic' setup, the Emms
interactive playlist mode, reading information from tagged
audio files, and a metadata cache.

\(fn)" nil nil)

(autoload (quote emms-all) "emms-setup" "\
An Emms setup script.
Everything included in the `emms-standard' setup and adds all the
stable features which come with the Emms distribution.

\(fn)" nil nil)

(autoload (quote emms-devel) "emms-setup" "\
An Emms setup script.
Everything included in the `emms-all' setup and adds all the
features which come with the Emms distribution regardless of if
they are considered stable or not.  Use this if you like living
on the edge.

\(fn)" nil nil)

(autoload (quote emms-default-players) "emms-setup" "\
Set `emms-player-list' to `emms-setup-default-player-list'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (emms-locate emms-source-file-regex emms-source-file-directory-tree)
;;;;;;  "emms-source-file" "emms-source-file.el" (17631 56910))
;;; Generated autoloads from emms-source-file.el
 (autoload 'emms-play-file "emms-source-file" nil t)
 (autoload 'emms-add-file "emms-source-file" nil t)
 (autoload 'emms-play-directory "emms-source-file" nil t)
 (autoload 'emms-add-directory "emms-source-file" nil t)
 (autoload 'emms-play-directory-tree "emms-source-file" nil t)
 (autoload 'emms-add-directory-tree "emms-source-file" nil t)
 (autoload 'emms-play-find "emms-source-file" nil t)
 (autoload 'emms-add-find "emms-source-file" nil t)
 (autoload 'emms-play-dired "emms-source-file" nil t)
 (autoload 'emms-add-dired "emms-source-file" nil t)

(autoload (quote emms-source-file-directory-tree) "emms-source-file" "\
Return a list of all files under DIR that match REGEX.
This function uses `emms-source-file-directory-tree-function'.

\(fn DIR REGEX)" nil nil)

(autoload (quote emms-source-file-regex) "emms-source-file" "\
Return a regexp that matches everything any player (that supports
files) can play.

\(fn)" nil nil)

(autoload (quote emms-locate) "emms-source-file" "\
Search for REGEXP and display the results in a locate buffer

\(fn REGEXP)" t nil)
 (autoload 'emms-play-url "emms-source-file" nil t)
 (autoload 'emms-add-url "emms-source-file" nil t)
 (autoload 'emms-play-streamlist "emms-source-file" nil t)
 (autoload 'emms-add-streamlist "emms-source-file" nil t)

;;;***

;;;### (autoloads nil "emms-source-playlist" "emms-source-playlist.el"
;;;;;;  (17724 10064))
;;; Generated autoloads from emms-source-playlist.el
 (autoload 'emms-play-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory-tree
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory-tree
          "emms-source-file" nil t)

;;;***

;;;### (autoloads (emms-streams) "emms-streams" "emms-streams.el"
;;;;;;  (17615 25856))
;;; Generated autoloads from emms-streams.el

(autoload (quote emms-streams) "emms-streams" "\
Opens the EMMS Streams interface.

\(fn)" t nil)

;;;***

;;;### (autoloads (oggc-show-header) "ogg-comment" "ogg-comment.el"
;;;;;;  (17544 16217))
;;; Generated autoloads from ogg-comment.el

(autoload (quote oggc-show-header) "ogg-comment" "\
Show a pretty printed representation of the Ogg Comments in FILE.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (tq-create) "tq" "tq.el" (17655 39246))
;;; Generated autoloads from tq.el

(autoload (quote tq-create) "tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine.

\(fn PROCESS)" nil nil)

;;;***

;;;### (autoloads nil nil ("emms-bookmarks.el" "emms-browser.el"
;;;;;;  "emms-info-libtag.el" "emms-info-mp3info.el" "emms-info-ogg.el"
;;;;;;  "emms-info-ogginfo.el" "emms-info.el" "emms-last-played.el"
;;;;;;  "emms-maint.el" "emms-metaplaylist-mode.el" "emms-mode-line-icon.el"
;;;;;;  "emms-player-mpg321-remote.el" "emms-player-mplayer.el" "emms-player-simple.el"
;;;;;;  "emms-playlist-sort.el" "emms-stream-info.el" "emms-volume-amixer.el"
;;;;;;  "emms-volume.el" "emms.el" "jack.el" "later-do.el") (17739
;;;;;;  63778 683362))

;;;***

;;;### (autoloads (emms-cache-toggle emms-cache-disable emms-cache-enable)
;;;;;;  "emms-cache" "emms-cache.el" (17692 33614))
;;; Generated autoloads from emms-cache.el

(autoload (quote emms-cache-enable) "emms-cache" "\
Enable caching of Emms track data.

\(fn)" t nil)

(autoload (quote emms-cache-disable) "emms-cache" "\
Disable caching of Emms track data.

\(fn)" t nil)

(autoload (quote emms-cache-toggle) "emms-cache" "\
Toggle caching of Emms track data.

\(fn)" t nil)

;;;***
