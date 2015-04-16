;;emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)

(defalias 'emms-read-directory-name #'read-file-name)

(global-set-key (kbd "<f6>") 'emms-play-directory)
(global-set-key (kbd "<f4>") 'emms-play-find)
(global-set-key (kbd "<C-f6>") 'emms-playlist-mode-go)
(setq emms-source-file-default-directory "~/music/")
;;(setq emms-source-file-default-directory nil)
(setq emms-playlist-buffer-name "*EMMS*") ;; show the playlist buffer
(setq emms-repeat-playlist t) ;; repeat the playlist

(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
