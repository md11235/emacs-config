;; desktop
(require 'desktop)
(desktop-save-mode 1)
(setq history-length 250)

(add-to-list 'desktop-globals-to-save 'file-name-history)
;; (condition-case nil
;;     (desktop-read)
;;   (error nil))

(setq desktop-save t)
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
