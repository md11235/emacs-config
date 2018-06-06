;;; this file must be in the same directory as .emacs

(setq emacs-config-dir (file-name-directory load-file-name))

(require 'package)
(package-initialize)
(setq-default package-user-dir (concat emacs-config-dir "../shared/elpa/"))

(setq package-archives nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-list-packages)

(package-install 'slime)
(package-install 'elpy)