;;; this file must be in the same directory as .emacs

(require 'url-vars)

;; (setq url-proxy-services
;;       '(("http" .  "192.168.100.166:1080")
;;         ("https" . "192.168.100.166:1080")))

;; (setenv "http_proxy" "http://192.168.100.166:1080")


(setq emacs-config-dir (file-name-directory load-file-name))

(require 'package)
(package-initialize)
(setq-default package-user-dir (concat emacs-config-dir "../shared/elpa/"))

(setq package-archives nil)
(setq package-archives
      '(("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
        ("org"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("marmalade"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-refresh-contents)

(package-install 'slime)
(package-install 'elpy)
(package-install 'use-package)
(package-install 'diminish)
(package-install 'smart-mode-line)
