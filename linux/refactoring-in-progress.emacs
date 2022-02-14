;;;; this file serves to set up the core infrastructure to load other configurations

(setq emacs-config-dir (file-name-directory load-file-name))

(defmacro eval-when-gnu/linux (&rest body)
  `(if (string= system-type "gnu/linux")
      (progn
        ,@body)))

(defmacro eval-when-windows-nt (&rest body)
  `(if (string= system-type "windows-nt")
      (progn
        ,@body)))

(defun add-non-system-site-lisp (site-lisp-path)
  (let* ((dir (expand-file-name site-lisp-path))
         (default-directory dir))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path)))))

(add-non-system-site-lisp (concat emacs-config-dir "site-lisp"))
(add-non-system-site-lisp (concat emacs-config-dir "../shared/site-lisp"))
(add-non-system-site-lisp (concat emacs-config-dir "../shared/elpa"))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(require 'package)

(setq-default package-user-dir (concat emacs-config-dir "../shared/elpa/"))

;;; !!! Be careful. set package-load-list might disable all other packages.
;; (setq package-load-list '((elpy all)
;;                           (slime all)
;;                           ))
;; (setq package-load-list '(all))

(setq package-archives nil)
(setq package-archives
      '(("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ("org"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("nongnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

(package-initialize)


;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; (setq package-archives
;;       '(("marmalade" . "https://marmalade-repo.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("popkit" . "http://elpa.popkit.org/packages/")
;;         ("gnu" . "https://elpa.gnu.org/packages/")
;;         ;; ("org" . "http://orgmode.org/elpa/")
;;         ))

;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; (add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(icicle-reminder-prompt-flag 3)
 '(lpr-command "lpr")
 '(lpr-printer-switch "-o PageSize=A4 -o media=a4 -o page-bottom=36 -o page-left=36 -o page-right=36 -o page-top=36 -p")
 '(muse-project-alist (quote (("WikiPlanner" ("~/net9svn/plan" :default "TaskPool" :major-mode planner-mode :visit-link planner-visit-link) (:base "planner-html" :path "~/net9svn/plan/public")) ("linux_notes" ("~/net9svn/muse" :default "index" :major-mode muse-mode) (:base "html" :path "~/net9svn/muse/public")) ("lessen_notes" ("/opt/English/iBT/notes" :default "index" :major-mode muse-mode) (:base "planner-html" :path "/opt/English/iBT/notes/public")))))
 '(org-agenda-files (quote ("~/org/gtd.org")))
 '(ps-font-family (quote Times))
 '(safe-local-variable-values (quote ((Syntax . ANSI-COMMON-LISP) (Package . ccl) (Package . SYSTEM) (Package . C) (Syntax . Common-Lisp) (Mode . C++) (Mode . Emacs-Lisp) (Mode . LISP) (Mode . Lisp) (Package CCL :use CL) (Package X86 :use CL) (Package . umweb) (package . asdf) (Package . modlisp) (Package . UFFI) (Package ARCH :use CL) (Package X8632 :use CL) (package . net\.aserve) (Package . FLEXI-STREAMS) (fill-column) (Package . DRAKMA) (eval add-hook (quote write-file-hooks) (quote time-stamp)) (Package . CCL) (Package . CL-USER) (Package . CL-FAD) (Syntax . ANSI-Common-Lisp) (Syntax . COMMON-LISP) (Package . HUNCHENTOOT) (Base . 10)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(info-title-1 ((t (:inherit variable-pitch :weight semi-bold :height 2.2))))
 '(info-title-2 ((t (:inherit variable-pitch :weight semi-bold :height 1.8))))
 '(info-title-3 ((t (:inherit variable-pitch :weight semi-bold :height 1.6))))
 '(info-title-4 ((t (:inherit variable-pitch :weight semi-bold :height 1.4))))
 '(variable-pitch ((t (:inherit default :family "Lucida Grande")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (concat emacs-config-dir "emacs-custom.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-particle (particle-filename)
  (load-file (concat emacs-config-dir particle-filename)))

(load-particle "keybinding-config.el")
(load-particle "as-config.el")
(load-particle "clipboard-config.el")
(load-particle "comment-style-config.el")
(load-particle "css-config.el")
(load-particle "desktop-config.el")
(load-particle "dired-config.el")
(load-particle "emacs-customization.el")
;; (load-particle "emms-config.el")
(load-particle "fold-config.el")
(load-particle "font-config.el")
(load-particle "gtags-config.el")
;; (load-particle "haskell-config.el")
(load-particle "header2-config.el")
(load-particle "highlight-line-config.el")
(load-particle "highlight-parentheses-config.el")
(load-particle "hippie-config.el")
(load-particle "html-helper-config.el")
(load-particle "hyperspec-config.el")
(load-particle "ibuffer-config.el")
(load-particle "ido-config.el")
(load-particle "indent-expand-config.el")
(load-particle "isearch-config.el")
(load-particle "js-config.el")
(load-particle "locale-config.el")
(load-particle "mmm-config.el")
(load-particle "mode-compile-config.el")
(load-particle "muse-config.el")
(load-particle "org-config.el")
(load-particle "paredit-config.el")
;; (load-particle "php-config.el")
(load-particle "psgml-config.el")
(load-particle "quote-config.el")
(load-particle "risky-functions-config.el")
(load-particle "ruby-config.el")
;; (load-particle "slime-config.el")
(load-particle "tex-config.el")
(load-particle "text-mode-config.el")
(load-particle "time-config.el")
(load-particle "tinyeat-config.el")
(load-particle "tramp-config.el")
(load-particle "uniquify-config.el")
(load-particle "util-config.el")
(load-particle "w3m-config.el")
(load-particle "x-window-config.el")
(load-particle "yasnippet-config.el")
(load-particle "server-config.el")
(load-particle "c-mode-config.el")
(load-particle "version-control-config.el")
;; (load-particle "auto-completion-config.el")
(load-particle "misc-require-config.el")
(load-particle "lisp-mode-config.el")
;;(load-particle "clojure-config.el")
(load-particle "color-theme-config.el")
;; (load-particle "python-mode-config.el")
(load-particle "lsp-mode-config.el")

(use-package smart-mode-line
  :init
  (setq sml/theme 'dark)
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))


(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\.*\\|192\\.168.*\\)")
        ("http" .  "10.30.12.34:8118")
        ("https" . "10.30.12.34:8118")))

;; (setenv "http_proxy" "http://192.168.100.3:1080")

;; (setq url-proxy-services nil)
;; (setenv "http_proxy" "http://192.168.100.3:1080")
