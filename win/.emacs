(setq emacs-config-dir (file-name-directory load-file-name))
;;(setq emacs-config-dir "d:/Kanbox/badb01@me.com/emacs-config/win/")
;;(setq emacs-config-dir "j:/zhang/Kanbox/badb01@me.com/emacs-config/win/")
;;(setq emacs-config-dir (file-name-directory (file-truename (buffer-file-name))))


(setq msys2-root-dir "d:/msys64/")
(setq debug-ignored-errors (remq 'user-error debug-ignored-errors))

;; (setenv "PATH" "c:/Users/zhang/Anaconda2;c:/Users/zhang/Anaconda2/Scripts;d:/msys64/usr/bin;d:/msys64/mingw32/bin")

(setq custom-file (concat emacs-config-dir "custom-file.el"))
(load-file custom-file)
(setq custom-file (concat emacs-config-dir "custom_defuns.el"))
(load-file custom-file)

;;(setq initial-major-mode 'lisp-interaction-mode)

;;-----------------------------------load directories

;; (let* ((dir (expand-file-name (concat emacs-config-dir "site-lisp")))
;;        (default-directory dir))
;;   (when (file-directory-p dir)
;;     (add-to-list 'load-path dir)
;;     (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;         (normal-top-level-add-subdirs-to-load-path))))

(defun add-non-system-site-lisp (site-lisp-path)
  (let* ((dir (expand-file-name site-lisp-path))
         (default-directory dir))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path)))))

(add-non-system-site-lisp (concat emacs-config-dir "site-lisp"))
(add-non-system-site-lisp (concat emacs-config-dir "../shared/site-lisp/"))
(add-non-system-site-lisp (concat emacs-config-dir "../shared/elpa/"))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))


;;;; emacs customization
(setq kill-ring-max 200)
(setq enable-recursive-minibuffers t)
;;(setq major-mode 'text-mode)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(setq show-paren-style 'parenthesis)
;;; relevance based sorting instead of alphabetical
(setq apropos-sort-by-scores t)

;;(mouse-avoidance-mode 'animate)

(menu-bar-mode -1)
(tool-bar-mode 0)

(auto-image-file-mode)
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)
(setq inhibit-startup-message t)
(setq explicit-shell-file-name "cmdproxy.exe")

(setq make-backup-files nil)
(setq delete-auto-save-files t)

(setq split-height-threshold nil)
(setq split-width-threshold nil)

(set-frame-height (selected-frame) 39)
(set-frame-width (selected-frame) 80)
(set-frame-position (selected-frame) 0 0)
(global-font-lock-mode t)
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq frame-title-format (list "[Emacs]: (%f)"))
(set-face-foreground 'mode-line "firebrick")
(setq column-number-mode t)

;;;; madsen


(setq-default transient-mark-mode t)
(delete-selection-mode 1)
;; ;; always end a file with a newline
;; (setq require-final-newline t)
;; ;; stop at the end of the file, not just add lines
;; (setq next-line-add-newlines nil)
(setq visible-bell t)

;;; key bindings
;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(keyboard-translate ?\C-h ?\C-?)
(keyboard-translate ?\C-? ?\C-d)
(global-set-key "\M-'" 'mark-sexp)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-x C-g") 'find-file-at-point)
(global-set-key (kbd "C-c C-f") 'find-file-at-point)
(global-set-key [(M-f4)] 'save-buffers-kill-emacs)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
;;(global-set-key (kbd "<C-return>") 'hippie-expand)
(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key (kbd "C-2") 'set-mark-command)
(global-set-key (kbd "C-x C-2") 'pop-global-mark)
(global-set-key [f12] 'shell)
(global-set-key [f9] 'list-bookmarks)
(global-set-key [f8] 'goto-line)
(global-set-key "\C-x\C-b" 'bs-show) ;;or 'electric-buffer-list
;(global-set-key [(f5)] 'joc-dired-magic-buffer)
;; (global-set-key [(control f5)] (function
;;                                 (lambda nil (interactive)
;;                                   (joc-dired-magic-buffer default-directory))))
(global-set-key [(shift f5)] (function
                              (lambda nil (interactive)
                                (message "Current directory is: %s" default-directory))))
(global-set-key [(meta f5)] 'joc-dired-toggle-buffer-name)
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)
(global-set-key (kbd "C-c C-k") 'kill-region)
;; (global-set-key (kbd "C-x a") 'anything)
;; (global-set-key (kbd "C-x C-a") 'anything)
;; (global-set-key (kbd "C-x C-n") 'anything)
;; (global-set-key (kbd "C-x n") 'anything)
;;(global-set-key "\C-w"     'backward-kill-word)
;;(global-set-key "\C-x\C-k" 'kill-region)
(defalias 'exit 'save-buffers-kill-emacs)

;;; key bindings end

(if (string= "windows-nt" system-type)
    (setq server-auth-dir
          (expand-file-name ".emacs.d/server/" (getenv "APPDATA"))))

(server-start)

;; use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
;; Don't add newlines to end of buffer when scrolling
(setq-default next-line-add-newlines nil)
;; Always end a file with a newline
(setq-default require-final-newline nil)
;; Stop at the end of the file, not just add lines
(setq-default next-line-add-newlines nil)
(setq mode-require-final-newline nil)

;;;; emacs customization ends




;; (autoload 'thumbs "thumbs" "Preview images in a directory." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq nxml-sexp-element-flag t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; javascript mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; (autoload 'javascript-mode "javascript" nil t)

;;;;;;;; mew
;;(load "d:/myemacs/.mew.el")


;; ;;;; folding-mode

;; (require 'folding)
;; (autoload 'folding-mode          "folding" "Folding mode" t)
;; (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
;; (autoload 'turn-on-folding-mode  "folding" "Folding mode" t)
;; (folding-add-to-marks-list 'php-mode "//{{"  "//}}}"  nil t)
;; (folding-add-to-marks-list 'prolog-mode "%{{{" "%}}}" nil t)
;; (folding-add-to-marks-list 'html-mode "<!-- {{{ " "<!-- }}} -->" " -->" nil t)

;; ;;;; folding-mode end


;;;; html-helper-mode

;; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; (setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))

;;;; html-helper-mode end


;;;; highlight-current-line
;; (require 'highlight-current-line)
;; (highlight-current-line-on nil)
;; ;;(highlight-current-line-set-fg-color "#696969")
;; (highlight-current-line-set-fg-color "#000000")
;; (highlight-current-line-set-bg-color "#e5e5e5")

(setq highlight-nonselected-windows t)

;;;; highlight-current-line end



;;;; for gnuserver

;;(require 'gnuserv)
;;(load "gnuserv")
;;(gnuserv-start)
;;(setenv "GNUSERV_SHOW_EMACS" "1")
;;(setq gnuserv-frame (selected-frame))

;;;; for gnuserver end




;;;; actionscript mode

;;(require 'actionscript-mode)
;;(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

;; (add-to-list 'load-path "D:/funny/aemoncannon-flyparse-mode")
;; (add-to-list 'load-path "D:/funny/as3-mode")
;; (add-to-list 'load-path "D:/funny")
;; (require 'yasnippet)
;; (require 'yasnippet-bundle)
;;(require 'flyparse-mode)
;;(require 'as3-mode)
;;(setq as3-project-source-paths `("D:/dev/k2/src"))

;;(setenv "CLASSPATH" "d:\\Program Files\\Java\\thirdParty")
;;(add-to-list 'auto-mode-alist '("\\.as\\'" . as3-mode))

;;;; actionscript mode end


;; ;;;; matlab mode

;; (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . objc-mode) auto-mode-alist))
;; (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
;; (setq matlab-indent-function t)	; if you want function bodies indented
;; (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
;; (defun my-matlab-mode-hook ()
;;   (setq fill-column 72))		; where auto-fill should wrap
;; (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
;; (defun my-matlab-shell-mode-hook ()
;;   '())
;; (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

;; ;;;; matlab mode



;;(set-background-color "white")
;;(set-foreground-color "black")
;;(set default-fill-column 120)
;;(x-parse-geometry "120x270+0-0")
;;(setq-default auto-fill-mode t)

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(cygwin-mount-build-mount-table-asynch t)
;;  '(cygwin-mount-cygwin-bin-directory "d:/cygwin/bin/")
;;  '(cygwin-mount-table (quote (("/" . "d:/cygwin/")))))
;;custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.


;; desktop
(use-package desktop
  :config
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
  )


;;; desktop-override-stale-locks.el begins here
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;;; desktop-override-stale-locks.el ends here

;;(setq desktop-path (append (list (concat (getenv "HOME") "/emacs_desktop/")) desktop-path))


;;(require 'anything-config)


;; hooks
(add-hook 'emacs-startup-hook ;; term-setup-hook
          #'(lambda () (w32-send-sys-command ?\xF030)))

;; (require 'cl-info)
;; (global-set-key (kbd "<f5>") 'cl-info)

(normal-erase-is-backspace-mode 1)

(ansi-color-for-comint-mode-on)

;;; accelerator for what-line & goto-line
(global-set-key "\C-c\C-l" 'what-line)
(global-set-key "\C-c\C-g" 'goto-line)

;;; now set the display for smart quotes to the standard quote character
(standard-display-ascii ?\221 "\`")
(standard-display-ascii ?\222 "\'")
(standard-display-ascii ?\223 "\"")
(standard-display-ascii ?\224 "\"")
(standard-display-ascii ?\227 "\-")
(standard-display-ascii ?\225 "\*")

;; better switch buffer mode
;;(iswitchb-mode 0)

;; madsen mark
;; buffers have unique names so we know where index.html file belongs
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  :defer t)

(setq display-time-format "[%Y-%m-%d %H:%M:%S (%z)]")
;; (setq display-time-format nil)

(setq
 ;; First two not needed if set display-time-string-forms directly.
 display-time-day-and-date t
 display-time-24hr-format  t
 display-time-interval     10)

(display-time-mode t)

;; 2008-10-15
(c-set-offset 'inline-open 0)
(c-add-style "zscstyle" '("awk" (c-offsets-alist (inline-open . 0))))
(setq c-default-style
      '((c-mode . "gnu") (c++-mode . "gnu") (java-mode . "java") (awk-mode . "awk") (other . "gnu")))


(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

;; after save hooks
(add-hook 'after-save-hook
          (lambda ()
            (mapcar
             (lambda (file)
               (setq file (expand-file-name file))
               (when (string= file (buffer-file-name))
                 (save-excursion (byte-compile-file file))))
             `(,(concat emacs-config-dir ".emacs") "~/.gnus.el" "~/.wl"))))

(windmove-default-keybindings 'meta)

;;(make-local-variable 'user-full-name)
;;(make-local-variable 'user-mail-address)
;; (setq user-full-name "Zhang Sen <sen.zhang@gmail.com>")
;; (setq user-mail-address "sen.zhang@126.com")
;; (setq user-mail-address "md11235@gmail.com")
;; (setq user-full-name "Madsen Zhang")


;; Update file headers when write files.
;;(add-hook 'write-file-hooks 'update-file-header)

;; Create headers for file buffers in my favorite modes.
;;(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;;(add-hook 'c-mode-common-hook   'auto-make-header)

(defun zs-set-comment-style ()
  "Set comment-style to extra-line."
  (interactive)
  ;; <add other stuff here>
  (set (make-local-variable (quote comment-style)) 'extra-line)
  )
;; ;;(add-hook 'c-mode-hook '(setq comment-style (quote extra-line)))
;; ;;(add-hook 'c-mode-hook 'zs-set-comment-style nil t)
(add-hook 'c-mode-hook 'zs-set-comment-style)
(add-hook 'c++-mode-hook 'zs-set-comment-style)
;;(setq comment-style 'extra-line)

(scroll-bar-mode -1)
(toggle-scroll-bar -1)
(setq vertical-scroll-bar nil)
(horizontal-scroll-bar-mode -1)

(defun generalized-shell-command (command arg)
  "Unifies `shell-command' and `shell-command-on-region'. If no region is
selected, run a shell command just like M-x shell-command (M-!).  If
no region is selected and an argument is a passed, run a shell command
and place its output after the mark as in C-u M-x `shell-command' (C-u
M-!).  If a region is selected pass the text of that region to the
shell and replace the text in that region with the output of the shell
command as in C-u M-x `shell-command-on-region' (C-u M-|). If a region
is selected AND an argument is passed (via C-u) send output to another
buffer instead of replacing the text in region."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; No active region
        (if (eq arg nil)
            (shell-command command)
          (shell-command command t))
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region p m command t t)
        (shell-command-on-region p m command)))))




;;(require 'redshank)

;; turn on normal-erase-is-backspace-mode for some programming languages modes
(mapc (lambda (mode)
        (let ((hook (intern (concat (symbol-name mode)
                                    "-mode-hook"))))
          (add-hook hook (lambda () (normal-erase-is-backspace-mode 1)))))
      '(php lisp emacs-lisp inferior-lisp python ruby c))


;; madsen mark
(defun isearch-yank-symbolic-word-or-char ()
  (interactive)
  (isearch-yank-internal
   (lambda ()
     (let ((distance (skip-syntax-forward "w_")))
       (when (zerop distance) (forward-char 1))
       (point)))))

(add-hook 'lisp-mode-hook
          (lambda ()
            (make-local-variable 'isearch-mode-map)
            (define-key isearch-mode-map
              "\C-w" 'isearch-yank-symbolic-word-or-char)))

(load (concat emacs-config-dir "keybindings.el"))

;; (defadvice indent-according-to-mode (around indent-comment-properly-in-php-mode activate)
;;   (if (string-equal mode-name "PHP")
;;       (let ((paren-state (c-parse-state))
;;             (orig-point (point))
;;             (use-orig-function t)
;;             syn-context)
;;         (save-excursion
;;           (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
;;           (let ((header-tag (buffer-substring-no-properties (- (point) 2)
;;                                                             (+ (point) 3))))
;;             (if (or (string-equal header-tag "<?php")
;;                     (string-equal header-tag "<?PHP"))
;;                 (progn
;;                   (setq use-orig-function nil)
;;                   (goto-char orig-point)
;;                   (setq syn-context (c-guess-basic-syntax))
;;                   (if (assq 'topmost-intro-cont syn-context)
;;                       (setq syn-context (cons (list (car (assoc 'topmost-intro c-offsets-alist))
;;                                                     (cdr (assoc 'topmost-intro c-offsets-alist)))
;;                                               (cdr syn-context))))
;;                   (c-indent-line syn-context)
;;                   )
;;               )))
;;         (if use-orig-function
;;             ad-do-it))
;;     ad-do-it))
;;;; madsen

;;;; color-theme
;; (load-library "color-theme")
;; (require 'color-theme)

;; (color-theme-initialize)
;; (setq color-theme-is-global nil)

;; (require 'color-theme-billc)
;; (color-theme-billc)
;;(color-theme-marine)

;; (load-library "color-theme")
(use-package color-theme
  :config
  (color-theme-initialize)
  (setq color-theme-is-global t)
  )

(use-package smart-mode-line
  :init
  (setq sml/theme 'dark)
  :config
  (sml/setup)
  (color-theme-billw)
)

;;(color-theme-marine)
;; (use-package color-theme-billc
;;   :config
;;   (color-theme-billw)
;;   )


;; (require 'tmtheme)
;; (setq tmtheme-directory (concat emacs-config-dir "site-lisp/tmthemes/"))
;; (tmtheme-scan)
;; (tmtheme-Blackboard-II)

;; ;;;; color-theme end

;; (defalias 'oc 'occur)

;; (setq abbrev-mode nil)

;; (defun toggle-current-window-dedication ()
;;  (interactive)
;;  (let* ((window    (selected-window))
;;         (dedicated (window-dedicated-p window)))
;;    (set-window-dedicated-p window (not dedicated))
;;    (message "Window %sdedicated to %s"
;;             (if dedicated "no longer " "")
;;             (buffer-name))))

;; (global-set-key (kbd "<pause>") 'toggle-current-window-dedication)

;; (when (eq 'windows-nt system-type)
;;   (setq default-directory (getenv "HOME")))

;;;; emacs keybindings

;;;; emacs keybindings

(defun pretty-lambdas ()
  (font-lock-add-keywords nil `(("(\(lambda\>\)"
                                 (0 (progn (compose-region (match-beginning 1) (match-end 1) ,(make-char 'greek-iso8859-7 107)) nil))))))

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)



;; (add-hook 'before-save-hook '(lambda ()
;;                                (delete-trailing-whitespace)))

(setq auto-save-timeout 15)


;; vc
;; (setenv "SVN_SSH" "d:/Program\\ Files/TortoiseSVN/bin/TortoisePlink.exe")

(global-set-key (kbd "M-n") 'skip-to-next-non-spaces)

(defun skip-to-next-non-spaces (backward)
  (interactive "P")
  (if backward
      (progn
        (backward-word)
        (skip-chars-backward "^a-zA-Z"))
    (progn
      (let ((cur-char (char-after)))
        (if cur-char
            (progn
              (if (not (member (string (char-syntax cur-char)) '("_" " ")))
                (forward-word))
              (skip-chars-forward "[:blank:]")))))))

(auto-compression-mode 1)

;; (require 'erin)

;; (require 'yaml-mode)

;;;; rgrep-config.el

;; (defun jbr-init ()
;;   "Called from term-setup-hook after the default
;; terminal setup is
;; done or directly from startup if term-setup-hook not
;; used.  The value
;; 0xF030 is the command for maximizing a window."
;;   (interactive)
;;   (w32-send-sys-command #xf030)
;;   (ecb-redraw-layout)
;;   (calendar))
;; (setq term-setup-hook 'jbr-init)
;; (setq window-setup-hook 'jbr-init)

;; ;;;; fullscreen

;; (add-to-list 'load-path "d:/dropbox/emacs_config/win/site-lisp/fullscreen/")
;; (require 'darkroom-mode)

;; ;;;; fullscreen end

(use-package icomplete+
  :defer t)
(use-package markdown-mode
  :defer t)

(use-package sx-load
  :defer t)

(setq gnutls-trustfiles '("d:\\msys64\\usr\\ssl\\certs\\ca-bundle.crt" "d:\\zhang\\msys64\\usr\\ssl\\certs\\ca-bundle.crt"))

(require 'package)
(setq-default package-user-dir (concat emacs-config-dir "../shared/elpa/")) ;;  "d:\\Kanbox\\badb01@me.com\\emacs-config\\shared\\elpa"
;; (setq package-quickstart t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq package-archives
      '(("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
        ("org"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("marmalade"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))

;; (setq package-archives
;;       '(("melpa" . "https://melpa.org/packages/")
;;         ("marmalade" . "http://marmalade-repo.org/packages/")
;;         ("popkit" . "http://elpa.popkit.org/packages/")
;;         ("org" . "http://orgmode.org/elpa/")))
;; (setq package-archives (butlast package-archives))

(use-package cl)

(defun load-particle (particle-filename)
  (load-file (concat emacs-config-dir particle-filename)))

(load-particle "util-defuns.el")

;; (load-particle "ido-config.el")
(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-tramp-completion t)
  (setq ido-enable-flex-matching t) ; fuzzy matching is a must have
  (icomplete-mode 1)
  (defface ido-first-match  '((t (:bold t :foreground "purple")))
    "Face used by ido for highlighting first match."
    :group 'ido)
  (setq ido-indicator '((t (:background "red" :foreground "yellow" :width condensed))))
  (setq ido-only-match '((t (:foreground "Green" :weight ultra-bold))))
  (setq ido-subdir '((t (:foreground "red"))))
  :bind (("C-x C-f" . ido-find-file)
         ("C-x b" . ido-switch-buffer)
         )
)

(use-package ibuffer
  :commands ibuffer
  :bind (("C-x C-b" . ibuffer)
         )
  )

;; (load-particle "scheme-config.el")
(use-package scheme)

;; (load-particle "paredit-config.el")
(use-package paredit
  :config
  (push #'(lambda (endp delimiter)
            (if (and (not endp)
                     (eq ?\" (char-syntax delimiter)))
                (if (and (member major-mode '(slime-repl-mode lisp-mode))
                         (string-equal (upcase (buffer-substring-no-properties (- (point) 2) (point)))
                                       "#P"))
                    nil
                  t)
              t))
        paredit-space-for-delimiter-predicates)
  :bind (:map emacs-lisp-mode-map
         ("M-0" . paredit-close-round-and-newline)
         :map scheme-mode-map
         ("M-0" . paredit-close-round-and-newline)
         ("M-9" . paredit-backward-slurp-sexp)
         :map lisp-mode-map
         ("M-0" . paredit-close-round-and-newline)
         ("M-[" . paredit-backward-barf-sexp)
         ("M-]" . paredit-forward-barf-sexp)
         )
  
  :hook ((emacs-lisp lisp inferior-lisp slime slime-repl) . paredit-mode)
  )

(load-particle "isearch-config.el")

;; (load-particle "muse-config.el")
;; (load-particle "swank-emacs-lisp-config.el")

;; (load-particle "w3m-config.el")
;; (load-particle "sdcv-config.el")
;; (load-particle "hippie-expand-config.el")

;; (load-particle "rgrep-config.el")

(case (intern system-configuration)
  ('i686-w64-mingw32
   (setq exec-path
         (cons
          (concat msys2-root-dir "usr/bin/")
          (cons
           (concat emacs-config-dir "bin/")
           exec-path))))
  (t
   nil))
(setenv "PATH" (concat
                (concat msys2-root-dir "mingw32/bin")
                ";"
                (concat msys2-root-dir "usr/bin")) t)

(use-package grep
  :config 
  (setq grep-find-use-grep-r nil)
  
  (grep-apply-setting 'grep-find-command
                      (concat msys2-root-dir "usr/bin/find.exe"))
  )

;; (load-particle "dired-config.el")
(defun mdsz-dired-view-file ()
 (interactive)
 (case system-type
   ('windows-nt (dired-w32-browser))
   (t (dired-view-file))))

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
        loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map (kbd "v") 'mdsz-dired-view-file)
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (joc-dired-single-buffer "..")))))

(use-package dired
  :init
  (setq-default diredp-hide-details-initially-flag nil)
  :config
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init))

  (setq dired-recursive-deletes 'top dired-recursive-copies 'top dired-dwim-target t)
  (add-hook 'dired-mode-hook (lambda ()
                               (local-set-key (kbd "C-c C-r") 'wdired-change-to-wdired-mode)))

  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (setq dired-omit-files "^\\.")
  )

(use-package dired-single)
(use-package dired+)

;; (load-particle "highlight-paren-config.el")

(use-package highlight-parentheses
  :config
  (setq hl-paren-colors
        (make-list 20 "magenta1")
        ;; '(;"#8f8f8f" ; this comes from Zenburn
        ;;                                   ; and I guess I'll try to make the far-outer parens look like this
        ;;   "orange1" "yellow1" "greenyellow" "green1"
        ;;   "springgreen1" "cyan1" "slateblue1" "magenta1" "purple")
        )

  :hook ((lisp emacs-lisp) .
         (lambda ()
           (highlight-parentheses-mode)
           (setq autopair-handle-action-fns
                 (list 'autopair-default-handle-action
                       '(lambda (action pair pos-before)
                          (hl-paren-color-update))))))
  )

(use-package seognil
  :config
  (setq seognil-dictionary-path (concat emacs-config-dir "dict/seognil/dicts"))
  (setq seognil-dictionaries '("CollinsCobuild" "Collins"))
  :bind
  (("C-c d" . seognil-search))
)

;; (load-particle "slime-config.el")


;; (load-particle "php-mode-config.el")
;; (load-particle "printer-config.el")

;; (load-particle "org-mode-config.el")
;; (load-particle "org-wiki-config.el")

;; (load-particle "clojure-config.el")

;; (load-particle "proxy-config.el")

;; (load-particle "windows-config.el")
;; very unstable due to yas
;; (load-particle "python-config.el")

;; (load-particle "eclim-config.el")
;; (load-particle "org-latex-config.el")

(load-particle "appearances-config.el")


;; (load-particle "seognil-config.el")


(load-particle "locale-config.el")

(setq initial-major-mode 'lisp-interaction-mode)
