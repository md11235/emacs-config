;;;;;;;;;;;;;;;;; emacs behavior settings ;;;;;;;;;;;;;;;;
;; enable visual feedback on selections
(setq-default transient-mark-mode t)
(menu-bar-mode 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
;;(setq show-paren-style 'expression)
;;(setq show-paren-style 'mixed)

;; always end a file with a newline
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

(setq-default kill-whole-line nil)

(auto-image-file-mode t)

(setq inhibit-startup-message t)

(setq-default major-mode 'emacs-lisp-mode)
;; (mouse-avoidance-mode 'animate)

(setq use-file-dialog nil)
(setq suggest-key-bindings t)

(setq sentence-end-double-space nil)

;;(set-background-color "white")
;;(set-foreground-color "black")
;;(set default-fill-column 120)
;;(x-parse-geometry "153x60+9-6")
;;(setq-default auto-fill-mode t)
;;(set-frame-height (selected-frame) 39)
;;(set-frame-width (selected-frame) 80)
;;(set-frame-position (selected-frame) 0 0)
(set-cursor-color "purple")
(global-font-lock-mode t)
(setq-default indent-tabs-mode nil)
(setq frame-title-format (list "[Emacs]: (%f)"))
(set-face-foreground 'mode-line "firebrick")
(setq-default default-tab-width 4)
(setq column-number-mode t)
(setq mouse-yank-at-point t)
(setq kill-ring-max 200)
(setq-default fill-column 70)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
;;(auto-fill-mode 1)

(setq visible-bell t)
(setq vertical-scroll-bar nil)
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)
;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(setq enable-recursive-minibuffers t)

;; ignore ring bell
(setq ring-bell-function 'ignore)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode nil))
(setq scroll-bar-mode nil)

(setq size-indication-mode t)
(setq initial-major-mode 'lisp-interaction-mode)

(menu-bar-mode 0)

(show-paren-mode t)
;;(setq show-paren-style 'expression)
(setq show-paren-style 'parenthesis)
;;(setq show-paren-style 'mixed)

;;(load "ssh.el" nil t t)
;; no backup..
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
;;(keyboard-translate ?\C-h ?\C-?)
;; (keyboard-translate ?\C-? ?\C-d)
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

;;(set default-fill-column 120)
;;(setq frame-title-format (list "[Emacs]: (%f)"))
;;(x-parse-geometry "120x270+0-0")
(set-face-foreground 'mode-line "firebrick")
(setq-default default-tab-width 4)
(setq column-number-mode t)
;;(setq-default auto-fill-mode 1)

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; always end a file with a newline
;;(setq require-final-newline t)

(setq track-eol t)
(global-linum-mode 0)
(setq bookmark-save-flag 1)

;;(setq scroll-left 'disabled nil)
(setq show-trailing-whitespace t)
;;(require 'ecmascript-mode)

;;(setq initial-major-mode 'lisp-interaction-mode)

;;;;;;;;;;;;;;; window system ;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  ;; enable wheelmouse support by default
  (if (fboundp 'mwheel-install)
      (mwheel-install)))

(transient-mark-mode t)

(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
;; Don't add newlines to end of buffer when scrolling
;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)
;; Always end a file with a newline
;;(setq require-final-newline t)
(defalias 'exit 'save-buffers-kill-emacs)

;;; hooks
(add-hook 'after-save-hook
          (lambda ()
            (mapcar
             (lambda (file)
               (setq file (expand-file-name file))
               (when (string= file (buffer-file-name))
                 (save-excursion (byte-compile-file file))))
             '("~/.emacs" "~/.gnus.el" "~/.wl"))))

(windmove-default-keybindings 'meta)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (setq url-proxy-services
;;       '(("http" .  "192.168.199.184:1080")
;;         ("https" . "192.168.199.184:1080")))

(setenv "http_proxy" "http://192.168.199.184:1080")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
