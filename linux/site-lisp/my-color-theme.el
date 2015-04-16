;;; my-color-theme.el --- 
;; 
;; Filename: my-color-theme.el
;; Description: 
;; Author: 
;; Created: Thu Mar 27 10:12:42 2008
;; Version: 
;; Last-Updated: Mon Mar 23 11:52:54 2015 (28800 CST)
;;           By: md11235 <md11235@gmail.com>
;;     Update #: 17
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; allow for color prompt in shell
(ansi-color-for-comint-mode-on)

(set-cursor-color "red")
(set-face-foreground 'font-lock-comment-face "#95917E")
(set-face-foreground 'font-lock-string-face "#61ce3c")
(set-face-foreground 'font-lock-keyword-face "#f8dd2d")
(set-face-foreground 'font-lock-function-name-face "#e3611f")
(set-face-foreground 'font-lock-variable-name-face "#98fbff")
(set-face-foreground 'font-lock-type-face "#A6E22E")
(set-face-foreground 'region "#272822")
(set-face-background 'region "#66D9EF")
(set-face-foreground 'font-lock-constant-face "#66d9ef")

(setq default-frame-alist
      '((background-color . "#0b0f20")
        (foreground-color . "#f8f8f2")
        (cursor-color . "red")
        (cursor-type . bar)))

;; 0b0f20 #f8f8f2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my-color-theme.el ends here
