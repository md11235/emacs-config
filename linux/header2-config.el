;; header
(require 'header2)
(setq make-header-hook '( ;;header-mode-line
                         header-title
                         header-blank
                         header-file-name
                         header-description
                         ;;header-status
                         header-author
                         ;;header-maintainer
                         ;;header-copyright
                         header-creation-date
                         ;;header-rcs-id
                         header-version
                         ;;header-sccs
                         header-modification-date
                         header-modification-author
                         header-update-count
                         header-url
                         header-keywords
                         header-compatibility
                         header-blank
                         header-lib-requires
                         header-end-line
                         header-commentary
                         header-blank
                         header-blank
                         header-blank
                         header-end-line
                         header-history
                         header-blank
                         header-blank
                         ;; header-rcs-log
                         header-end-line
                         ;;header-free-software
                         header-code
                         header-eof
                         ))

;;(make-local-variable 'user-full-name)
;;(make-local-variable 'user-mail-address)
(setq user-full-name "md11235 <md11235@gmail.com>")
(setq user-mail-address "md11235@gmail.com")

;; Update file headers when write files.
(add-hook 'write-file-hooks 'update-file-header)

;; Create headers for file buffers in my favorite modes.
;; (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)

(setq file-header-update-alist nil)
(register-file-header-action "Last-Updated[ \t]*: "
                             'update-last-modified-date)
(register-file-header-action "          By[ \t]*: "
                             'update-last-modifier)
(register-file-header-action "    Update #[ \t]*: "  'update-write-count)
