;;;;;;;;;;;;;;;;;; hippie-expand customizaton
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-line
;;         try-expand-line-all-buffers
;;         try-expand-list
;;         try-expand-list-all-buffers
;;         try-expand-dabbrev
;;         try-expand-dabbrev-visible
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-file-name
;;         try-complete-file-name-partially
;;         try-complete-lisp-symbol
;;         try-complete-lisp-symbol-partially
;;         try-expand-whole-kill))

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-whole-kill
        ;;senator-try-expand-semantic
        try-expand-dabbrev-visible
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-line
        try-expand-line-all-buffers))


(global-set-key (kbd "<C-return>") 'hippie-expand)