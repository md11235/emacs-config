(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
;;(setq inferior-lisp-program "/usr/bin/ccl")
;;(setq inferior-lisp-program "/usr/bin/myclisp") ;; your Lisp system

(require 'slime)
(slime-setup '(slime-fancy slime-autodoc))
(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(define-key slime-repl-mode-map (kbd "C-h") 'paredit-backward-delete)
(define-key slime-repl-mode-map (kbd "DEL") 'paredit-backward-delete)
(define-key slime-repl-mode-map (kbd "M-9") 'paredit-backward-slurp-sexp)
(define-key slime-repl-mode-map (kbd "M-0") 'paredit-forward-slurp-sexp)
;; (define-key slime-repl-mode-map (kbd "M-r") 'slime-repl-previous-matching-input)
(define-key slime-repl-mode-map (kbd "M-r") 'raise-sexp) ;; or paredit-raise-sexp
(define-key slime-repl-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)

(define-key lisp-mode-map (kbd "C-h") 'paredit-backward-delete)
(define-key lisp-mode-map (kbd "DEL") 'paredit-backward-delete)
(define-key lisp-mode-map (kbd "M-9") 'paredit-backward-slurp-sexp)
(define-key lisp-mode-map (kbd "M-0") 'paredit-forward-slurp-sexp)
(define-key lisp-mode-map (kbd "M-i") 'slime-complete-symbol)
(define-key lisp-mode-map [(tab)] 'slime-complete-symbol)
(define-key lisp-mode-map (kbd "M-/") 'slime-complete-symbol)
(define-key lisp-mode-map (kbd "M-r") 'raise-sexp)
(define-key lisp-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
(define-key lisp-mode-map (kbd "C-i") 'slime-indent-and-complete-symbol)
(define-key lisp-mode-map (kbd "M-r") 'paredit-raise-sexp)

(add-hook 'lisp-mode-hook
          (lambda ()
            (define-key lisp-mode-map (kbd "C-c C-f") 'mark-defun)))

;; jump quickly to the slime sbcl buffer
(global-set-key (kbd "M-o x")
                '(lambda ()
                   (interactive)
                   (switch-to-buffer "*slime-repl sbcl*")))0 ;136;0c

