;; (mapc (lambda (mode)
;;         (let ((hook (intern (concat (symbol-name mode)
;;                                     "-mode-hook"))))
;;           (add-hook hook (lambda () (normal-erase-is-backspace-mode 1)))))
;;       '(php lisp emacs-lisp inferior-lisp python ruby c))

;; (kbd "<deletechar>")
;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (make-local-variable 'paredit-mode-map)
;;             (define-key paredit-mode-map
;;               [deletechar] 'paredit-backward-delete)))

