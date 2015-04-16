(require 'paredit)
(mapc (lambda (mode)
        (let ((hook (intern (concat (symbol-name mode)
                                    "-mode-hook"))))
          (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp))
(add-hook 'lisp-mode-hook
          (lambda ()
            (make-local-variable 'paredit-mode-map)
            (define-key paredit-mode-map
              (kbd "<deletechar>") 'paredit-backward-delete)
            (highlight-parentheses-mode)))

(define-key paredit-mode-map (kbd "M-r") 'paredit-raise-sexp)


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

