(require 'elpy)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook '(lambda ()
                               (elpy-mode t)
                               (elpy-enable)
                               (setq elpy-rpc-backend "jedi")))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq-default py-indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))

(eval-after-load 'elpy
  '(define-key elpy-mode-map (kbd "C-c C-e") 'elpy-shell-send-current-statement))


