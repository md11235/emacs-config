(require 'elpy)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook '(lambda ()
                               (elpy-mode t)
                               (elpy-enable)
                               (setq elpy-rpc-backend "jedi")))

(eval-after-load 'elpy
  '(define-key elpy-mode-map (kbd "C-c C-e") 'elpy-shell-send-current-statement))
