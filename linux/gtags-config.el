(autoload 'gtags-mode "gtags" "" t)
(add-hook 'gtags-mode-hook '(lambda ()
                              (local-set-key (kbd "M-8") 'gtags-pop-stack)))
(add-hook 'gtags-mode-hook '(lambda ()
                              (local-set-key (kbd "M-,") 'gtags-pop-stack)))

