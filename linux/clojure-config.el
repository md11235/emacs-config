(require 'clojure-mode)
(require 'cider)
(require 'projectile)
(require 'company)
(require 'company-capf)

(setq cider-lein-command (expand-file-name "~/bin/lein"))

(add-hook 'clojure-mode-hook
          (lambda ()
            (cider-mode)
            (company-mode)
            ;; (company-capf 1)
            ))

(add-hook 'cider-mode-hook
          (lambda ()
            (paredit-mode +1)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (paredit-mode +1)))
