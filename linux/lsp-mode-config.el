(use-package dash)

(use-package company
  :hook (python-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

(use-package lsp-mode
  :hook ((c++-mode python-mode java-mode js-mode) . lsp-deferred)
  :commands lsp
  :config
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5))

(use-package lsp-lens)

(use-package lsp-modeline)

(use-package lsp-headerline)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05)
  :custom
  '((lsp-ui-doc-enable            . t)
    (lsp-ui-doc-position          . 'at-point)
    (lsp-ui-doc-header            . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-max-width         . 150)
    (lsp-ui-doc-max-height        . 30)
    (lsp-ui-doc-use-childframe    . nil)
    (lsp-ui-doc-use-webkit        . nil)
    (lsp-ui-peek-enable           . t)
    (lsp-ui-peek-peek-height      . 20)
    (lsp-ui-peek-list-width       . 50)))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python")
          (setq lsp-pyright-python-executable-cmd "python")))
