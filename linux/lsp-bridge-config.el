(add-to-list 'load-path "/home/chengdu/emacs-sitelisp/lsp-bridge-master")

(require 'yasnippet)
(yas-global-mode 1)

(use-package lsp-bridge
  ;; :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
  ;;           :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
  ;;           :build (:not compile))
  :bind (("M-." . lsp-bridge-find-def)
         ("M-," . lsp-bridge-find-def-return))
  :init (global-lsp-bridge-mode))