;;another fold..
(setq fold-mode-prefix-key "\C-c\C-o")
(setq fold-autoclose-other-folds nil)
(require 'fold nil t)
(when (featurep 'fold)
  (add-hook 'find-file-hook 'fold-find-file-hook t))
;;mode for view tab and whitespace
