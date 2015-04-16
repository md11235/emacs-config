;; use extended compound-text coding for X clipboard

;; Non-nil means cutting and pasting uses the clipboard. This is in addition to, but in preference to, the primary selection.
(when (and (boundp 'window-system) window-system)
  (and (boundp 'x-select-enable-clipboard) (setq x-select-enable-clipboard t))
  (and (boundp 'x-cut-buffer-or-selection-value) (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))
