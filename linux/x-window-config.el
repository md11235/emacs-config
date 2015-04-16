(defun zs-x-settings ()
  "some settings specular to window-system"
  ;;(setq x-select-enable-clipboard t)
  ;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (tool-bar-mode 0))

(if window-system
    (zs-x-settings))

