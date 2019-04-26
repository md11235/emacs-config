(defun change-underscore-syntex-class ()
  (modify-syntax-entry ?_ "w"))

(add-hook 'text-mode-hook 'change-underscore-syntex-class)
