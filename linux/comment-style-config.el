(defun zs-set-comment-style ()
  "Set comment-style to extra-line."
  (interactive)
  ;; <add other stuff here>
  (set (make-local-variable (quote comment-style)) 'extra-line))
;; ;;(add-hook 'c-mode-hook '(setq comment-style (quote extra-line)))
;; ;;(add-hook 'c-mode-hook 'zs-set-comment-style nil t)
(add-hook 'c-mode-hook 'zs-set-comment-style)

;;(setq comment-style 'extra-line)
