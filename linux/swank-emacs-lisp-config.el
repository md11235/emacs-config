;; todo: possibly semantic analysis
(defun mds-el-goto-definition ()
  (interactive)
  ;; if I could get the thing at point, then the argument "function" is not required.
  (setq mds-el-location-stack
        (cons (cons
               (current-buffer) (point))
              mds-el-location-stack))
  (find-function-do-it (function-called-at-point) nil 'switch-to-buffer))

(defun mds-el-jump-back ()
  (interactive)
  (let ((location (car mds-el-location-stack)))
    (switch-to-buffer (car location))
    (goto-char (cdr location))
    (setq mds-el-location-stack (cdr mds-el-location-stack))))
  
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "M-.") 'mds-el-goto-definition)
                                  (local-set-key (kbd "M-,") 'mds-el-jump-back)))

