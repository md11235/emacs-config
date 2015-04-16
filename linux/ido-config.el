(ido-mode 1)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(icomplete-mode 1)

(require 'icomplete+)
;; (defun ido-execute ()
;;   (interactive)
;;   (call-interactively
;;    (intern
;;     (ido-completing-read
;;      "M-x "
;;      (let (cmd-list)
;;        (mapatoms (lambda (S) (when (commandp S) (setq cmd-list (cons (format "%S" S) cmd-list)))))
;;        cmd-list)))))

;; (global-set-key "\M-x" 'ido-execute)
