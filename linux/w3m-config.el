(require 'w3m-load)

;; (define-key w3m-mode-map (kbd "C-x b") 'ido-switch-buffer)

;;(setq w3m-command-arguments
;;      (nconc w3m-command-arguments
;;             '("-o" "http_proxy=http://localhost:8118/")))
;;(require 'mime-w3m)

(fset 'w3m-switch-to-buffer 'ido-switch-buffer)
;; (substitute-key-definition 'w3m-switch-to-buffer 'ido-switch-buffer
;;                            w3m-mode-map global-map)


;; (if (= emacs-major-version 23)
;; 	(require 'w3m-load))
