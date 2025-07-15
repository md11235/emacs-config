;;;;;color-theme
;; (load-library "color-theme")
(require 'color-theme-modern)

(add-to-list 'custom-theme-load-path
             (file-name-as-directory
              (expand-file-name "~/utility-config/emacs-config/replace-colorthemes")))

(load-theme 'billw t t)
(enable-theme 'billw)

;; (if (display-graphic-p) ;; window-system
;;     (color-theme-feng-shui)
;;   ;;(color-theme-renegade)
;;   (color-theme-arjen))

;; (color-theme-initialize)
;; (setq color-theme-is-global t)

;; ;;(color-theme-marine)
;; (require 'color-theme-billc)



;; (add-hook 'before-make-frame-hook
;;           (lambda (frame)
;;             (set-variable 'color-theme-is-global t)
;;             (select-frame frame)
;;             (if (display-graphic-p)
;;                 (color-theme-feng-shui)
;;               (color-theme-billc))))

