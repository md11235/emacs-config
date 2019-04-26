;;;;;color-theme
;; (load-library "color-theme")
(require 'color-theme)

(color-theme-initialize)
(setq color-theme-is-global t)

;;(color-theme-marine)
(require 'color-theme-billc)

(if (display-graphic-p) ;; window-system
    (color-theme-feng-shui)
  ;;(color-theme-renegade)
  (color-theme-arjen))

;; (add-hook 'before-make-frame-hook
;;           (lambda (frame)
;;             (set-variable 'color-theme-is-global t)
;;             (select-frame frame)
;;             (if (display-graphic-p)
;;                 (color-theme-feng-shui)
;;               (color-theme-billc))))

