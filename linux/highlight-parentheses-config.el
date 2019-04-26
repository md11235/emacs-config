(require 'highlight-parentheses)

(setq hl-paren-colors
      (make-list 20 "magenta1")
      ;; '(;"#8f8f8f" ; this comes from Zenburn
      ;;                                   ; and I guess I'll try to make the far-outer parens look like this
      ;;   "orange1" "yellow1" "greenyellow" "green1"
      ;;   "springgreen1" "cyan1" "slateblue1" "magenta1" "purple")
      )


(add-hook 'lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode)
             (setq autopair-handle-action-fns
                   (list 'autopair-default-handle-action
                         '(lambda (action pair pos-before)
                            (hl-paren-color-update))))))


(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode)
             (setq autopair-handle-action-fns
                   (list 'autopair-default-handle-action
                         '(lambda (action pair pos-before)
                            (hl-paren-color-update))))))


(add-hook 'lisp-mode-hook
          (lambda ()
            (make-local-variable 'paredit-mode-map)
            (define-key paredit-mode-map
              (kbd "<deletechar>") 'paredit-backward-delete)
            (highlight-parentheses-mode)))

