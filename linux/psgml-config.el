;;psgml-mode
(setq sgml-set-face t)
(setq sgml-auto-activate-dtd t)
(setq sgml-indent-data t)
(setq sgml-markup-faces '(
                          (start-tag . font-lock-keyword-face)
                          (end-tag . font-lock-keyword-face)
                          (comment . font-lock-comment-face)
                          (pi . font-lock-constant-face) ;; <?xml?>
                          (sgml . font-lock-type-face)
                          (doctype . bold)
                          (entity . italic)
                          (shortref . font-lock-reference-face)))

;;
;; Not exactly related to editing HTML: enable editing help with mouse-3 in all sgml files
;; (defun go-bind-markup-menu-to-mouse3 ()
;;   (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))
;; ;;
;; (add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3)

