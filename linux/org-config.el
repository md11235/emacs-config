;; org-mode
;;(add-to-list 'load-path "PATH_TO_WHERE_YOU_UNPACKED_ORGMODE")
(require 'org)
;; (require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/gtd.org"
                             "~/org/lab.org"
                             "~/org/learn.org"
                             "~/org/home.org"
                             "~/org/ent.org"))

;; (add-hook 'org-mode-hook '(lambda () (local-unset-key [(meta return)])))
;; (add-hook 'org-mode-hook '(lambda () (local-set-key [(meta return)] 'org-insert-todo-heading)))
(setq org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"org.css\">")

(setq org-export-latex-default-packages-alist '(("T1" "fontenc" t)
                                                ("" "fixltx2e" nil)
                                                ("" "graphicx" t)
                                                ("" "longtable" nil)
                                                ("" "float" nil)
                                                ("" "wrapfig" nil)
                                                ("" "soul" t)
                                                ("" "textcomp" t)
                                                ("" "marvosym" t)
                                                ("" "wasysym" t)
                                                ("" "latexsym" t)
                                                ("" "amssymb" t)
                                                ("" "hyperref" nil)
                                                "\\tolerance=1000"))

