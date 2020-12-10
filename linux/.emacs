(load-file "/home/zhang/utility-config/emacs-config/linux/refactoring-in-progress.emacs")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default))
 '(icicle-reminder-prompt-flag 3)
 '(lpr-command "lpr")
 '(lpr-printer-switch
   "-o PageSize=A4 -o media=a4 -o page-bottom=36 -o page-left=36 -o page-right=36 -o page-top=36 -p")
 '(muse-project-alist
   '(("WikiPlanner"
      ("~/net9svn/plan" :default "TaskPool" :major-mode planner-mode :visit-link planner-visit-link)
      (:base "planner-html" :path "~/net9svn/plan/public"))
     ("linux_notes"
      ("~/net9svn/muse" :default "index" :major-mode muse-mode)
      (:base "html" :path "~/net9svn/muse/public"))
     ("lessen_notes"
      ("/opt/English/iBT/notes" :default "index" :major-mode muse-mode)
      (:base "planner-html" :path "/opt/English/iBT/notes/public"))))
 '(org-agenda-files '("~/org/gtd.org"))
 '(package-selected-packages
   '(smart-mode-line matlab-mode debug-print rope-read-mode jedi projectile material-theme markdown-mode+ elpy cider))
 '(ps-font-family 'Times)
 '(safe-local-variable-values
   '((Syntax . ANSI-COMMON-LISP)
     (Package . ccl)
     (Package . SYSTEM)
     (Package . C)
     (Syntax . Common-Lisp)
     (Mode . C++)
     (Mode . Emacs-Lisp)
     (Mode . LISP)
     (Mode . Lisp)
     (Package CCL :use CL)
     (Package X86 :use CL)
     (Package . umweb)
     (package . asdf)
     (Package . modlisp)
     (Package . UFFI)
     (Package ARCH :use CL)
     (Package X8632 :use CL)
     (package . net\.aserve)
     (Package . FLEXI-STREAMS)
     (fill-column)
     (Package . DRAKMA)
     (eval add-hook 'write-file-hooks 'time-stamp)
     (Package . CCL)
     (Package . CL-USER)
     (Package . CL-FAD)
     (Syntax . ANSI-Common-Lisp)
     (Syntax . COMMON-LISP)
     (Package . HUNCHENTOOT)
     (Base . 10))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(info-title-1 ((t (:inherit variable-pitch :weight semi-bold :height 2.2))))
 '(info-title-2 ((t (:inherit variable-pitch :weight semi-bold :height 1.8))))
 '(info-title-3 ((t (:inherit variable-pitch :weight semi-bold :height 1.6))))
 '(info-title-4 ((t (:inherit variable-pitch :weight semi-bold :height 1.4))))
 '(variable-pitch ((t (:inherit default :family "Lucida Grande")))))
