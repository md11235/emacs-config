;; (setq emacs-config-dir (file-name-directory load-file-name))



;; ;;-----------------------------------load directories
;; (let* ((dir (expand-file-name "~/site-lisp"))
;;        (default-directory dir))
;;   (when (file-directory-p dir)
;;     (add-to-list 'load-path dir)
;;     (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;         (normal-top-level-add-subdirs-to-load-path))))

;;-----------------------------------load directories

;; (let* ((dir (expand-file-name (concat emacs-config-dir "site-lisp")))
;;        (default-directory dir))
;;   (when (file-directory-p dir)
;;     (add-to-list 'load-path dir)
;;     (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;         (normal-top-level-add-subdirs-to-load-path))))



;; ;; Load nxml according to the instructions, ie something like:
;; (load "rng-auto.el")
;; (require 'nxhtml)

;; ;; Then autoload nxhtml-mode:
;; (autoload 'nxhtml-mode "nxhtml" "Mode for editing XHTML files - based on nxml-mode." t)

;; ;; For file associations you can use:
;; (require 'fmode)
;; (fmode-replace-default-mode 'html-mode 'nxhtml-mode)
;; (fmode-replace-default-mode 'xml-mode 'nxml-mode)

;; (custom-set-variables
;;  `(nxhtml-default-encoding (quote utf-8))
;;  `(nxhtml-load t) ;;Autoloading NXHTML Mode with css-js-php support
;;  `(nxhtml-skip-welcome t))

;; better switch buffer mode
;;(iswitchb-mode 0)

;; (defadvice ido-find-file (before find-file-at-point activate)
;;   "run `find-file-at-point' before ido-find-file"
;;   (interactive)
;;   (find-file-at-point))



;;;;;;;;;;;;;;;; testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/src/semantic-1.4.4/")
;; (require 'semantic)
;; (require 'semantic-bnf)
;; (require 'semantic-make)

;; (add-to-list 'load-path "~/src/clojure-mode")
;; (require 'clojure-mode)

;; (message "%s" load-path)

;; you should probably comment the next line or replace ~remy by another path
;; (setq load-path (cons "~remy/emacs/" load-path))



;; (require 'tmtheme)
;; (setq tmtheme-directory "~/site-lisp/tmthemes/")
;; (tmtheme-scan)
;; (tmtheme-Blackboard-II)
