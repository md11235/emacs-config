;;-----------------------------------load directories
(let* ((dir (expand-file-name "~/site-lisp"))
       (default-directory dir))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/elisp/yasnippet/snippets/")
;; (require 'yasnippet-bundle)

;;(require 'tinyeat)
;;(global-set-key  "\M-d" 'tinyeat-forward)
;;(global-set-key (kbd "<M-backspace>") 'tinyeat-backward)

(require 'cl-info)
(setq Info-additional-directory-list '("~/data/info" "~/src/semantic-1.4.4"))

;; I-search with initial contents or region
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun isearch-forward-current-region (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the region marked."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end nil)
           (begin nil))
      (if (not (use-region-p))
          (isearch-forward regexp-p no-recursive-edit)
        ;;(setq begin (region-beginning))
        ;;(setq end (region-end))
        (setq isearch-initial-string (buffer-substring (region-beginning) (region-end)))
        (deactivate-mark)
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(global-set-key "\C-c\C-s" 'isearch-forward-current-region)

;;(setq inferior-lisp-program "/usr/bin/myclisp") ;; your Lisp system
;; (setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
;;(setq inferior-lisp-program "/usr/bin/ccl")

(require 'slime)
(slime-setup '(slime-c-p-c slime-fancy slime-autodoc slime-hyperdoc))

(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (paredit-mode +1)
                                  (setq eldoc-mode nil)))

(define-key slime-repl-mode-map (kbd "C-h") 'paredit-backward-delete)
(define-key slime-repl-mode-map (kbd "DEL") 'paredit-backward-delete)
(define-key slime-repl-mode-map (kbd "M-9") 'paredit-backward-slurp-sexp)
(define-key slime-repl-mode-map (kbd "M-0") 'paredit-forward-slurp-sexp)
(define-key slime-repl-mode-map (kbd "M-r") 'paredit-raise-sexp)

(define-key lisp-mode-map (kbd "C-h") 'paredit-backward-delete)
(define-key lisp-mode-map (kbd "DEL") 'paredit-backward-delete)
(define-key lisp-mode-map (kbd "M-9") 'paredit-backward-slurp-sexp)
(define-key lisp-mode-map (kbd "M-0") 'paredit-forward-slurp-sexp)
(define-key lisp-mode-map (kbd "M-i") 'slime-complete-symbol)
(define-key lisp-mode-map [(tab)] 'slime-complete-symbol)
(define-key lisp-mode-map (kbd "M-/") 'slime-complete-symbol)
(define-key lisp-mode-map (kbd "C-i") 'slime-indent-and-complete-symbol)
(define-key lisp-mode-map (kbd "M-r") 'paredit-raise-sexp)

;;;;;;;; php-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)
(setq php-electric-mode nil)
(setq php-stutter-mode nil)
(setq php-mode-force-pear t)

(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))

(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;; (add-hook 'php-mode-hook 'auto-make-header)
(add-hook 'php-mode-hook 'zs-set-comment-style)
(add-hook 'php-mode-hook 'my-php-mode-hook)

(defun my-php-mode-hook()
  (interactive)
  ;; offset customizations not in my-c-style
  ;;(c-set-offset 'member-init-intro '+)
  (c-set-offset 'topmost-intro-cont 2)
  (c-set-offset 'topmost-intro 2))

;; (define-key w3m-mode-map (kbd "C-x b") 'ido-switch-buffer)

(setq-default kill-whole-line nil)

(defun zs-x-settings ()
  "some settings specular to window-system"
  ;;(setq x-select-enable-clipboard t)
  ;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (tool-bar-mode 0)
  )

(if window-system
    (zs-x-settings)
  )

(require 'haskell-mode)
;; (add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;;(require 'unicad nil t)
(require 'xterm-extras)
(xterm-extra-keys)
;; (require 'icicles)
;; (require 'icicles-menu)
;; for haskell
;; (require 'haskell-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; (when window-system          ; start speedbar if we're using a window system
;;   (speedbar t))
(require 'w3m-load)
(require 'w3m)
;;(setq w3m-command-arguments
;;      (nconc w3m-command-arguments
;;             '("-o" "http_proxy=http://localhost:8118/")))
;;(require 'mime-w3m)
(setq use-file-dialog nil)
(setq suggest-key-bindings t)

(setq sentence-end-double-space nil)
(fset 'w3m-switch-to-buffer 'ido-switch-buffer)
(substitute-key-definition
 'w3m-switch-to-buffer
 'ido-switch-buffer
 w3m-mode-map global-map)
;; (if (= emacs-major-version 23)
;; 	(require 'w3m-load)
;;     )

(require 'blank-mode)
;;(require 'two-mode-mode)


;;(require 'ctypes)
;;(ctypes-auto-parse-mode 1)

(require 'visible-lines)
;; (require 'cal-china-x)
;; (setq calendar-date-display-form
;;       '((cal-china-x-calendar-display-form
;;          (mapcar (lambda (el) (string-to-number el))
;;                  (list month day year)))))

(defun my-kill-ring-save-or-kill-word ()
  "When mark active, do `kill-ring-save', otherwise just kill word
backward"
  (interactive)
  (call-interactively
   (if (and mark-active transient-mark-mode)
       'kill-region
     'backward-kill-word)))

;; from ann77@newsmth
(require 'mouse)
(when window-system
  (defun wcy-mark-some-thing-at-point()
    (interactive)
    (let* ((a (mouse-start-end (point) (point) 1))
           (start (car a))
           (end (cadr a)))
      (goto-char end)
      (push-mark)
      (goto-char start)))
  (define-key global-map (kbd "C-3") 'wcy-mark-some-thing-at-point)
  )

(global-set-key (kbd "C-x C-.") 'ska-point-to-register)
(global-set-key (kbd "C-x C-,") 'ska-jump-to-register)
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  ;;(setq zmacs-region-stays t)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  ;;(setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

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

;;css-mode
;; (autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
;; ;;(require 'css-mode)
;; (setq auto-mode-alist
;;       (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; (setq cssm-indent-function #'cssm-c-style-indenter)
;; (setq cssm-indent-level '2)

;; ;; mmm-mode
;; (add-hook 'php-mode-user-hook 'turn-on-font-lock)
;; ;;
;; (require 'mmm-mode)
;; (setq mmm-global-mode 'maybe)
;; ;;
;; ;; set up an mmm group for fancy html editing
;; (mmm-add-group
;;  'fancy-html
;;  '(
;;    (html-php-tagged
;;     :submode php-mode
;;     :face mmm-code-submode-face
;;     :front "<[?]php"
;;     :back "[?]>")
;;    (html-css-attribute
;;     :submode css-mode
;;     :face mmm-declaration-submode-face
;;     :front "style=\""
;;     :back "\"")))
;; ;;
;; ;; What files to invoke the new html-mode for?
;; (add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . html-mode))
;; (add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
;; (add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
;; ;;
;; ;; What features should be turned on in this html-mode?
;; (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
;; (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
;; (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))
;;
;; Not exactly related to editing HTML: enable editing help with mouse-3 in all sgml files
;; (defun go-bind-markup-menu-to-mouse3 ()
;;   (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))
;; ;;
;; (add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3)

;;another fold..
(setq fold-mode-prefix-key "\C-c\C-o")
(setq fold-autoclose-other-folds nil)
(require 'fold nil t)
(when (featurep 'fold)
  (add-hook 'find-file-hook 'fold-find-file-hook t))
;;mode for view tab and whitespace
(require 'blank-mode)
;;
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(setq enable-recursive-minibuffers t)
;;mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;;emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)

(defalias 'emms-read-directory-name #'read-file-name)
(defalias 'rg #'rgrep)

(global-set-key (kbd "C-l") 'recenter)
(global-set-key (kbd "<f6>") 'emms-play-directory)
(global-set-key (kbd "<f4>") 'emms-play-find)
(global-set-key (kbd "<C-f6>") 'emms-playlist-mode-go)
(setq emms-source-file-default-directory "~/music/")
;;(setq emms-source-file-default-directory nil)
(setq emms-playlist-buffer-name "*EMMS*") ;; show the playlist buffer
(setq emms-repeat-playlist t) ;; repeat the playlist

(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
;; ;; header
;; (require 'header2)
;; (setq make-header-hook '( ;;header-mode-line
;;                          header-title
;;                          header-blank
;;                          header-file-name
;;                          header-description
;;                          ;;header-status
;;                          header-author
;;                          ;;header-maintainer
;;                          ;;header-copyright
;;                          header-creation-date
;;                          ;;header-rcs-id
;;                          header-version
;;                          ;;header-sccs
;;                          header-modification-date
;;                          header-modification-author
;;                          header-update-count
;;                          header-url
;;                          header-keywords
;;                          header-compatibility
;;                          header-blank
;;                          header-lib-requires
;;                          header-end-line
;;                          header-commentary
;;                          header-blank
;;                          header-blank
;;                          header-blank
;;                          header-end-line
;;                          header-history
;;                          header-blank
;;                          header-blank
;;                          ;; header-rcs-log
;;                          header-end-line
;;                          ;;header-free-software
;;                          header-code
;;                          header-eof
;;                          ))

;; ;;(make-local-variable 'user-full-name)
;; ;;(make-local-variable 'user-mail-address)
;; (setq user-full-name "")
;; (setq user-full-name "")
;; (setq user-mail-address "")

;; ;; Update file headers when write files.
;; (add-hook 'write-file-hooks 'update-file-header)

;; ;; Create headers for file buffers in my favorite modes.
;; (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;; (add-hook 'c-mode-common-hook   'auto-make-header)

;; (setq file-header-update-alist nil)
;; (register-file-header-action "Last-Updated[ \t]*: "
;;                              'update-last-modified-date)
;; (register-file-header-action "          By[ \t]*: "
;;                              'update-last-modifier)
;; (register-file-header-action "    Update #[ \t]*: "  'update-write-count)

(defun zs-set-comment-style ()
  "Set comment-style to extra-line."
  (interactive)
  ;; <add other stuff here>
  (set (make-local-variable (quote comment-style)) 'extra-line)
  )
;; ;;(add-hook 'c-mode-hook '(setq comment-style (quote extra-line)))
;; ;;(add-hook 'c-mode-hook 'zs-set-comment-style nil t)
(add-hook 'c-mode-hook 'zs-set-comment-style)

;;(setq comment-style 'extra-line)



;;tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq ido-enable-tramp-completion t)

;; org-mode
;;(add-to-list 'load-path "PATH_TO_WHERE_YOU_UNPACKED_ORGMODE")
(require 'org)
(require 'org-install)
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

;; ignore ring bell
      (setq ring-bell-function 'ignore)

;; Non-nil means cutting and pasting uses the clipboard. This is in addition to, but in preference to, the primary selection.
(when (and (boundp 'window-system) window-system)
  (and (boundp 'x-select-enable-clipboard) (setq x-select-enable-clipboard t))
  (and (boundp 'x-cut-buffer-or-selection-value) (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))

(setq scroll-bar-mode nil)
(setq size-indication-mode t)
(setq initial-major-mode 'text-mode)
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'gtags-mode-hook '(lambda ()
                              (local-set-key (kbd "M-8") 'gtags-pop-stack)))
(add-hook 'gtags-mode-hook '(lambda ()
                              (local-set-key (kbd "M-,") 'gtags-pop-stack)))

(menu-bar-mode 0)

(show-paren-mode t)
;;(setq show-paren-style 'expression)
(setq show-paren-style 'parenthesis)
;;(setq show-paren-style 'mixed)

(require 'muse-mode)                   ; load authoring mode
(require 'muse-publish)
(require 'muse-html)          ; load publishing styles I use
;;(require 'muse-latex)
;;(require 'muse-texinfo)
;;(require 'muse-docbook)
;; (require 'info)
;; (setq Info-directory-list
;;       (cons (expand-file-name "/usr/local/emacs/info")
;;             Info-directory-list))
(require 'muse-project)

(defun replace-in-string (target old new &optional literal)
  (replace-regexp-in-string old new  target nil literal))

(require 'muse-wiki) ;;; Allow wiki-links
(setq muse-wiki-allow-nonexistent-wikiword t)
;;planner
(require 'planner)
(require 'planner-publish)
(require 'planner-id)
(setq planner-id-add-task-id-flag t)
(require 'planner-tasks-overview)
(require 'planner-cyclic)
(setq planner-cyclic-diary-nag nil)
(require 'remember)
(require 'remember-planner)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)

(setq planner-project "WikiPlanner")

(setq muse-project-alist
      '(
        ("WikiPlanner"
         ("~/net9svn/plan" ;; where your Planner pages are located
          :default "TaskPool" ;; use value of `planner-default-page'
          :major-mode planner-mode
          :visit-link planner-visit-link)

         ;; This next part is for specifying where Planner pages
         ;; should be published and what Muse publishing style to
         ;; use.  In this example, we will use the XHTML publishing
         ;; style.
         (:base "planner-html"
                ;; where files are published to
                ;; (the value of `planner-publishing-directory', if
                ;;  you have a configuration for an older version
                ;;  of Planner)
                :path "~/net9svn/plan/public"))
        ("linux_notes"                 ; my various writings
         ("~/net9svn/muse"
          :default "index"
          :major-mode muse-mode
          :visit-link planner-visit-link)
         (:base "html"
                :path "~/net9svn/muse/public"))
        ("lessen_notes"                 ; notes from lessons
         ("/opt/English/iBT/notes"
          :default "index"
          :major-mode muse-mode)
         (:base "planner-html"
                :path "/opt/English/iBT/notes/public"))
        ))

(setq muse-file-extension nil
      muse-mode-auto-p t)

(setq debug-on-error t)

(setq muse-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"core/core.css\">")
(setq muse-html-charset-default "utf-8")
(setq muse-html-meta-content-encoding "utf-8")

(defun format-time-last-changed ()
  (format-time-string "%Y-%m-%d [%H:%M]"))

(defun insert-last-changed ()
  (insert (format-time-last-changed)))

(defun update-last-changed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#lastchange\\s +[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s \\[[0-9]\\{2\\}:[0-9]\\{2\\}\\]" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert "#lastchange ")
      (insert-last-changed))))

(defun record-last-changed ()
  (setq write-contents-functions 'update-last-changed))

(setq muse-mode-hook (quote (highlight-changes-mode record-last-changed)))

(setq muse-html-footer "<div class=\"navfoot\">
      <hr>
      <table summary=\"Footer navigation\" border=\"0\" width=\"100%\">
        <col width=\"33%\"><col width=\"34%\"><col width=\"33%\">
        <tbody><tr>
          <td align=\"left\">
            <span class=\"footdate\">Last Updated:
                <lisp>
                   (format-time-string \"%D %T\"
                       (nth 5 (file-attributes
                            muse-publishing-current-file)))
                </lisp>
          </span>
          </td>
          <td align=\"center\">

            <span class=\"foothome\">
              <a href=\"/\">Home</a> / <a href=\"index.html\">Index</a>
            </span>
          </td>
          <td align=\"right\">
			<a href=\"mailto:\"></a>
          </td>
        </tr>
      </tbody></table>
	 </div>")


;;auctex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)


;;(autoload 'mpg123 "mpg123" "A Front-end to mpg123" t)

(auto-image-file-mode t)
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)
(setq inhibit-startup-message t)
(setq-default major-mode 'emacs-lisp-mode)
;; (mouse-avoidance-mode 'animate)

(autoload 'thumbs "thumbs" "Preview images in a directory." t)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)

;;;;;;;;;;;;;; javascript mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;;(autoload 'js2-mode "js2" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'actionscript-mode)
(autoload 'actionscript-mode "actionscript" nil t)
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

;;(setq js2-basic-offset 2)
;;(setq js2-use-font-lock-faces t)

;;;;;;;;; dired-single ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default diredp-hide-details-initially-flag nil)
(require 'dired)
(require 'dired+)
(require 'dired-single)

;;(setq dired-omit-files "^\\.")
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
        loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  ;;(define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  ;;   (define-key dired-mode-map "^"
  ;;     (function
  ;;      (lambda nil (interactive) (joc-dired-single-buffer ".."))))
  )

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)


;;ibuffer
(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-g") 'find-file-at-point)
(autoload 'ibuffer "ibuffer" "List buffers." t)
;;(add-to-list 'ibuffer-never-show-regexps "^\\*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; folding-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'folding)
;; (autoload 'folding-mode          "folding" "Folding mode" t)
;; (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
;; (autoload 'turn-on-folding-mode  "folding" "Folding mode" t)
;; (folding-add-to-marks-list 'php-mode "//{{{"  "//}}}"  nil t)
;; (folding-add-to-marks-list 'prolog-mode "%{{{" "%}}}" nil t)
;; (folding-add-to-marks-list 'html-mode "<!-- {{{ " "<!-- }}} -->" " -->" nil t)

(require 'paredit)
(mapc (lambda (mode)
        (let ((hook (intern (concat (symbol-name mode)
                                    "-mode-hook"))))
          (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp))
(add-hook 'lisp-mode-hook
          (lambda ()
            (make-local-variable 'paredit-mode-map)
            (define-key paredit-mode-map
              (kbd "<deletechar>") 'paredit-backward-delete)
            (highlight-parentheses-mode)))

(define-key paredit-mode-map (kbd "M-r") nil)


(push #'(lambda (endp delimiter)
          (if (and (not endp)
                   (eq ?\" (char-syntax delimiter)))
              (if (and (member major-mode '(slime-repl-mode lisp-mode))
                       (string-equal (upcase (buffer-substring-no-properties (- (point) 2) (point)))
                                     "#P"))
                  nil
                t)
            t))
      paredit-space-for-delimiter-predicates)

;;;;;;;;; html-helper-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist))
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))

;;(load "ssh.el" nil t t)
;; no backup..
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
;;;;;;;;;;;;;;; ruby-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
;;(keyboard-translate ?\C-h ?\C-?)
;; (keyboard-translate ?\C-? ?\C-d)
(global-set-key "\M-&" 'query-replace-regexp)
(global-set-key (kbd "C-2") 'set-mark-command)
;;(global-set-key "\C-c\C-m" 'set-mark-command)
(add-hook 'lisp-mode-hook (lambda ()
                            (define-key lisp-mode-map (kbd "C-c C-f") 'mark-defun)))
(global-set-key "\M-'" 'mark-sexp)
(global-set-key (kbd "C-.") 'set-mark-command)
;;(global-set-key "\M-s" 'search-forward-regexp)
(global-set-key [deletechar] 'delete-backward-char)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key (kbd "<C-return>") 'hippie-expand)
;;(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key [(control \;)] 'set-mark-command)
(global-set-key (kbd "C-2") 'set-mark-command)
(global-set-key [f12] 'shell)
(global-set-key [f9] 'list-bookmarks)
(global-set-key [f8] 'goto-line)
;;(global-set-key "\C-x\C-b" 'bs-show) ;;or 'electric-buffer-list
(global-set-key [(f3)] 'remember)
(global-set-key [(f7)] 'manual-entry)
(global-set-key [(f2)] 'planner-create-task)
(global-set-key [(f5)] 'cl-info)
;;(global-set-key (kbd "<F9> t") 'planner-create-task-from-buffer)
;;(global-set-key [(f4)] 'manual-entry)
(global-set-key [(f4)] 'planner-goto-most-recent)
(global-set-key [(f9)] 'compile)
;;(global-set-key [(f5)] 'joc-dired-magic-buffer)
(global-set-key [(meta f5)] 'joc-dired-toggle-buffer-name)
(global-set-key [(control f5)] (function
                                (lambda nil (interactive)
                                  (joc-dired-magic-buffer default-directory))))
(global-set-key [(shift f5)] (function
                              (lambda nil (interactive)
                                (message "Current directory is: %s" default-directory))))
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)
;;(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-w"     'my-kill-ring-save-or-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-x C-2") 'clone-indirect-buffer)
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-z")
(global-set-key (kbd "\e\el") 'goto-line)
(global-set-key "\C-m" 'newline-and-indent)
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

;;;;;;;;;;;;;;;;; emacs behavior settings ;;;;;;;;;;;;;;;;
;; enable visual feedback on selections
(setq-default transient-mark-mode t)
;; always end a file with a newline
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)
(setq visible-bell t)
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
    ;;(set-background-color "white")
;;(set-foreground-color "black")
;;(set default-fill-column 120)
;;(x-parse-geometry "153x60+9-6")
;;(setq-default auto-fill-mode t)
;;(set-frame-height (selected-frame) 39)
;;(set-frame-width (selected-frame) 80)
;;(set-frame-position (selected-frame) 0 0)
(set-cursor-color "purple")
(global-font-lock-mode t)
(setq-default indent-tabs-mode nil)
(setq frame-title-format (list "[Emacs]: (%f)"))
(set-face-foreground 'mode-line "firebrick")
(setq-default default-tab-width 4)
(setq column-number-mode t)
(setq mouse-yank-at-point t)
(setq kill-ring-max 200)

(setq-default fill-column 70)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
;;(auto-fill-mode 1)

(setq visible-bell t)
(setq vertical-scroll-bar nil)
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;;(set default-fill-column 120)
;;(setq frame-title-format (list "[Emacs]: (%f)"))
;;(x-parse-geometry "120x270+0-0")
(set-face-foreground 'mode-line "firebrick")
(setq-default default-tab-width 4)
(setq column-number-mode t)
;;(setq-default auto-fill-mode 1)

;; ruby mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))



;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; always end a file with a newline
;;(setq require-final-newline t)

;; Chinese font support
;; (set-keyboard-coding-system 'cn-gb-2312)
;; (set-selection-coding-system 'cn-gb-2312)
;; (set-terminal-coding-system  'cn-gb-2312)
;; (set-buffer-file-coding-system 'gb2312)
;; (setq default-buffer-file-coding-system 'gb2312)
;; (setq locale-coding-system 'gb2312)
;; (set-language-environment-coding-systems "chinese-gb"  nil)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system  'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
;;(set-language-environment-coding-systems "utf-8"  nil)
;;(set-language-environment "UTF-8"  nil)

;;(setq x-select-request-type
;;      '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; font setting
;;;;;;;;;;;;;;; window system ;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  ;; enable wheelmouse support by default
  (if (fboundp 'mwheel-install)
      (mwheel-install)))

;;;;;;;;;;;;;;;;;; hippie-expand customizaton
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-line
;;         try-expand-line-all-buffers
;;         try-expand-list
;;         try-expand-list-all-buffers
;;         try-expand-dabbrev
;;         try-expand-dabbrev-visible
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-file-name
;;         try-complete-file-name-partially
;;         try-complete-lisp-symbol
;;         try-complete-lisp-symbol-partially
;;         try-expand-whole-kill))

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-whole-kill
        ;;senator-try-expand-semantic
        try-expand-dabbrev-visible
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-line
        try-expand-line-all-buffers))


;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)


(global-set-key (kbd "<C-return>") 'hippie-expand)
;; window-system
;; enable wheelmouse support by default
;;(mwheel-install)
;; use extended compound-text coding for X clipboard
(transient-mark-mode t)

;; start the emacs server if necessary
(require 'server)

(unless (string-equal "root" (getenv "USER"))
  ;; Only start server mode if I'm not root
  (unless (server-running-p)
    (server-start)))

(put 'downcase-region 'disabled nil)

(put 'set-goal-column 'disabled nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(icicle-reminder-prompt-flag 3)
 '(lpr-command "lpr")
 '(lpr-printer-switch
   "-o PageSize=A4 -o media=a4 -o page-bottom=36 -o page-left=36 -o page-right=36 -o page-top=36 -p")
 '(muse-project-alist
   (quote
    (("WikiPlanner"
      ("~/net9svn/plan" :default "TaskPool" :major-mode planner-mode :visit-link planner-visit-link)
      (:base "planner-html" :path "~/net9svn/plan/public"))
     ("linux_notes"
      ("~/net9svn/muse" :default "index" :major-mode muse-mode)
      (:base "html" :path "~/net9svn/muse/public"))
     ("lessen_notes"
      ("/opt/English/iBT/notes" :default "index" :major-mode muse-mode)
      (:base "planner-html" :path "/opt/English/iBT/notes/public")))))
 '(org-agenda-files (quote ("~/org/gtd.org")))
 '(ps-font-family (quote Times))
 '(safe-local-variable-values
   (quote
    ((Package . CL-WHO)
     (Package . CHUNGA)
     (Package . ODD-STREAMS)
     (Package . w4)
     (Syntax . Ansi-Common-Lisp)
     (package . RFC2388)
     (Syntax . ANSI-COMMON-LISP)
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
     (eval add-hook
           (quote write-file-hooks)
           (quote time-stamp))
     (Package . CCL)
     (Package . CL-USER)
     (Package . CL-FAD)
     (Syntax . ANSI-Common-Lisp)
     (Syntax . COMMON-LISP)
     (Package . HUNCHENTOOT)
     (Base . 10)))))
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

(put 'upcase-region 'disabled nil)

;; font settings:
;;(set-default-font "DejaVu Sans Mono-12")
;;(set-default-font "Monaco-12")
;;(set-default-font "Lucida Console-12")
;;(set-default-font "Anonymous-11")
;;(set-default-font "Consolas-13")

;;;;;color-theme
(load-library "color-theme")
(require 'color-theme)

(color-theme-initialize)
(setq color-theme-is-global t)
;;(color-theme-marine)
(require 'color-theme-billc)

(if window-system
    (color-theme-feng-shui)
  ;;(color-theme-renegade)
  ;; (color-theme-billw)
  (color-theme-billc))

;; (require 'tmtheme)
;; (setq tmtheme-directory "~/site-lisp/tmthemes/")
;; (tmtheme-scan)
;; (tmtheme-Blackboard-II)

(when (and (boundp 'window-system)
           window-system
           (fboundp 'set-fontset-font))
  (set-frame-font "Anonymous Pro:pixelsize=20:antialias=subpixel")
  ;;(set-frame-font "Andale Mono-13")
  (let ((fontset "fontset-default"))
    (dolist (charset '(han symbol cjk-misc bopomofo kana hangul))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Hiragino Sans GB" :size 20)
      )))
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;                   'han '("STHeiti" . "ISO10646-1"))
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;                   'symbol '("STHeiti" . "ISO10646-1"))
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;                   'cjk-misc '("STHeiti" . "ISO10646-1"))
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;                   'bopomofo '("STHeiti" . "ISO10646-1"))
  )

;;; (add-hook 'before-make-frame-hook
;;;           (lambda (frame)
;;;             (set-variable 'color-theme-is-global nil)
;;;             (select-frame frame)
;;;             (if window-system
;;;                 (color-theme-marine)
;;;               (color-theme-tty-dark))))

;;;;;;;;; highlight-current-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'highlight-current-line)
;;(highlight-current-line-on t)
;;(set-face-background 'highlight-current-line-face "honeydew")

;; (require 'highlight-current-line)
;; (highlight-current-line-on t)
;; ;;(highlight-current-line-set-fg-color "#696969")
;; (highlight-current-line-set-fg-color nil)
;; (highlight-current-line-set-bg-color nil)


;;(require 'hl-line+)
;;(global-hl-line-mode t)
;;(hl-line-mode 1)

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (set-variable 'color-theme-is-global nil)
;;             (select-frame frame)
;;             (if window-system
;;                 (color-theme-feng-shui)
;;               (color-theme-renegade))
;;             (if window-system
;;                 (set-face-background 'highlight-current-line-face "honeydew")
;;               (set-face-background 'highlight-current-line-face "dark blue" (selected-frame)))))


;; (if (window-system)
;;     (progn
;;       (set-face-background 'highlight-current-line-face "lemon chiffon")
;;       )
;; )

;; hooks
(add-hook 'after-save-hook
          (lambda ()
            (mapcar
             (lambda (file)
               (setq file (expand-file-name file))
               (when (string= file (buffer-file-name))
                 (save-excursion (byte-compile-file file))))
             '("~/.emacs" "~/.gnus.el" "~/.wl"))))

(windmove-default-keybindings 'meta)

;; TAB indent expand
;; (defun indent-or-complete ()
;;   "Complete if point is at end of line, and indent line."
;;   (interactive)
;;   (if (looking-at "$")
;;       (hippie-expand nil))
;;        (indent-for-tab-command)
;;        )

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (hippie-expand arg)
    (indent-according-to-mode)))

(defun my-tab-fix ()
  (interactive)
  (local-set-key [(tab)] 'indent-or-expand)
  (local-set-key [(control i)] 'indent-or-expand))

(defun change-underscore-syntex-class ()
  (modify-syntax-entry ?_ "w")
  )

;; (add-hook 'c-mode-hook 'my-tab-fix)
;; (add-hook 'java-mode-hook 'my-tab-fix)
;; (add-hook 'text-mode-hook 'my-tab-fix)
;; ;; (add-hook 'sh-mode-hook 'my-tab-fix)
;; (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
;; (add-hook 'LaTeX-mode-hook 'my-tab-fix)
;; (add-hook 'php-mode-hook 'my-tab-fix)
;; (add-hook 'python-mode-hook 'my-tab-fix)
;; (add-hook 'javascript-mode-hook 'my-tab-fix)
;; (add-hook 'c-mode-common-hook 'my-tab-fix)
;; (add-hook 'haskell-mode-hook 'my-tab-fix)

(defun custom-batch-add-hook (mode-name-list hook-function-or-name)
  (mapc (lambda (mode)
          (let ((hook (intern (concat (symbol-name mode)
                                      "-mode-hook"))))
            (add-hook hook (if (functionp hook-function-or-name)
                               hook-function-or-name
                             (symbol-function hook-function-or-name)))))
        mode-name-list)
  t)

(custom-batch-add-hook '(c java text
                           ;; emacs-lisp
                           LaTeX php python javascript
                           ;; lisp inferior-lisp
                           haskell) 'my-tab-fix)
;; (custom-batch-add-hook '(c java text emacs-lisp LaTeX php python javascript lisp haskell)
;;                        (lambda () (normal-erase-is-backspace-mode 1)))


(add-hook 'text-mode-hook 'change-underscore-syntex-class)

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

(global-unset-key (kbd "C-SPC"))

(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun insert-comment-time-author ()
  "Insert a nicely formated date string and my name."
   (interactive)
   (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
   (insert " by ")
   (insert ""))

(global-set-key (kbd "C-c i") 'insert-comment-time-author)
(setq track-eol t)
(global-linum-mode 0)
(setq bookmark-save-flag 1)

;;(setq scroll-left 'disabled nil)
(setq show-trailing-whitespace t)
;;(require 'ecmascript-mode)

(global-set-key (kbd "ESC <deletechar>") 'backward-kill-word)

;;;  insert current date into the buffer at point
(defun insert-date()
      "Insert a time-stamp according to locale's date and time format."
            (interactive)
                    (insert (format-time-string "%c" (current-time))))

(global-set-key "\C-cd" 'insert-date)

;;; accelerator for what-line & goto-line
(global-set-key "\C-c\C-l" 'what-line)
(global-set-key "\C-c\C-g" 'goto-line)

;;; now set the display for smart quotes to the standard quote character
(standard-display-ascii ?\221 "\`")
(standard-display-ascii ?\222 "\'")
(standard-display-ascii ?\223 "\"")
(standard-display-ascii ?\224 "\"")
(standard-display-ascii ?\227 "\-")
(standard-display-ascii ?\225 "\*")

;; better switch buffer mode
;;(iswitchb-mode 0)

;; buffers have unique names so we know where index.html file belongs
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; (require 'recent-jump)
;; (setq recent-jump-threshold 4)
;; (setq recent-jump-ring-length 10)
;; (global-set-key (kbd "C-o") 'recent-jump-jump-backward)


(setq display-time-format '(format-time-string "%Y-%m-%d %H:%M:%S" ))

;;;                              (if time-zone " (") time-zone (if time-zone ")")
;;;                              (if mail " Mail" "")))

(setq display-time-format "%H:%M %d/%m")

;; (setq display-time-string-forms   '((substring year -2) "/" month "/" day
;;                                     " " 24-hours ":" minutes ":" seconds
;;                                     (if time-zone " (") time-zone (if time-zone ")")
;;                                     (if mail " Mail" "")))

(setq display-time-format "[%Y-%m-%d %H:%M:%S (%Z)]")

(setq
  ;; First two not needed if set display-time-string-forms directly.
 display-time-day-and-date t
 display-time-24hr-format  t
 display-time-interval     10)
(display-time-mode t)

;; 2008-10-15
(c-set-offset 'inline-open 0)
(c-add-style "zscstyle" '("awk" (c-basic-offset . 2)
                          (c-offsets-alist (inline-open . 0))))

(c-add-style "oepcstyle" '("awk" (c-basic-offset . 1)
                           (c-offsets-alist
                            (inline-open . 0)
                            (topmost-intro-cont . 0)
                            (topmost-intro . 0)
                            (statement-block-intro . 2)
                            (comment-intro . 0))))
(c-add-style "appleapcstyle" '("awk" (c-offsets-alist
                                  (inline-open . 0)
                                  (topmost-intro-cont . 4)
                                  (topmost-intro . 4)
                                  (comment-intro . 0))))
(setq c-default-style
      '((c-mode . "zscstyle") (c++-mode . "zscstyle") (java-mode . "java") (awk-mode . "awk") (other . "gnu")))


(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))



(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (defadvice indent-according-to-mode (around indent-comment-properly-in-php-mode activate)
;;               (if (and (stringp mode-name)
;;                        (string-equal mode-name "PHP"))
;;                   (let ((paren-state (c-parse-state))
;;                         (orig-point (point))
;;                         (use-orig-function t)
;;                         syn-context)
;;                     (save-excursion
;;                       (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
;;                       (let ((header-tag (buffer-substring-no-properties (- (point) 2)
;;                                                                         (+ (point) 3))))
;;                         (if (or (string-equal header-tag "<?php")
;;                                 (string-equal header-tag "<?PHP"))
;;                             (progn
;;                               (setq use-orig-function nil)
;;                               (goto-char orig-point)
;;                               (setq syn-context (c-guess-basic-syntax))
;;                               (if (assq 'topmost-intro-cont syn-context)
;;                                   (setq syn-context (cons (list (car (assoc 'topmost-intro c-offsets-alist))
;;                                                                 (cdr (assoc 'topmost-intro c-offsets-alist)))
;;                                                           (cdr syn-context))))
;;                               (c-indent-line syn-context)))))
;;                     (if use-orig-function
;;                         ad-do-it))
;;                 ad-do-it))))

;; (defun php-mode-fix-first-line-syntactic ()
;;   (save-excursion
;;     (let ((paren-state (c-parse-state)))
;;       (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
;;       (let ((header-tag (buffer-substring-no-properties (- (point) 2)
;;                                                         (+ (point) 3))))
;;         (if (or (string-equal header-tag "<?php")
;;                 (string-equal header-tag "<?PHP"))
;;             (progn
;;               (c-add-syntax 'topmost-intro (c-point 'boi))
;;               (indent-line-to (cdr (assoc 'topmost-intro c-offsets-alist)))
;;               ))))))



(require 'hyperspec)
;; (require 'cl-lookup)
;; (autoload 'cl-lookup "cl-lookup"
;;  "View the documentation on ENTRY from the Common Lisp HyperSpec, et al.")
;; (global-set-key [(control ?,) ?h] 'cl-lookup)
;; (setq cl-lookup-categories
;;       ;; Comment out category lines you don't want (like "cl-lookup-clisp" in
;;       ;; this example). Add your custom category file name as a string, which
;;       ;; then will be automatically loaded.
;;       '(:hyperspec-index           ; e.g. "", "spec" "CLHS"
;;         :hyperspec-chapters        ; e.g. [index], [syntax]
;;         :format-control-characters ; e.g. "~C: Character", "~%: Newline"
;;         :reader-macro-characters   ; e.g. "(", "#'", "#b", "#+"
;;         :loop                      ; e.g. loop:with, loop:collect
;;         :arguments                 ; e.g. :test, :key, :eof-error-p
;;         :concepts                  ; e.g. "lambda lists:", "character names:"
;;         "cl-lookup-glossary"       ; e.g. {absolute}, {binding}
;;         "cl-lookup-mop"            ; e.g. add-dependent, ensure-class

;;         ;; implementation specific categories
;;         ;; "cl-lookup-clisp"          ; e.g. ext:cd

;;         ;; library categories
;;         "cl-lookup-ppcre"          ; e.g. cl-ppcrfke:parse-tree-synonym
;;         ))

(setq common-lisp-hyperspec-root "~/data/info/HyperSpec/")
(setq browse-url-browser-function '(("." . w3m-browse-url)))
;;(add-hook 'c-special-indent-hook 'php-mode-fix-first-line-syntactic)

;; (defadvice ido-find-file (before find-file-at-point activate)
;;   "run `find-file-at-point' before ido-find-file"
;;   (interactive)
;;   (find-file-at-point))


;;;;;;;;;;;;;;;;;;;; under Windows: maxmize frame on startup
(when (fboundp 'w32-send-sys-command)
(defun w32-restore-frame ()
    "Restore a minimized frame"
      (interactive)
        (w32-send-sys-command 61728))
(defun w32-maximize-frame ()
    "Maximize the current frame"
      (interactive)
        (w32-send-sys-command 61488)))

;; use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
;; Don't add newlines to end of buffer when scrolling
(setq next-line-add-newlines nil)
;; Always end a file with a newline
;;(setq require-final-newline t)
;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; (mapc (lambda (mode)
;;         (let ((hook (intern (concat (symbol-name mode)
;;                                     "-mode-hook"))))
;;           (add-hook hook (lambda () (normal-erase-is-backspace-mode 1)))))
;;       '(php lisp emacs-lisp inferior-lisp python ruby c))


;; (kbd "<deletechar>")
;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (make-local-variable 'paredit-mode-map)
;;             (define-key paredit-mode-map
;;               [deletechar] 'paredit-backward-delete)))

;;(require 'redshank)


(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (when (and (fboundp 'w32-maximize-frame) (eq 'windows-nt system-type))
                 (w32-maximize-frame))
               (when (and (boundp 'window-system)
                          window-system
                          (fboundp 'set-fontset-font))
                 (set-frame-font "Dejavu Sans Mono:pixelsize=14:antialias=subpixel")
                 (let ((fontset (frame-parameter new-frame 'font)))
                   (dolist (charset '(han symbol cjk-misc bopomofo))
                     (set-fontset-font fontset charset '("YaHei consolas Hybrid" . "unicode-bmp"))))
                 ;; (set-fontset-font (frame-parameter new-frame 'font)
;;                                    'han '("YaHei Consolas Hybrid" . "unicode-bmp"))
;;                  (set-fontset-font (frame-parameter new-frame 'font)
;;                                    'symbol '("YaHei Consolas Hybrid" . "unicode-bmp"))
;;                  (set-fontset-font (frame-parameter new-frame 'font)
;;                                    'cjk-misc '("YaHei Consolas Hybrid" . "unicode-bmp"))
;;                  (set-fontset-font (frame-parameter new-frame 'font)
;;                                    'bopomofo '("YaHei Consolas Hybrid" . "unicode-bmp"))
                 )))

(add-hook 'window-setup-hook
          '(lambda () (set-cursor-color "purple")))
(add-hook 'after-make-frame-functions
          '(lambda (f) (with-selected-frame f (set-cursor-color "purple"))))

;; jump quickly to the slime sbcl buffer
(global-set-key (kbd "M-o x") '(lambda ()
                                 (interactive)
                                 (switch-to-buffer "*slime-repl sbcl*")))0;136;0c



;;;;;;;;;;;;;;;; testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/src/semantic-1.4.4/")
;; (require 'semantic)
;; (require 'semantic-bnf)
;; (require 'semantic-make)

(defalias 'exit 'save-buffers-kill-emacs)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; vc sector
(require 'vc)
(setq vc-cvs-diff-switches "-u")

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

;;(define-key emacs-lisp-mode-map (kbd "M-r") 'paredit-raise-sexp)
(define-key paredit-mode-map (kbd "M-r") 'paredit-raise-sexp)

;; (add-to-list 'load-path "~/src/clojure-mode")
;; (require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))



(require 'auto-complete)
(require 'auto-complete-clang)


(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
    (when (and isearch-forward
               isearch-other-end)
      (goto-char isearch-other-end)))
(defadvice isearch-exit (after my-goto-match-beginning activate)
    "Go to beginning of match."
      (when (and isearch-forward
               isearch-other-end)
        (goto-char isearch-other-end)))

;; (message "%s" load-path)



;; you should probably comment the next line or replace ~remy by another path
(setq load-path (cons "~remy/emacs/" load-path))

(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; desktop
;;(set-frame-position (selected-frame) 0 0)
(require 'desktop)
(desktop-save-mode 1)
(setq history-length 250)
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-globals-to-save 'file-name-history)
;; (condition-case nil
;;     (desktop-read)
;;   (error nil))
;;; desktop-override-stale-locks.el begins here
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;;; desktop-override-stale-locks.el ends here


;; (setq desktop-save t)
;; (setq desktop-buffers-not-to-save
;;       (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;               "\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;;               "\\)$"))
;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
