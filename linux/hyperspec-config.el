(require 'hyperspec)

;; (require 'cl-info)
;; (require 'cl-lookup)

;; (setq Info-additional-directory-list '("~/data/info"))
(setq common-lisp-hyperspec-root (expand-file-name (concat emacs-config-dir "../shared/data/info/HyperSpec/")))

(setq browse-url-browser-function '(("." . w3m-browse-url)))
;;(add-hook 'c-special-indent-hook 'php-mode-fix-first-line-syntactic)

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
