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
                           haskell)
                       'my-tab-fix)

;; (custom-batch-add-hook '(c java text emacs-lisp LaTeX php python javascript lisp haskell)
;;                        (lambda () (normal-erase-is-backspace-mode 1)))
