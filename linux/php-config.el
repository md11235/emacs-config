;;;;;;;; php-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)
(setq php-electric-mode nil)
(setq php-stutter-mode nil)
(setq php-mode-force-pear t)

(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))

(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

(add-hook 'php-mode-hook 'auto-make-header)
(add-hook 'php-mode-hook 'zs-set-comment-style)
(add-hook 'php-mode-hook 'my-php-mode-hook)

(defun my-php-mode-hook()
  (interactive)
  ;; offset customizations not in my-c-style
  ;;(c-set-offset 'member-init-intro '+)
  (c-set-offset 'topmost-intro-cont 2)
  (c-set-offset 'topmost-intro 2))

(add-hook 'php-mode-hook
          (lambda ()
            (defadvice indent-according-to-mode (around indent-comment-properly-in-php-mode activate)
              (if (and (stringp mode-name)
                       (string-equal mode-name "PHP"))
                  (let ((paren-state (c-parse-state))
                        (orig-point (point))
                        (use-orig-function t)
                        syn-context)
                    (save-excursion
                      (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
                      (let ((header-tag (buffer-substring-no-properties (- (point) 2)
                                                                        (+ (point) 3))))
                        (if (or (string-equal header-tag "<?php")
                                (string-equal header-tag "<?PHP"))
                            (progn
                              (setq use-orig-function nil)
                              (goto-char orig-point)
                              (setq syn-context (c-guess-basic-syntax))
                              (if (assq 'topmost-intro-cont syn-context)
                                  (setq syn-context (cons (list (car (assoc 'topmost-intro c-offsets-alist))
                                                                (cdr (assoc 'topmost-intro c-offsets-alist)))
                                                          (cdr syn-context))))
                              (c-indent-line syn-context)))))
                    (if use-orig-function
                        ad-do-it))
                ad-do-it))))

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
