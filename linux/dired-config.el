;;;; dired

(setq-default diredp-hide-details-initially-flag nil)
(require 'dired)
(require 'dired-single)
(require 'dired+)

(defun mdsz-dired-view-file ()
 (interactive)
 (case system-type
   ('windows-nt (dired-w32-browser))
   (t (dired-view-file))))

;;(setq dired-omit-files "^\\.")
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
        loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map (kbd "v") 'mdsz-dired-view-file)
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

(setq dired-recursive-deletes 'top dired-recursive-copies 'top dired-dwim-target t)
(add-hook 'dired-mode-hook (lambda ()
                             (local-set-key (kbd "C-c C-r") 'wdired-change-to-wdired-mode)))

(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-omit-files "^\\.")

;;; w32-browser
;; (require 'w32-browser)

;; (defun w32-browser (doc)
;;   "Browse to a particular file/URL using default web browser"
;;   (w32-shell-execute 1 doc))

;; (eval-after-load "dired"
;;   '(define-key dired-mode-map [f3]
;;      (lambda ()
;;        (interactive)
;;        (w32-browser
;;         (dired-replace-in-string
;;          "/" "\\"
;;          (dired-get-filename))))))

;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "<C-return>")
;;                            (lambda nil
;;                              (interactive)
;;                              (w32-browser
;;                               (dired-replace-in-string "/" "\\" (dired-get-filename)))))))

;;;; dired end
