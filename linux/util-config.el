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

(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;; (defun insert-comment-time-author ()
;;   "Insert a nicely formated date string and my name."
;;    (interactive)
;;    (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
;;    (insert " by ")
;;    (insert ""))

(global-set-key (kbd "C-c i") 'insert-comment-time-author)

;;;  insert current date into the buffer at point
(defun insert-date()
      "Insert a time-stamp according to locale's date and time format."
            (interactive)
                    (insert (format-time-string "%c" (current-time))))

(global-set-key "\C-cd" 'insert-date)

;;; accelerator for what-line & goto-line
(global-set-key "\C-c\C-l" 'what-line)
(global-set-key "\C-c\C-g" 'goto-line)

(defalias 'rg #'rgrep)

(global-set-key (kbd "C-l") 'recenter)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "%" 'match-paren)

(defun my-kill-ring-save-or-kill-word ()
  "When mark active, do `kill-ring-save', otherwise just kill word
backward"
  (interactive)
  (call-interactively
   (if (and mark-active transient-mark-mode)
       'kill-region
     'backward-kill-word)))


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
