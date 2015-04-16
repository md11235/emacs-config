;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun Y (f)
  ((lambda (x) (x x))
   (lambda (y)
     (f (lambda (&rest args)
          ((y y) args))
     ))))


aaaa ()


(defun mdsz-mark-outer-sexp ()
  (interactive)
  (let ((cur-char (char-after)))
    (cond
     ((char-equal ?\( cur-char) (mark-sexp))
     (t (if (not (eq nil (search-backward "\(" (line-beginning-position) t)))
            (mark-sexp)
          (beginning-of-line))))))

(defun mdsz-delete-inside-sexp ()
  (interactive)
  (let ((cur-char (char-after))
        start
        end)
    (cond
     ((char-equal ?\( cur-char)
      (save-excursion
        (setq start (+ (point) 1))
        (forward-sexp)
        (setq end (- (point) 1))
        )
      (kill-region start end)
      (forward-char))
     (t (if (not (eq nil (search-backward "\(" (line-beginning-position) t)))
            (progn
              (save-excursion
              (setq start (+ (point) 1))
              (forward-sexp)
              (setq end (- (point) 1))
              )
              (kill-region start end)
              (forward-char))
          (beginning-of-line))))))

(defun mdsz-delete-inside-sexp-2 ()
  (interactive)
  (let ((cur-char (char-after))
        start
        end)
    (cond
     ((char-equal ?\( cur-char)
      (mark-sexp)
      (kill-region (point) (mark)))
     (t (if (not (eq nil (search-backward "\(" (line-beginning-position) t)))
            (progn
              (save-excursion
              (setq start (+ (point) 1))
              (forward-sexp)
              (setq end (- (point) 1))
              )
              (kill-region start end)
              (forward-char))
          (beginning-of-line))))))

(defun mdsz-delete-inside-sexp ()
  (interactive)
  )

(char-equal ?\a ?\b)

