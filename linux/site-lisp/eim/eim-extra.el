;; -*- coding: utf-8 -*-
;;; eim-extra.el --- provide extra function for chinese input method 

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: eim-extra.el,v 0.0 2006/07/16 02:28:05 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'eim-extra)

;;;_* Code:

(provide 'eim-extra)
(eval-when-compile
  (require 'cl))

;;;_. handle punctuation
(defun eim-read-punctuation (package)
  (let ((eim-current-package package)
	buf punc-list punc)
    (setq buf (cdr (assoc "buffer" (car (eim-buffer-list)))))
    (save-excursion
      (set-buffer buf)
      (save-restriction
        (widen)
        (let ((region (eim-section-region "Punctuation")))
          (goto-char (car region))
          (while (< (point) (cdr region))
            (setq punc (eim-line-content))
            (if (> (length punc) 3)
                (error "标点不支持多个转换！"))
            (add-to-list 'punc-list punc)
            (forward-line 1)))))
    punc-list))

(defun eim-punc-translate (punc-list char)
  (if (< char ? )
      ""
    (let ((str (char-to-string char))
          punc)
      (if (setq punc (cdr (assoc str punc-list)))
          (if (= (safe-length punc) 1)
              (car punc)
            (setcdr (cdr punc) (not (cddr punc)))
            (if (cddr punc)
                (car punc)
              (nth 1 punc)))
        str))))

;;;_. load and save history
(defun eim-load-history (history-file package)
  (let* ((eim-current-package package)
         (history (eim-history))
         item)
    (when (file-exists-p history-file)
      (with-current-buffer (find-file-noselect history-file)
        (goto-char (point-min))
        (while (not (eobp))
          (if (setq item (eim-line-content))
              (puthash (car item)
                       `(nil ("pos" . ,(string-to-number (cadr item))))
                       history))
          (forward-line 1))
        (kill-buffer (current-buffer))))))

(defun eim-save-history (history-file package)
  (interactive)
  (let* ((eim-current-package eim-wb-package)
         (history (eim-history)))
    (with-temp-buffer 
      (erase-buffer)
      (let (pos)
        (maphash (lambda (key val)
                   (unless (= (setq pos (cdr (assoc "pos" (cdr val)))) 1)
                     (insert key " " (number-to-string pos) "\n")))
                 history))
      (write-file history-file))))

;;;_. 增加两个快速选择的按键
(defun eim-quick-select-1 ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car eim-current-choices)
      (let ((index (eim-page-start))
            (end (eim-page-end)))
        (if (>= index end)
            (eim-append-string (eim-translate last-command-event))
          (eim-remember-select (1+ index))
          (setq eim-current-str (eim-choice (nth index (car eim-current-choices))))))
    (eim-append-string (eim-translate last-command-event)))
  (eim-terminate-translation))

(defun eim-quick-select-2 ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car eim-current-choices)
      (let ((index (1+ (eim-page-start)))
            (end (eim-page-end)))
        (if (>= index end)
            (eim-append-string (eim-translate last-command-event))
          (eim-remember-select (1+ index))
          (setq eim-current-str (eim-choice (nth index (car eim-current-choices))))))
    (eim-append-string (eim-translate last-command-event)))
  (eim-terminate-translation))

;;;_. 一个快速插入英文的命令。按自己的需要绑定到 "\\"
(defun eim-insert-ascii ()
  (interactive)
  (if current-input-method
      (insert (read-from-minibuffer ""))
    (insert-char last-command-event 1)))

;;;_. char table
(defun eim-make-char-table (chars table)
  "Set `eim-char-database'"
  (dolist (char chars)
    (let ((code (car char)))
      (dolist (c (cdr char))
        (set (intern c table) code)))))

(defsubst eim-get-char-code (char table)
  "Get the code of the character CHAR"
  (symbol-value (intern-soft (char-to-string char) table)))


;;; eim-extra.el ends here
