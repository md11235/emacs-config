;;; erc-fill.el --- Filling IRC messages in various ways

;; Copyright (C) 2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;;         Mario Lang <mlang@delysid.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcFilling

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package implements filling of messages sent and received.  Use
;; `erc-fill-mode' to switch it on.  Customize `erc-fill-function' to
;; change the style.

;;; Code:

(require 'erc)
(require 'erc-stamp); for the timestamp stuff

(defgroup erc-fill nil
  "Filling means to reformat long lines in different ways."
  :group 'erc)

;;;###autoload (autoload 'erc-fill-mode "erc-fill" nil t)
(erc-define-minor-mode erc-fill-mode
  "Toggle ERC fill mode.
With numeric arg, turn ERC fill mode on if and only if arg is
positive.  In ERC fill mode, messages in the channel buffers are
filled."
  nil nil nil
  :global t :group 'erc-fill
  (if erc-fill-mode
      (erc-fill-enable)
    (erc-fill-disable)))

(defun erc-fill-enable ()
  "Setup hooks for `erc-fill-mode'."
  (interactive)
  (add-hook 'erc-insert-modify-hook 'erc-fill)
  (add-hook 'erc-send-modify-hook 'erc-fill))

(defun erc-fill-disable ()
  "Cleanup hooks, disable `erc-fill-mode'."
  (interactive)
  (remove-hook 'erc-insert-modify-hook 'erc-fill)
  (remove-hook 'erc-send-modify-hook 'erc-fill))

(defcustom erc-fill-prefix nil
  "Values used as `fill-prefix' for `erc-fill-variable'.
nil means fill with space, a string means fill with this string."
  :group 'erc-fill
  :type '(choice (const nil) string))

(defcustom erc-fill-function 'erc-fill-variable
  "Function to use for filling messages.

Variable Filling with an `erc-fill-prefix' of nil:

<shortnick> this is a very very very long message with no
	    meaning at all

Variable Filling with an `erc-fill-prefix' of four spaces:

<shortnick> this is a very very very long message with no
    meaning at all

Static Filling with `erc-fill-static-center' of 27:

		<shortnick> foo bar baz
	 <a-very-long-nick> foo bar baz quuuuux
		<shortnick> this is a very very very long message with no
			    meaning at all

These two styles are implemented using `erc-fill-variable' and
`erc-fill-static'.  You can, of course, define your own filling
function.  Narrowing to the region in question is in effect while your
function is called."
  :group 'erc-fill
  :type '(choice (const :tag "Variable Filling" erc-fill-variable)
                 (const :tag "Static Filling" erc-fill-static)
                 function))

(defcustom erc-fill-static-center 27
  "Column around which all statically filled messages will be
centered.  This column denotes the point where the ' ' character
between <nickname> and the entered text will be put, thus aligning
nick names right and text left."
  :group 'erc-fill
  :type 'integer)

(defcustom erc-fill-variable-maximum-indentation 17
  "If we indent a line after a long nick, don't indent more then this
characters. Set to nil to disable."
  :group 'erc-fill
  :type 'integer)

(defcustom erc-fill-column 78
  "The column at which a filled paragraph is broken."
  :group 'erc-fill
  :type 'integer)

;;;###autoload
(defun erc-fill ()
  "Fill a region using the function referenced in `erc-fill-function'.
You can put this on `erc-insert-modify-hook' and/or `erc-send-modify-hook'."
  (unless (erc-string-invisible-p (buffer-substring (point-min) (point-max)))
    (when erc-fill-function
      ;; skip initial empty lines
      (goto-char (point-min))
      (save-match-data
        (while (and (looking-at "[ \t\n]*$")
                    (= (forward-line 1) 0))))
      (unless (eobp)
        (save-restriction
          (narrow-to-region (point) (point-max))
          (funcall erc-fill-function))))))

(defun erc-fill-static ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (save-match-data
    (goto-char (point-min))
    (looking-at "^\\(\\S-+\\)")
    (let ((nick (match-string 1)))
        (let ((fill-column (- erc-fill-column (erc-timestamp-offset)))
              (fill-prefix (make-string erc-fill-static-center 32)))
          (insert (make-string (max 0 (- erc-fill-static-center
                                         (length nick) 1))
                               32))
          (erc-fill-regarding-timestamp))
        (erc-restore-text-properties))))

(defun erc-fill-variable ()
  "Fill from `point-min' to `point-max'."
  (let ((fill-prefix erc-fill-prefix)
        (fill-column (or erc-fill-column fill-column)))
    (goto-char (point-min))
    (if fill-prefix
        (let ((first-line-offset (make-string (erc-timestamp-offset) 32)))
          (insert first-line-offset)
          (fill-region (point-min) (point-max) t t)
          (goto-char (point-min))
          (delete-char (length first-line-offset)))
      (save-match-data
        (let* ((nickp (looking-at "^\\(\\S-+\\)"))
               (nick (if nickp
                         (match-string 1)
                       ""))
               (fill-column (- erc-fill-column (erc-timestamp-offset)))
               (fill-prefix (make-string (min (+ 1 (length nick))
                                              (- fill-column 1)
                                              (or erc-fill-variable-maximum-indentation
                                                  fill-column))
                                         32)))
          (erc-fill-regarding-timestamp))))
    (erc-restore-text-properties)))

(defun erc-fill-regarding-timestamp ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (fill-region (point-min) (point-max) t t)
  (goto-char (point-min))
  (forward-line)
  (indent-rigidly (point) (point-max) (erc-timestamp-offset)))

(defun erc-timestamp-offset ()
  "Get length of timestamp if inserted left."
  (if (and (boundp 'erc-timestamp-format)
           erc-timestamp-format
           (eq erc-insert-timestamp-function 'erc-insert-timestamp-left)
           (not erc-hide-timestamps))
      (length (format-time-string erc-timestamp-format))
    0))

(defun erc-restore-text-properties ()
  "Restore the property 'erc-parsed for the region."
  (let* ((parsed-posn (text-property-not-all (point-min) (point-max)
                                             'erc-parsed nil))
         (parsed-prop (when parsed-posn
                        (get-text-property parsed-posn 'erc-parsed))))
    (put-text-property (point-min) (point-max) 'erc-parsed parsed-prop)))

(provide 'erc-fill)

;;; erc-fill.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;; arch-tag: 89224581-c2c2-4e26-92e5-e3a390dc516a
