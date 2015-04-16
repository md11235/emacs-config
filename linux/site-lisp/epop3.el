;;; epop3.el -- extended pop3.el functions (RFC 1725/1939)
;; Copyright (c) 1997 Franklin Lee

;; Author:      Franklin Lee <flee@lehman.com>
;; Keywords:    mail, pop3
;; Version:     0.5

;; This file is NOT part of GNU Emacs.

;; This program code is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program code is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The code below implements commands not in pop3.el.  Some of these
;; commands (UIDL, TOP) are optional POP3 commands introduced in RFC 1725.
;; The LIST command is implemented for the use of Mail User Agents which
;; may want to access the LIST command information.
;;
;;
;; Many thanks to Rich Pieri for writing pop3.el, and to Stainless Steel
;; Rat <ratinox@peorth.gweep.net> for his clarifications and comments.
;;

;;; History:
;;  --------
;;  11/1997 initial version.

(require 'pop3)
(require 'cl)

;;; Code:

(defvar pop3-extended-response-beginning nil
  "Start of the region containing the last pop3 extended response.
This does NOT include the initial response from `pop3-read-response'.")

(defvar pop3-extended-response-end nil
  "End of the region containing the last pop3 extended response.")

(defun pop3-uidl (process &optional msgno)
  "Return the results of a UIDL command.
If UIDL is unsupported on this mail server or if msgno is invalid, return nil.
Otherwise, return a list in the form

   (N (1 UIDL-1) (2 UIDL-2) ... (N UIDL-N))

where

   N is an integer for the number of UIDLs returned (could be 0)
   UIDL-n is a string."

  (if msgno
	  (pop3-send-command process (format "UIDL %d" msgno))
	(pop3-send-command process "UIDL"))

  (let ((uidl-not-supported nil))
	(condition-case ()
		(pop3-read-response process t)
	  (error (setq uidl-not-supported t)))

	(unless uidl-not-supported
	  (let ((retlist '())
			(uidl nil)
			(msgno nil))
		(save-excursion
		  (pop3-get-extended-response process)
		  (set-buffer (process-buffer process))
		  (goto-char pop3-extended-response-beginning)

		  (while (looking-at "\\([^ \n\t]*\\) \\([^ \n\t]*\\)")
			(setq msgno (string-to-int
						 (buffer-substring (match-beginning 1) (match-end 1))))
			(setq uidl (buffer-substring (match-beginning 2) (match-end 2)))
			(push (cons msgno (list uidl)) retlist)
			(beginning-of-line 2))
		  (cons (length retlist) (nreverse retlist)))))))

(defun pop3-list (process &optional msgno)
  "Return the results of a LIST command.
If (optional) msgno is invalid, return nil.
Otherwise, return a list in the form

   (N (1 LEN-1) (2 LEN-2) ... (N LEN-N))

where

   N is an integer for the number of msg/len pairs (could be 0)
   LEN-n is an integer."
  (let ((bad-msgno nil))

  (if msgno
	  (pop3-send-command process (format "LIST %d" msgno))
	(pop3-send-command process "LIST"))

  (condition-case ()
	  (pop3-read-response process t)
	(error (setq bad-msgno t)))

  (unless bad-msgno
	(let ((retlist '())
		  (len nil)
		  (msgno nil))
	  (save-excursion
		(pop3-get-extended-response process)
		(set-buffer (process-buffer process))
		(goto-char pop3-extended-response-beginning)

		(while (looking-at "\\([^ \n\t]*\\) \\([^ \n\t]*\\)")
		  (setq msgno (string-to-int
					   (buffer-substring (match-beginning 1) (match-end 1))))
		  (setq len (string-to-int
					 (buffer-substring (match-beginning 2) (match-end 2))))
		  (push (cons msgno (list len)) retlist)
		  (beginning-of-line 2))
		(cons (length retlist) (nreverse retlist)))))))

;;; Utility code

(defun pop3-get-extended-response (process)
  "Get the extended pop3 response in the PROCESS buffer.
Called by `pop3-uidl', `pop3-list', `pop3-top'."
  ;;
  ;; code extracted from pop3.el's 'pop-retr' function
  ;;
  (let ((start pop3-read-point)
		(end nil))
	(save-excursion
	  (set-buffer (process-buffer process))
	  (while (not (re-search-forward "^\\.\r\n" nil t))
		(accept-process-output process 3)
		(if (> (buffer-size)  20000) (sleep-for 1))
		(if (> (buffer-size)  50000) (sleep-for 1))
		(if (> (buffer-size) 100000) (sleep-for 1))
		(if (> (buffer-size) 200000) (sleep-for 1))
		(if (> (buffer-size) 500000) (sleep-for 1))
		(goto-char start))
	  (setq pop3-extended-response-beginning start)
	  (setq pop3-read-point (point-marker))
	  (goto-char (match-beginning 0))
	  (setq end (point-marker)
			pop3-extended-response-end (point-marker))
	  (pop3-clean-region start end)
	  (goto-char start))))

(provide 'epop3)

;;; epop3.el ends here
