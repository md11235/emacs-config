;; if (string= "windows-nt" system-type)
(eval-when-windows-nt
 (setq msys2-root-dir "d:/zhang/msys32/")
 ;;;;;;;;;;;;;;;;;;;; under Windows: maxmize frame on startup
 (when (fboundp 'w32-send-sys-command)
   (defun w32-restore-frame ()
     "Restore a minimized frame"
     (interactive)
     (w32-send-sys-command 61728))
   (defun w32-maximize-frame ()
     "Maximize the current frame"
     (interactive)
     (w32-send-sys-command 61488))))


