;; start the emacs server if necessary
(require 'server)

(eval-when-gnu/linux
 (unless (string-equal "root" (getenv "USER"))
   ;; Only start server mode if I'm not root
   (unless (server-running-p)
     (server-start))))
