(setq display-time-format '(format-time-string "%Y-%m-%d %H:%M:%S" ))

;;;                              (if time-zone " (") time-zone (if time-zone ")")
;;;                              (if mail " Mail" "")))

(setq display-time-format "%H:%M %d/%m")

;; (setq display-time-string-forms   '((substring year -2) "/" month "/" day
;;                                     " " 24-hours ":" minutes ":" seconds
;;                                     (if time-zone " (") time-zone (if time-zone ")")
;;                                     (if mail " Mail" "")))

(setq display-time-format "[%Y-%m-%d %H:%M:%S (%Z)]")

(setq
  ;; First two not needed if set display-time-string-forms directly.
 display-time-day-and-date t
 display-time-24hr-format  t
 display-time-interval     10)
(display-time-mode t)
