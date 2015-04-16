;;ibuffer
(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-g") 'find-file-at-point)
(autoload 'ibuffer "ibuffer" "List buffers." t)
;;(add-to-list 'ibuffer-never-show-regexps "^\\*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; folding-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'folding)
;; (autoload 'folding-mode          "folding" "Folding mode" t)
;; (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
;; (autoload 'turn-on-folding-mode  "folding" "Folding mode" t)
;; (folding-add-to-marks-list 'php-mode "//{{{"  "//}}}"  nil t)
;; (folding-add-to-marks-list 'prolog-mode "%{{{" "%}}}" nil t)
;; (folding-add-to-marks-list 'html-mode "<!-- {{{ " "<!-- }}} -->" " -->" nil t)

