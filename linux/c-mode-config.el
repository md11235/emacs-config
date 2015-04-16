;; 2008-10-15
(c-set-offset 'inline-open 0)
(c-add-style "zscstyle" '("awk" (c-basic-offset . 2)
                          (c-offsets-alist (inline-open . 0))))

(c-add-style "oepcstyle" '("awk" (c-basic-offset . 1)
                           (c-offsets-alist
                            (inline-open . 0)
                            (topmost-intro-cont . 0)
                            (topmost-intro . 0)
                            (statement-block-intro . 2)
                            (comment-intro . 0))))
(c-add-style "appleapcstyle" '("awk" (c-offsets-alist
                                  (inline-open . 0)
                                  (topmost-intro-cont . 4)
                                  (topmost-intro . 4)
                                  (comment-intro . 0))))
(setq c-default-style
      '((c-mode . "zscstyle") (c++-mode . "zscstyle") (java-mode . "java") (awk-mode . "awk") (other . "gnu")))
