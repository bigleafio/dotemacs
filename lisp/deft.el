(use-package deft)

(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/Dropbox/Notes")
(global-set-key [f8] 'deft)
(setq deft-recursive t)