(package-initialize)

;;(require 'org-install)
(org-babel-load-file "~/.emacs.d/emacs.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules
   (quote
    (org-bbdb org-gnus org-id org-info org-protocol org-drill org-jsinfo org-habit org-irc org-mouse org-protocol org-annotate-file org-eval org-expiry org-interactive-query org-man org-collector org-panel org-screen org-toc)))
 '(package-selected-packages
   (quote
    (neotree projectile general which-key helm doom-themes evil-escape evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
