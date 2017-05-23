;; path fix
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package osx-clipboard :ensure t
  :init
  (osx-clipboard-mode +1))

(use-package pbcopy :ensure t
  :if (not (display-graphic-p))
  :init
  (turn-on-pbcopy))

(use-package grab-mac-link :ensure t
  :commands grab-mac-link)