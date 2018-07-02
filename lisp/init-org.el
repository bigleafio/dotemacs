(use-package org-mime)
(use-package org-id)
(use-package org-clock)
(use-package org-habit)
(use-package org-mouse)
(use-package org-protocol)


(setq org-id-link-to-org-use-id 'use-existing)

(setq org-startup-indented t)

(setq org-directory "~/Dropbox/Notes/")

(setq org-default-notes-file (concat org-directory "organizer.org"))

(setq org-imenu-depth 5)

(setq org-list-allow-alphabetical t)

;; org-mode hook
(defun jsg/org-mode-hook ()
  "My org-mode-hook function."
  (auto-fill-mode 1))

(add-hook 'org-mode-hook 'jsg/org-mode-hook)

(require 'contacts)
(require 'agenda)
;;(require 'shortcuts)
(require 'todo)
(require 'code)
(require 'captures)
(require 'mobile)
(require 'papers)

(provide 'init-org)
