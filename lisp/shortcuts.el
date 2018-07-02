(defun jsg/org-view ()
	(org-agenda-list nil nil 1)
	(split-window-horizontally)
	(find-file (concat org-directory "organizer.org"))	
)

(spacemacs/set-leader-keys "ot" (lambda () (interactive) (org-agenda-list) (split-window-horizontally) (find-file (concat org-directory "organizer.org"))	))

(provide 'shortcuts)
