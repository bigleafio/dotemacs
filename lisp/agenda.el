
(setq org-agenda-files
      (mapcar
       (function (lambda (f) (concat org-directory f)))
       (list "organizer.org"            ; main work/life todos
             "church.org"               ; church
             "notes.org"    ; programming notebook
             )))

(setq org-agenda-dim-blocked-tasks nil)

(setq org-agenda-sticky t)

(setq org-habit-show-all-today t)

(provide 'agenda)
