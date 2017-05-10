 '(org-capture-templates
   (quote
    (("w" "Add Work Task" entry
      (file+headline "~/doc/tasks/todo.org" "Inbox")
      "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:CLIENT:
:TICKET:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("n" "Note" entry
      (file "~/doc/tasks/notes.org")
      "* NOTE %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("c" "Calendar" entry
      (file+headline "~/doc/tasks/todo.org" "Inbox")
      "* APPT %?
SCHEDULED: %t
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
      ("J" "Journal entry with date" plain
         (file+datetree+prompt "~/doc/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
     ("t" "Add Task" entry
      (file+headline "~/doc/tasks/todo.org" "Inbox")
      "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t))))


(setq org-agenda-text-search-extra-files '(agenda-archives))

(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))

(setq org-enforce-todo-dependencies t)

(setq org-log-done (quote time))

(setq org-log-redeadline (quote time))

(setq org-log-reschedule (quote time))
