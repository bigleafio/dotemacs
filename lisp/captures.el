(setq org-capture-templates
      '(("t" "Todo"
         entry (file+headline (lambda () (concat org-directory "organizer.org")) "Task List")
         "* TODO %?
DEADLINE: %t
:LOGBOOK:
- State \"TODO\"       from \"\"           %U
:END:
see: %a\n")
        ("n" "Note"
         entry (file+headline (lambda () (concat org-directory "organizer.org")) "Notes")
         "* %?
%U\n%a\n")
        ("b" "Book" entry (file+headline (lambda () (concat org-directory "organizer.org")) "Books")
         "* %?
(C-c C-w to refile to fiction/non-fiction)
see %a
entered on %U\n")
        ("q" "Clock (quick)" plain (clock)
         "%a%?\n")
        ("s" "Emacs tool sharpening"
         entry (file+olp (lambda () (concat org-directory "programming_notes.org"))
                         "Emacs"
                         "Sharpening list")
         "* %?
see %a
entered on %U\n")
        ("S" "General tool sharpening"
         entry (file+olp (lambda () (concat org-directory "programming_notes.org"))
                         "General sharpening")
         "* %?
see %a
entered on %U\n")
        ("d" "Date"
         entry (file+datetree+prompt (lambda () (concat org-directory "dates.org")))
         "* %?
%t
see %a\n")
        ("j" "Journal"
         plain (file+datetree (lambda () (concat org-directory "journal.org")))
         "**** <title>\n%U\n\n%?\n")
        )
)
(provide 'captures)
