  ;;(require 'org-checklist)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; settings
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|org\\.txt\\)$" . org-mode))
  (setq org-startup-indented t)
  (setq org-cycle-separator-lines 1)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
  (setq org-agenda-file-regexp "\\`[^.].*\\.\\(org\\.txt\\|org\\)\\'")
  (setq org-clock-idle-time 15)
  (setq org-ellipsis " ▼") ;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html

  ;; todos
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(eval-when-compile
  ;; organizer directory
  (if (memq window-system '(w32))
      (setq jsn/home-dir "C:/Users/JG186074")
    (setq jsn/home-dir (expand-file-name "~")))
  (setq org-directory (concat jsn/home-dir "/Dropbox/Documents/Organizer/"))
  (setq org-default-notes-file (concat org-directory "inbox.org.txt"))
  (setq jsn/org-default-habits-file (concat org-directory "habits.org.txt")))

  ;; agenda
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)

  ;; tags
  ;; Tags with fast selection keys
  (setq org-tag-alist (quote ((:startgroup)
                              ("@errand" . ?e)
                              ("@office" . ?o)
                              ("@home" . ?H)
                              (:endgroup)
                              ("WAITING" . ?w)
                              ("HOLD" . ?h)
                              ("IDEA" . ?i)
                              ("PERSONAL" . ?P)
                              ("DRAFT" . ?D)
                              ("WORK" . ?W)
                              ("NOTE" . ?n)
                              ("CANCELLED" . ?c)
                              ("FLAGGED" . ??))))

  ;; capture
  (setq org-capture-templates
        (quote (("t" "todo" entry (file org-default-notes-file)
                 "* TODO %?\n%U\n%a\n")
                ("m" "meeting" entry (file org-default-notes-file)
                 "* MEETING with %? :MEETING:\n%U")
                ("i" "idea" entry (file org-default-notes-file)
                 "* %? :IDEA:\n%U\n%a\n")
                ("n" "note" entry (file org-default-notes-file)
                 "* %? :NOTE:\n%U\n%a\n")
                ("h" "habit" entry (file jsn/org-default-notes-file)
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

  ;; refiling
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  ;; pomodoro
  (add-hook 'org-pomodoro-finished-hook (lambda()
                                          (org-journal-new-entry nil)
                                          ))

