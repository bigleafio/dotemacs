(use-package org
  :defer t
  :commands (org-mode
             org-agenda-list
             org-capture
             org-store-link
             org-agenda)
  :mode (("\\.org\\'" . org-mode)
         ("*Org Agenda*" . org-agenda-mode))

  :bind*
  (:map dired-mode-map
   ("C-c C-l" . org-store-link))

  :config

  ;; ---------- Extension ---------------------------------------------------
  (use-package org-timeline :ensure t
    :disabled t)

 (use-package ob-shell :ensure t
    :disabled t)

 (use-package org-trello
   :ensure t)
 
  (use-package org-bullets :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

    :config
    (setq org-bullets-bullet-list  '("➡" "➠" "➟" "➝" "↪")))

  (use-package org-indent :ensure nil :diminish "")

  (use-package orgit :ensure t :disabled t)

  (use-package ox-gfm :ensure t :disabled t)

  ;; ---------- BABEL -------------------------------------------------------
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((R . t)
                                 (perl . t)
                                 (python . t)
                                 (clojure . t)
                                 (sh . t)
                                 (emacs-lisp . t)
                                 (dot . t)
                                 (makefile . t)))

  (defun org-babel-tangle-all-block-same-file ()
    "tangle all blocks which belong to the same file."
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively #'org-babel-tangle)))

  (general-define-key
   :keymaps 'org-mode-map
    "s-e" 'org-babel-tangle-all-block-same-file
    )

  ;; ---------- default -----------------------------------------------------
  (require 'org-agenda)
  ;;(require 'org-mu4e)
  (define-key global-map "\C-cc" 'org-capture)
  ;; inspired from  http://pages.sachachua.com/.emacs.d/Sacha.html#orgce6f46d
  (setq org-agenda-files
        (list "~/Notes/inbox.org"
              "~/Notes/todo.org"
              "~/Notes/work.org"
              "~/Notes/goals.org"
              "~/Notes/church.org")
        )

  (setq org-mu4e-link-query-in-headers-mode nil)
  (setq org-capture-use-agenda-date t) 
  (setq org-agenda-span 7)
  (setq org-agenda-tags-column -100)    
  (setq org-agenda-sticky nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-use-tag-inheritance t)
  (setq org-agenda-show-log t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-deadline-warning-days 4)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  
  (setq org-agenda-time-grid
        '((daily today require-timed)
          "----------------"
          (800 1000 1200 1400 1600 1800)))
  ;; agenda start on mondays
  (setq org-agenda-start-on-weekday 1)

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))))

;;;*** gtd with org
  (setq
   org-modules '(org-crypt)
   org-tags-column 80                  
   org-hide-block-startup t 
   org-refile-targets '(("~/Notes/todo.org" :level . 2)
                        ("~/Notes/work.org" :level . 1)
                        ("~/Notes/goals.org" :level . 1)
                        ("~/Notes/church.org" :level . 2))

   org-default-notes-file "~/Notes/notes.org"
   org-capture-templates
   '(("w" "Add Work Task" entry
      (file+headline "~/Notes/todo.org" "Inbox")
      "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:CLIENT:
:TICKET:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("l" "Link" entry (file+headline "~/Notes/todo.org" "Link") "* TODO %? %T\n%a")
     ("e" "Todo with Email Link" entry (file+headline "~/Notes/todo.org" "Todo") 
      "* TODO [#A] %?
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))
ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:LINK: %a
:END:" :prepend t)
     ("t" "Todo" entry (file+headline "~/Notes/todo.org" "Todo") "* TODO %? %T")
     ("n" "notes" entry (file+datetree "~/Notes/journal.org") "* %(hour-minute-timestamp) %?\n")))

  (defun hour-minute-timestamp ()
    (format-time-string "%H:%M" (current-time)))

  (setq org-agenda-include-diary nil)
;;;*** src block and babel
  (setq
   org-src-preserve-indentation t
;;;*** footnotes
   org-footnote-auto-adjust t
   org-footnote-define-inline nil
   org-footnote-fill-after-inline-note-extraction t
   org-footnote-section nil
;;;*** export
   org-export-with-todo-keywords nil
   org-export-default-language "en"
   org-export-backends '(ascii html icalendar latex md koma-letter)

   org-latex-remove-logfiles t
   org-src-fontify-natively t
   org-latex-table-caption-above nil
   org-latex-tables-booktabs t
   org-startup-with-inline-images nil
   org-startup-indented t)


  (defun org-insert-heading-with-date-after-current ()
    "Insert a new heading with current date same level as current,
     after current subtree."
    (interactive)
    (org-back-to-heading)
    (org-insert-heading)
    (insert-timestamp)
    (org-move-subtree-down)
    (end-of-line 1))

  (defun org-agenda-cts ()
    (let ((args (get-text-property
                 (min (1- (point-max)) (point))
                 'org-last-args)))
      (nth 2 args)))

  (defhydra hydra-org-agenda-view (:hint nil :columns 1)
    "VIEW"
    ("SPC" org-agenda-reset-view)
    ("d" org-agenda-day-view
     (format "%s - day" (if (eq 'day (org-agenda-cts)) "[x]" "[ ]")))
    ("w" org-agenda-week-view
     (format "%s - week" (if (eq 'week (org-agenda-cts)) "[x]" "[ ]")))
    ("t" org-agenda-fortnight-view
     (format "%s - fortnight" (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]")))
    ("m" org-agenda-month-view
     (format "%s - month" (if (eq 'month (org-agenda-cts)) "[x]" "[ ]")))
    ("y" org-agenda-year-view
     (format "%s - year" (if (eq 'year (org-agenda-cts)) "[x]" "[ ]")))
    ("q" (message "Abort") :exit t))

  (bind-key (kbd "v") #'hydra-org-agenda-view/body org-agenda-mode-map)


;;;* Keybindings
  (general-define-key
   :keymaps 'org-mode-map
    (general-chord ",c") 'org-shiftcontrolleft
    (general-chord ",t") 'org-shiftcontroldown
    (general-chord ",s") 'org-shiftcontrolup
    (general-chord ",r") 'org-shiftcontrolright
    (general-chord ";C") 'org-metaleft
    (general-chord ";T") 'org-metadown
    (general-chord ";S") 'org-metaup
    (general-chord ";R") 'org-metaright))


;;;* Keybindings

