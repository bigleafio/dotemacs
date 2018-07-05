(setq user-full-name "Jason Graham"
      user-mail-address "jgraham20@gmail.com")

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------

;;(let ((normal-gc-cons-threshold (* 20 1024 1024))
  ;;    (init-gc-cons-threshold (* 128 1024 1024)))
  ;;(setq gc-cons-threshold init-gc-cons-threshold)
  ;;(add-hook 'after-init-hook
  ;;          (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   1)

(add-to-list 'default-frame-alist '(font . "PragmataPro"))
;;(add-to-list 'default-frame-alist '(height . 24))
;;(add-to-list 'default-frame-alist '(width . 80))

(use-package eyebrowse
 :ensure t
 :config 
  (eyebrowse-mode t))

(use-package shackle
 :ensure t 
 :init
   (setq shackle-rules '((compilation-mode :noselect t))
         shackle-default-rule '(:select t))
   (setq helm-display-function 'pop-to-buffer) ; make helm play nice
   (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)))
 :config (shackle-mode)
)

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode 1))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
	helm-mode-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-locate-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-completion-in-region-fuzzy-match t
	helm-candidate-number-list 150
	helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source t
	helm-echo-input-in-header-line t
	helm-autoresize-max-height 0
	helm-autoresize-min-height 20)
  :config
  (helm-mode 1))

(use-package dired
   :defer t
   :bind* (("C-x d" . dired-other-window)
           ("C-x C-d" . dired))
   :commands (dired)
   :config
   (setq dired-use-ls-dired nil)
   (use-package dired-x
     :bind* (("C-x C-'" . dired-jump))
     :commands (dired-omit-mode)
     :init
     (add-hook 'dired-load-hook (lambda () (load "dired-x")))
     (add-hook 'dired-mode-hook #'dired-omit-mode)
     :config
     (setq dired-omit-verbose nil)
     (setq dired-omit-files
           (concat dired-omit-files "\\|^\\..*$\\|^.DS_Store$\\|^.projectile$\\|^.git$"))))

;;; Ranger:
(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t
        ranger-show-dotfiles nil
        ranger-show-literal nil))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  "SPC" '(helm-M-x :which-key "M-x")
  ;;"pf"  '(helm-find-files :which-key "find files")
  ;; Buffers
  "b" '(:ignore t :which-key "Buffers")
  "bb"  '(helm-buffers-list :which-key "buffers list")
   ;; Buffers
  "f" '(:ignore t :which-key "File")
  "fr"  '(ranger :which-key "open ranger")
  "fd"  '(dired :which-key "open dired")
  "ff"  '(helm-find-files :which-key "find files")
  ;; Window
   "w" '(:ignore t :which-key "Windows")
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wx"  '(delete-window :which-key "delete window")
  ;; Others
  "a" '(:ignore t :which-key "Applications")
  "at"  '(ansi-term :which-key "open terminal")
  "ao"  '(org-mode :which-key "org-mode")
))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))
;; All The Icons
(use-package all-the-icons :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (neotree projectile general which-key helm doom-themes evil-escape evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; NeoTree

(defun my/tangle-dotfiles ()
  "If the current file is in '~/.emacs.d', the code blocks are tangled"
  (when (equal (file-name-directory (directory-file-name buffer-file-name))
               (concat (getenv "HOME") "/.emacs.d/"))
    (org-babel-tangle)
    (message "%s tangled" buffer-file-name)))

(add-hook 'after-save-hook #'my/tangle-dotfiles)

(use-package ox-hugo
  :after ox)

(defun jsg/org-captures() 
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
))

(setq org-enforce-todo-dependencies t)
(setq org-log-done 'time)
(setq org-log-note-clock-out nil)

(setq org-todo-keywords
      '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@!)")))
(setq org-log-into-drawer "LOGBOOK")

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; from https://lists.gnu.org/archive/html/emacs-orgmode/2012-02/msg00515.html
(defun org-summary-checkboxes ()
  "Switch entry to DONE when all sub-checkboxes are done, to TODO otherwise."
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
          (if (match-end 1)
              (if (equal (match-string 1) "100%")
                  (org-todo 'done)
                (org-todo 'todo))
            (if (and (> (match-end 2) (match-beginning 2))
                     (equal (match-string 2) (match-string 3)))
                (org-todo 'done)
              (org-todo 'todo)))))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-checkbox-statistics-hook 'org-summary-checkboxes)

(defun jsg/org-sort-todos ()
  "Sort entries by TODO status"
  (interactive)
  (org-sort-entries nil ?o)
  (outline-hide-leaves))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c 6") 'jsg/org-sort-todos)))

(use-package org
  :demand
  :mode ("\\.org\\'" . org-mode)
  :diminish org-indent-mode
  :init
  (require 'org-indent)
  :config
  (setq org-completion-use-ido t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-log-done t
        org-log-done-with-time t
        org-log-refile t
        org-support-shift-select t)

  (add-hook 'org-mode-hook 'auto-fill-mode))
 
(setq org-modules '(org-bbdb
                      org-gnus
                      org-drill
                      org-info
					  org-id
                      org-jsinfo
                      org-habit
                      org-irc
                      org-mouse
                      org-protocol
                      org-annotate-file
                      org-eval
                      org-expiry
                      org-interactive-query
                      org-man
                      org-collector
                      org-panel
                      org-screen
                      org-toc))
(eval-after-load 'org
 '(org-load-modules-maybe t))

 ;; Prepare stuff for org-export-backends
(setq org-export-backends '(org latex icalendar html ascii))

(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)
(bind-key "<f9> <f9>" 'org-agenda-list)
(bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))

(jsg/org-captures)
(menu-bar-mode 1)
(display-time-mode 1)
