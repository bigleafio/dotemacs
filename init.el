;;;;
;; Packages
;;; code:

;; -------------------------------------------------------
;; User Info
;; -------------------------------------------------------
(defconst emacs-start-time (current-time))
(load "~/.emacs.d/config/core-debug")
(setq spacemacs-debug-with-adv-timers t)
(spacemacs/init-debug)
(message "***** Loading (Initialize): %s" (current-time-string))
(setq user-full-name "Jason Graham")
(setq user-mail-address "jgraham20@gmail.com")

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore GC after startup
                               (setq gc-cons-threshold 8000000)))

;; -------------------------------------------------------
;; Define package repositories
;; -------------------------------------------------------
;; The comment below is needed DO NOT REMOVE
;; (package-initialize)
(setq inhibit-startup-screen nil
      initial-scratch-message ";; ready\n\n"
      package-enable-at-startup nil
      package-user-dir "~/.emacs.d/elpa/"
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

;; Keep custom in a separate file
(defconst base-path (file-name-directory load-file-name))
(setq custom-file (concat base-path "config/custom.el"))

(message "****** Loading Packages: %s" (current-time-string))

;; -------------------------------------------------------
;; A
;; -------------------------------------------------------
;;(use-package avy
;;  :defer 2)

;; autocomplete editor
(use-package auto-complete
  :defer t
  :diminish auto-complete-mode
  :config (ac-config-default))

; (use-package ag
;   :defer 8
;   :commands (ag)
;   :config
;   (progn
;     (setq ag-highlight-search t)
;     (setq ag-reuse-buffers t)
;     (add-to-list 'ag-arguments "--word-regexp")))

(use-package all-the-icons
  :defer t)

(use-package anzu
  :defer 2
  :diminish ""
  :commands (global-anzu-mode)
  :init
  (global-anzu-mode 1)
  :bind*
  (("C-/" . anzu-query-replace-at-cursor)
   ("C-9" . anzu-replace-at-cursor-thing))
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

;; -------------------------------------------------------
;; B
;; -------------------------------------------------------

(use-package beacon
  :defer 2
  :config
  (beacon-mode 1))

(use-package bind-key
  :defer t)

(use-package better-defaults
  :defer t)

;;This file is for lazy people wanting to swap buffers without typing C-x b on each window.
(use-package buffer-move :defer t)

;; -------------------------------------------------------
;; C
;; -------------------------------------------------------

;; Chords
;;Define chords using the :chords keyword in the same manner as :bind and related keywords,
;;using a cons or a list of conses
(use-package use-package-chords
  :defer t
  :config (key-chord-mode 1))

;;Text completion framework for Emacs
(use-package company
  :diminish ""
  :defer 2
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (setq
   company-idle-delay 0.2
   company-selection-wrap-around t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-show-numbers t)
  :config
  ;;(global-company-mode)

  (bind-keys :map company-active-map
    ("C-d" . company-show-doc-buffer)
    ("C-l" . company-show-location)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-t" . company-select-next)
    ("C-s" . company-select-previous)
    ("TAB" . company-complete))

  (setq company-backends
        '((company-css
           company-clang
           company-capf
           company-semantic
           company-xcode
           company-cmake
           company-files
           company-gtags
           company-etags
           company-keywords))))

;;Completion functions using Ivy
; (use-package counsel
;   :defer t
;   :bind*
;   (("M-x"     . counsel-M-x)
;    ("C-x C-f" . counsel-find-file)
;    ("C-c f"   . counsel-git)
;    ("C-c s"   . counsel-git-grep)
;    ("C-c /"   . counsel-ag)
;    ("C-c o"   . counsel-find-file-extern)
;    ("C-S-s"   . counsel-ag)
;    ("C-c l"   . counsel-locate))
;   :config
;   (setq counsel-find-file-at-point t)
;   (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
;   (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

; (use-package counsel-projectile
;   :defer t
;   :bind* (("H-P" . counsel-projectile-switch-to-buffer)
;           ("H-p" . counsel-projectile))
;   :config
;   (counsel-projectile-on))

;;(use-package counsel-gtags :defer t)

(use-package clojure-mode
  :defer t
  :diminish (clojure-mode . "λ")
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;;(use-package clojure-cheatsheet :defer 10)

(use-package cider
  :defer t
  :diminish "[Ƈ]"
  :config
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package color-theme
  :config
  (setq my-color-themes (list 'gruvbox 'leuven))
  (use-package color-theme-sanityinc-solarized :defer t)
  (use-package color-theme-sanityinc-tomorrow :defer t))


;; -------------------------------------------------------
;; D
;; -------------------------------------------------------

(use-package page-break-lines :defer t
  :diminish "")

(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)))
                       ;; (agenda . 5)))
  (setq frame-title-format "Welcome to BearMode")
  ;; Set the title
  (setq dashboard-banner-logo-title "Some days you eat the bear, some days the bear eats you.")
  ;; Set the banner
  (setq dashboard-startup-banner "~/.emacs.d/themes/emacs.png")
  :init
  (dashboard-setup-startup-hook))

;; deft
 (use-package deft
  :defer t
   :config (setq deft-extensions '("txt" "tex" "org"))
           (setq deft-directory "~/Dropbox/Documents/Organizer")
   :bind ("<f7>" . deft))

; (use-package dired
;   :defer t
;   :bind* (("C-x d" . dired-other-window)
;           ("C-x C-d" . dired))
;   :commands (dired)
;   :config
;   (setq dired-use-ls-dired nil)
;   (use-package dired-x
;     :bind* (("C-x C-'" . dired-jump))
;     :commands (dired-omit-mode)
;     :init
;     (add-hook 'dired-load-hook (lambda () (load "dired-x")))
;     (add-hook 'dired-mode-hook #'dired-omit-mode)
;     :config
;     (setq dired-omit-verbose nil)
;     (setq dired-omit-files
;           (concat dired-omit-files "\\|^\\..*$\\|^.DS_Store$\\|^.projectile$\\|^.git$")))

;   (use-package dired-details+
;     :ensure t
;     :config
;     (dired-details-install)
;     (setq-default dired-details-hidden-string " --- "
;                   dired-details-hide-link-targets nil))

;   (bind-keys :map dired-mode-map
;     ("SPC" . dired-view-other-window)
;     ("."   . hydra-dired-main/body)
;     ("t"   . dired-next-line)
;     ("s"   . dired-previous-line)
;     ("r"   . dired-find-file)
;     ("c"   . dired-up-directory)
;     ("'"   . eshell-here)
;     ("8"   . dired-mkdir-date)
;     ("9"   . dired-mkdir-date-rstyle)
;     ("C-'" . shell)
;     ("q"   . (lambda () (interactive) (quit-window 4)))))

;; -------------------------------------------------------
;; E
;; -------------------------------------------------------

;; evil (vim) mode
(use-package evil
  :defer 1
  :config (setq evil-default-cursor t)
  (setq evil-insert-state-cursor '(bar "white")
      evil-emacs-state-cursor '(bar "white") 
      evil-normal-state-cursor '(hbar "#97C150"))
  (bind-keys :map evil-normal-state-map
         ("r" . evilmr-replace-in-defun))
  (evil-mode t))

;; Easy Motion
(use-package evil-easymotion
  :defer 2
  :init (evilem-default-keybindings "SPC"))

(use-package evil-escape
  :defer t
  :diminish ""
  :init
   (evil-escape-mode)
  )

(use-package eshell
  :defer t
  :defines eshell-here
  :commands (eshell
             eshell-here)
  :config
  (use-package eshell-z :defer t)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-list-files-after-cd t
        eshell-ls-initial-args "-alh")

)

(use-package evil-matchit
  :defer 2
  :init (global-evil-matchit-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; -------------------------------------------------------
;; F
;; -------------------------------------------------------

; flycheck
(use-package flycheck
  :defer t
  :commands flycheck-mode
  :diminish (flycheck-mode . "ⓕ")
  :config
  (setq flycheck-highlighting-mode 'symbols))

;; -------------------------------------------------------
;; G
;; -------------------------------------------------------

; (use-package ggtags
;   :defer t
;   :commands (ggtags-mode)
;   :config
;   (general-define-key :keymaps 'ggtags-mode-map
;     "M-s-," 'ggtags-navigation-mode-abort
;     "M-s-." 'ggtags-find-tag-dwim))

;; General
(use-package general
  :ensure which-key
  :config
  (load-file "~/.emacs.d/config/general.el")
  )

; (use-package git-timemachine
;   :defer t)

; (use-package git-gutter
;   :defer t
;   :commands (global-git-gutter-mode)
;   :init
;   (global-git-gutter-mode +1)
;   (setq git-gutter:modified-sign "|")
;   (setq git-gutter:added-sign "|")
;   (setq git-gutter:deleted-sign "|")
;   :config
;   (setq git-gutter:update-interval 20)
;   (git-gutter:linum-setup))

; ;; git gutter
; (use-package git-gutter-fringe
;   :defer t
;   :if window-system
;   :diminish git-gutter-mode
;   :config (global-git-gutter-mode))

; (use-package goto-chg
;   :defer t
;   :commands (goto-last-change
;              goto-last-change-reverse))

;; -------------------------------------------------------
;; H
;; -------------------------------------------------------
(setq tramp-ssh-controlmaster-options nil)
;; Helm
(use-package helm-config
  :defer t
  :ensure helm
  :config
  (setq
   ;; open helm buffer inside current window, not occupy whole other window
   helm-split-window-in-side-p t
   ;; input close to where I type
   helm-echo-input-in-header-line t)

  (defun helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20))

; (use-package helm-make
;   :defer t
;   :bind* (("C-c C" . helm-make)
;           ("C-c p c" . helm-make-projectile))
;   :config
;   (setq helm-make-completion-method 'ivy))

(use-package helm-projectile
  :defer t
  :config
  (setq projectile-completion-system 'helm)
  :init
  (helm-projectile-on))

(use-package helm-gitignore
  :defer t
  :commands helm-gitignore)

(use-package hideshow
  :defer t
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

; (use-package hydra
;   :defer t
;   :config
;   ;;(load-file "~/.emacs.d/config/hydra.el")
;   (setq hydra-is-helpful t))

;; -------------------------------------------------------
;; I
;; -------------------------------------------------------

(use-package ibuffer :defer t
  :commands ibuffer
  :init
  (add-hook 'ibuffer-hook (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))
  :config
   (setq-default ibuffer-saved-filter-groups
                `(("Default"
                   ("Org" (mode . org-mode))
                   ("Markdown" (mode . markdown-mode))
                   ("Bash" (or (mode . shell-script-mode)))
                   ("Dired" (mode . dired-mode))
                   ("PDF" (mode . pdf-view-mode))
                   ("Mail" (or (mode . message-mode)
                               (mode . bbdb-mode)
                               (mode . mail-mode)
                               (mode . mu4e-compose-mode)))
                   ("Elisp" (mode . emacs-lisp-mode))
                   ("shell" (or (mode . eshell-mode)
                                (mode . shell-mode)))
                   ("Music" (or (mode . bongo-mode)
                                (mode . bongo-library-mode)
                                (mode . bongo-playlist-mode)))
                   ("Temp" (name . "\*.*\*")))))
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-display-summary t))

; (use-package ibuffer-vc
;   :requires ibuffer
;   :defer t
;   :config
;   (add-hook 'ibuffer-hook
;       (lambda ()
;         (ibuffer-vc-set-filter-groups-by-vc-root)
;         (unless (eq ibuffer-sorting-mode 'alphabetic)
;     (ibuffer-do-sort-by-alphabetic)))))


; (use-package info
;   :mode (("\\.info\\'" . Info-mode))
;   :config
;   (define-key Info-mode-map (kbd ".") #'hydra-info/body)

;   (defhydra hydra-info (:color pink
;                         :hint nil)
;     "
; Info-mode:
;   ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
;   ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
;   ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
;   ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
;   regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos
;   _1_ .. _9_ Pick first .. ninth item in the node's menu.
; "
;     ("]"   Info-forward-node)
;     ("["   Info-backward-node)
;     ("n"   Info-next)
;     ("p"   Info-prev)
;     ("s"   Info-search)
;     ("S"   Info-search-case-sensitively)

;     ("l"   Info-history-back)
;     ("r"   Info-history-forward)
;     ("H"   Info-history)
;     ("t"   Info-top-node)
;     ("<"   Info-top-node)
;     (">"   Info-final-node)

;     ("u"   Info-up)
;     ("^"   Info-up)
;     ("m"   Info-menu)
;     ("g"   Info-goto-node)
;     ("b"   beginning-of-buffer)
;     ("e"   end-of-buffer)

;     ("f"   Info-follow-reference)
;     ("i"   Info-index)
;     (","   Info-index-next)
;     ("I"   Info-virtual-index)

;     ("T"   Info-toc)
;     ("d"   Info-directory)
;     ("c"   Info-copy-current-node-name)
;     ("C"   clone-buffer)
;     ("a"   info-apropos)

;     ("1"   Info-nth-menu-item)
;     ("2"   Info-nth-menu-item)
;     ("3"   Info-nth-menu-item)
;     ("4"   Info-nth-menu-item)
;     ("5"   Info-nth-menu-item)
;     ("6"   Info-nth-menu-item)
;     ("7"   Info-nth-menu-item)
;     ("8"   Info-nth-menu-item)
;     ("9"   Info-nth-menu-item)

;     ("?"   Info-summary "Info summary")
;     ("h"   Info-help "Info help")
;     ("q"   Info-exit "Info exit")
;     ("C-g" nil "cancel" :color blue))
;   )

(with-eval-after-load "info"
  (info-initialize)
  (dolist (dir (directory-files package-user-dir))
    (let ((fdir (concat (file-name-as-directory package-user-dir) dir)))
      (unless (or (member dir '("." ".." "archives" "gnupg"))
                  (not (file-directory-p fdir))
                  (not (file-exists-p (concat (file-name-as-directory fdir) "dir"))))
        (add-to-list 'Info-directory-list fdir)))))

; (use-package ivy
;   :defer 2
;   :diminish (ivy-mode . "")
;   :commands (ivy-switch-buffer
;              ivy-switch-buffer-other-window)
;   :config
;   ;;(ivy-mode 1)

;   (setq ivy-use-virtual-buffers t)
;   (setq ivy-height 10)
;   (setq ivy-count-format "(%d/%d) ")
;   (setq ivy-initial-inputs-alist nil)
;   ;; from https://github.com/company-mode/company-statistics
;   ;; ignore buffers in the ignore buffer list.
;   (setq ivy-use-ignore-default 'always)
;   (setq ivy-ignore-buffers '("company-statistics-cache.el" "company-statistics-autoload.el"))
;   ;; if ivy-flip is t, presents results on top of query.
;   (setq ivy-flip t)
;   (setq ivy-overlay-at nil)
;   (setq ivy-re-builders-alist '((t      . ivy--regex-ignore-order)))

;   (defun ivy--matcher-desc ()
;     (if (eq ivy--regex-function
;             'ivy--regex-fuzzy)
;         "fuzzy"
;       "ivy")))

;; -------------------------------------------------------
;; K
;; -------------------------------------------------------

  ;; Key chords
  (use-package key-chord
    :config
    (setq key-chord-two-keys-delay 0.2)
    )

  (use-package key-seq :defer t)

;; -------------------------------------------------------
;; M
;; -------------------------------------------------------

;; magit
; (use-package magit
;   :defer t
;   :commands (magit-blame
;              magit-commit
;              magit-commit-popup
;              magit-diff-popup
;              magit-diff-unstaged
;              magit-fetch-popup
;              magit-init
;              magit-log-popup
;              magit-pull-popup
;              magit-push-popup
;              magit-revert
;              magit-stage-file
;              magit-status
;              magit-unstage-file
;              magit-blame-mode)
;   :bind (("s-v" . magit-status))

;   :config
;   (use-package git-modes :defer t)

;   (global-git-commit-mode)

;   (general-define-key
;    :keymaps 'magit-mode-map
;     "'" #'eshell-here)

;   (use-package magit-popup :defer t)
;   (use-package git-commit :defer t)

;   (setq magit-completing-read-function 'ivy-completing-read))

;; markdown mode

(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\'"   . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode 0)))
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-a" "insert"
    "C-c C-c" "export"
    "C-c TAB" "images"
    "C-c C-s" "text"
    "C-c C-t" "header"
    "C-c C-x" "move"))

;; -------------------------------------------------------
;; N
;; -------------------------------------------------------

;; neo-tree
(use-package neotree
  :defer t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
  :bind ("<f1>" . neotree-project-dir))

;; -------------------------------------------------------
;; O
;; -------------------------------------------------------

(use-package org
  :defer 5
  :config
  (load-file "~/.emacs.d/config/org.el"))

;; -------------------------------------------------------
;; P
;; -------------------------------------------------------

; (use-package paradox :defer 2
;   :commands (paradox-list-packages
;              package-list-packages))

;;(global-prettify-symbols-mode +1)

(use-package persp-mode
  :defer 5
  :diminish (persp-mode . "")
  :commands (persp-mode
             persp-next
             persp-prev
             pers-switch)
  :config
  (setq wg-morph-on nil)                ; switch off animation ?
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (setq persp-nil-name "nil")
  (global-set-key (kbd "H-p") 'persp-prev)
  (global-set-key (kbd "H-n") 'persp-next))

;; popwin
(use-package popwin
  :defer t)
  ;;:config
  ;;(popwin-mode 1))

(use-package powerline-evil)

;;TODO Fix the load paths
(use-package projectile
  :defer t
  :defines hydra-projectile/body
  :diminish (projectile-mode . " ⓟ")
  :bind* (("C-c p p" . projectile-switch-project))
  :commands (projectile-ag
             projectile-switch-to-buffer
             projectile-invalidate-cache
             projectile-find-dir
             projectile-find-file
             projectile-find-file-dwim
             projectile-find-file-in-directory
             projectile-ibuffer
             projectile-kill-buffers
             projectile-multi-occur
             projectile-switch-project
             projectile-recentf
             projectile-remove-known-project
             projectile-cleanup-known-projects
             projectile-cache-current-file
             projectile-project-root
             projectile-mode
             projectile-project-p)
  :bind (("s-p" . hydra-projectile/body)
         ("s-b" . projectile-switch-to-buffer))
  :config
  (projectile-global-mode 1)
  (helm-projectile-on))

  ; (use-package org-projectile
  ;   :defer t
  ;   :config
  ;   (org-projectile:per-repo)
  ;   (setq org-projectile:per-repo-filename "project_todo.org")
  ;   (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
  ;   (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p"))
  ;   (setq projectile-switch-project-action 'projectile-dired)
  ;   (setq projectile-completion-system 'ivy)
  ;   (add-to-list 'projectile-globally-ignored-files ".DS_Store"))


(use-package python
  :defer t
  :ensure python-mode
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.pyx\\'" . python-mode)
   ("\\.wsgi$" . python-mode))
  :interpreter
  (("ipython" . python-mode)
   ("python"  . python-mode))
  :init
  (add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))
  :config
  (load-file "~/.emacs.d/config/python-config.el"))


;; -------------------------------------------------------
;; R
;; -------------------------------------------------------
;; Rainbow Mode

(use-package rainbow-mode
  :defer 6
  :diminish rainbow-mode
  :init (rainbow-mode))

(use-package rainbow-delimiters
  :defer 6
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package recentf
  :defer 4
  :commands (recentf-mode
             counsel-recentf)
  :config
  (setq recentf-max-saved-items 50))

(use-package restart-emacs
  :defer 10
  :commands restart-emacs)

;; -------------------------------------------------------
;; S
;; -------------------------------------------------------

(use-package smart-mode-line
:config
(setq sml/no-confirm-load-theme t)
(setq sml/theme nil)
:init
(add-hook 'after-init-hook #'sml/setup))

(use-package smartparens
  :defer t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1))
  :init
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp))

;; SMEX
(use-package smex
  :defer t
  :config (smex-initialize)
  :bind ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c M-x" . execute-extended-command))

 ; (use-package swiper :defer t
 ;   :bind* (("M-s" . swiper)
 ;           ("M-S" . swiper-all)
 ;           :map swiper-map
 ;           ("C-s" . ivy-previous-history-element)
 ;           ("C-t" . ivy-yank-word)))

;; -------------------------------------------------------
;; T
;; -------------------------------------------------------

(use-package tile :defer t
  :defines (hydra-tile/body)
  :bind* (("C-c t" . hydra-tile/body)
          ("s-t" . tile))
  :config
  (setq tile-cycler
        (tile-strategies :strategies
          (list tile-tall-3
                tile-master-left-3
                tile-master-top-3
                tile-one))))

(use-package tiny :defer t
  :bind* (("C-;" . tiny-expand)))

(use-package transpose-frame :defer t)

;; -------------------------------------------------------
;; U
;; -------------------------------------------------------

;; better redo/undo
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  :bind ("s-z" . undo)
  ("s-Z" . redo))

;; -------------------------------------------------------
;; W
;; -------------------------------------------------------
;; Which-Key - displays available keybindings in popup
;; https://github.com/justbur/emacs-which-key
(use-package which-key :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; simple then alphabetic order.
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-height 0.5
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.5
        which-key-min-display-lines 7)
  ;; key description for C-x
  (which-key-add-key-based-replacements
  "C-x RET" "coding system -input"
  "C-x 4"   "Other Window"
  "C-x 5"   "Frame"
  "C-x 6"   "2C"
  "C-x @"   "event"
  "C-x 8"   "special char"
  "C-x a"   "abbrev"
  "C-x n"   "narrow"
  "C-x r"   "rectangle"
  "C-x v"   "version control"
  "C-c &"   "yas"
  "C-c @"   "hide-show"))

(use-package window-numbering :defer t
  :commands
  (window-numbering-mode
   select-window-0
   select-window-1
   select-window-2
   select-window-3
   select-window-4
   select-window-5
   select-window-6
   select-window-7
   select-window-8
   select-window-9)
  :config
  (defun window-numbering-install-mode-line (&optional position)
    "Do nothing, the display is handled by the powerline.")
  (window-numbering-install-mode-line))

(use-package winner
  :defer t)

(use-package whitespace
  :defer t)
;; -------------------------------------------------------
;; Y
;; -------------------------------------------------------

;; snippets
(use-package yasnippet :defer t
  :diminish yas-minor-mode
  :init (yas-global-mode 1))

(use-package yaml-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

;; ------------------------------------------------------
;; Misc settings
;; ------------------------------------------------------

(show-paren-mode 1) ; Always show matching parenthesis
(tool-bar-mode -1)
(setq-default cursor-type 'bar)
(delete-selection-mode 1) ; Deleting selected text if typed in/pasted
(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes or no

(whitespace-mode +1)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; OSX specific settings
;;(when (eq system-type 'darwin)
;;  (load "~/.emacs.d/config/osx"))

;; date and time in status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)


;;(setq version-control t )   ; use version control
;;(setq vc-make-backup-files t )    ; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq delete-old-versions t)
(setq vc-follow-symlinks t )               ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )  ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )  ; silent bell when you make a mistake

;; nice scrolling
(setq redisplay-dont-pause t
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(size-indication-mode t)

;; Disable menu bars, etc.
(if window-system (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode 1)

;; Set default fill column
(setq-default fill-column 80)

;;; Customize the modeline
(setq line-number-mode 1)
(setq column-number-mode 1)

;; ------------------------------------------------------
;; Theme
;; ------------------------------------------------------
(setq custom-safe-themes t)
(set-frame-font "Ubuntu Mono-14")

(require 'color-theme-sanityinc-solarized)
(color-theme-sanityinc-tomorrow-night)

(message "***** Loading Additional Config Files: %s" (current-time-string))

;; go full screen
(toggle-frame-maximized)
