;;Set up use-package

(defconst emacs-start-time (current-time))
;;(setq init-file-debug t)

(require 'package)
(setq inhibit-startup-screen nil;
      initial-scratch-message ";; ready\n\n"
      package-enable-at-startup nil
      package-user-dir "~/.emacs.d/elpa/"
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(defvar jsg/refreshed-package-list nil
  "This will be t if the package list has been refreshed.")

(defun jsg/ensure-refreshed ()
  "Ensure the package list has been refreshed this startup."
  (unless jsg/refreshed-package-list
    (package-refresh-contents)
    (setq jsg/refreshed-package-list t)))

(defun jsg/package-ensure-installed (package)
  "Install a missing PACKAGE if it isn't already."
  (unless (package-installed-p package)
    (package-install package)))

(advice-add 'package-install
            :before
            (lambda (&rest args)
              (jsg/ensure-refreshed)))

;; Keep custom in a separate file
(defconst base-path (file-name-directory load-file-name))
(setq custom-file (concat base-path "lisp/custom.el"))

(add-to-list 'load-path (expand-file-name "lisp" base-path))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package diminish)
(require 'diminish)                ;; if you use :diminish
(use-package bind-key)
(require 'bind-key)                ;; if you use any :bind variant

(use-package use-package-chords
  :defer t
  :config (key-chord-mode 1))

(setq use-package-always-ensure t)

;; End setup use-package

(setq custom-safe-themes t)

(use-package all-the-icons)

(use-package better-defaults
   :ensure t)

(use-package beacon
  :defer 2
  :config
  (beacon-mode 1))

(use-package bind-key
  :defer t)

;;Text completion framework for Emacs
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

(add-to-list 'load-path (expand-file-name "color-theme" base-path))


(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-sanityinc-tomorrow-night)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :commands color-theme-sanityinc-tomorrow-night
  )

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(use-package all-the-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)

  ;(require 'doom-neotree)
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  (doom-themes-org-config))

(use-package solaire-mode
  :config
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

  ;; ...if you use auto-revert-mode:
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)

  ;; highlight the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  ;; if the bright and dark background colors are the wrong way around, use this
  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; This should be used *after* you load the active theme!
  ;;
  ;; NOTE: This is necessary for themes in the doom-themes package!
  (solaire-mode-swap-bg))

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

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :custom (counsel-find-file-ignore-regexp
           (concat "\\(\\`\\.[^.]\\|"
                   (regexp-opt completion-ignored-extensions)
                   "\\'\\)"))
  :bind (("C-*"     . counsel-org-agenda-headlines)
         ("C-x C-f" . counsel-find-file)
         ("C-c e l" . counsel-find-library)
         ("C-c e q" . counsel-set-variable)
         ;("C-h e l" . counsel-find-library)
         ;("C-h e u" . counsel-unicode-char)
         ;("C-h f"   . counsel-describe-function)
         ("C-x r b" . counsel-bookmark)
         ("M-x"     . counsel-M-x)
         ;; ("M-y"     . counsel-yank-pop)

         ("M-s f" . counsel-file-jump)
         ("M-s g" . counsel-rg)
         ("M-s j" . counsel-dired-jump))
  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date)))

(use-package counsel-dash
  :bind ("C-c C-h" . counsel-dash))

(use-package counsel-osx-app
  :bind* ("S-M-SPC" . counsel-osx-app)
  :commands counsel-osx-app
  :config
  (setq counsel-osx-app-location
        (list "/Applications"
              "/Applications/Misc"
              "/Applications/Utilities"
              (expand-file-name "~/Applications")
              (expand-file-name "~/local/.bin")
              "/Applications/Xcode.app/Contents/Applications")))

(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)))
                       ;; (agenda . 5)))
  (setq frame-title-format "Welcome to the Bear")
  ;; Set the title
  (setq dashboard-banner-logo-title "Some days you eat the bear, some days the bear eats you.")
  :init
  (dashboard-setup-startup-hook)
  )

(use-package dired :ensure nil
  :bind (:map dired-mode-map
              ("/" . dired-narrow-regexp))
  :config
  (setq delete-by-moving-to-trash t)
  ;; mark symlinks
  (setq dired-ls-F-marks-symlinks t)
  ;; fix `ls' for macOS.
  (when (memq window-system '(mac ns))
    (setq insert-directory-program "gls" dired-use-ls-dired t))
  ;; Never prompt for recursive copies of a directory
  (setq dired-recursive-copies 'always)
  ;; Never prompt for recursive deletes of a directory
  (setq dired-recursive-deletes 'always)

  ;; makes dired guess the target directory
  (setq dired-dwim-target t)

  ;; Dired listing switches
  ;;  -a : Do not ignore entries starting with .
  ;;  -l : Use long listing format.
  ;;  -G : Do not print group names like 'users'
  ;;  -h : Human-readable sizes like 1K, 234M, ..
  ;;  -v : Do natural sort .. so the file names starting with . will show up first.
  ;;  -F : Classify filenames by appending '*' to executables,
  ;;       '/' to directories, etc.
  ;; default value for dired: "-al"
  (setq dired-listing-switches "-alGhvF --group-directories-first")

  ;; auto-revert dired
  (setq dired-auto-revert-buffer t)

  (defun rag/dired-rename-buffer-name ()
    "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
    (let ((name (buffer-name)))
      (if (not (string-match "/$" name))
    (rename-buffer (concat "*Dired* " name "/") t))))

  (add-hook 'dired-mode-hook #'rag/dired-rename-buffer-name)

  ;; filter dired lists by regexp, fuzzy matching or string
  ;; https://github.com/Fuco1/dired-hacks#dired-filter
  (use-package dired-narrow)

  ;; a hydra to sort files in dired easily
  ;; Press `S' to invoke dired-quick-sort hydra
  ;; https://gitlab.com/xuhdev/dired-quick-sort
  (use-package dired-quick-sort
    :config (dired-quick-sort-setup))

  ;; dired-ranger: copy paste like in GUI applications
  ;; https://github.com/Fuco1/dired-hacks#dired-ranger
  (use-package dired-ranger
    :ensure t
    :bind (:map dired-mode-map
                ("C" . dired-ranger-copy)
                ("R" . dired-ranger-move)
                ("Y" . dired-ranger-paste)))

  ;; dired-x - to hide uninteresting files in dired
  (use-package dired-x :ensure nil
    :config
    (progn
      (setq dired-omit-verbose nil)
      ;; hide backup, autosave, *.*~ files
      ;; omit mode can be toggled using `C-x M-o' in dired buffer.
      (add-hook 'dired-mode-hook #'dired-omit-mode)
      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.git$")))))


;; General
;(use-package general
;  :defer 2
;  :ensure which-key
;  :config
;  (load-file "~/.emacs.d/lisp/general.el")
;  )

(use-package helm
  :defer 2
  :diminish helm-mode
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)

         ;; Reverse tab and C-z
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z"   . helm-select-action)

         :map org-mode-map
         ("C-c h" . helm-org-in-buffer-headings))
  :config
  (helm-mode 1)

  ;; Resize based on the number of results
  (helm-autoresize-mode 1)

  ;; Turn on fuzzy matching for everything we can
  (setq helm-autoresize-max-height 15
        helm-autoresize-min-height 5
        helm-candidate-number-limit 1000
        helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t))

(use-package hydra
  :load-path "site-lisp/hydra")

(use-package ivy
  :load-path "site-lisp/swiper")

(use-package ivy-hydra
  :after (ivy hydra))

(use-package ace-window
  :bind* ("<C-return>" . ace-window))

(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package smex
   :ensure t)
(smex-initialize) 
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package ido-completing-read+ :ensure t)


(use-package paredit :ensure t)
   
(ido-ubiquitous-mode 1)

(use-package idle-highlight-mode
   :ensure t)
   
(use-package find-file-in-project
   :ensure t)
   
(use-package scpaste :ensure t)
   
(use-package elisp-slime-nav  :ensure t)

;; evil (vim) mode
(use-package evil
  :ensure t
  :config (setq evil-default-cursor t)
  (setq evil-insert-state-cursor '(bar "white")
      evil-emacs-state-cursor '(bar "white") 
      evil-normal-state-cursor '(hbar "#97C150"))
  (bind-keys :map evil-normal-state-map
         ("r" . evilmr-replace-in-defun))
  (evil-mode t))

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

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Easy Motion
(use-package evil-easymotion :ensure t)

(use-package evil-escape :ensure t)

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

;;(require 'init-org)

  ;; Key chords
(use-package key-chord
    :config
    (setq key-chord-two-keys-delay 0.2)
    )

(use-package key-seq :defer t)

;;(use-package powerline-evil)

(use-package projectile
  :defer t
  :after helm
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
  (projectile-global-mode 1))

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

(use-package general)

(use-package org
  :demand
  :general
  (general-nmap "SPC c" 'org-capture)
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
  (projectile-global-mode 1))

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

