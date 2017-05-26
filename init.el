;;;;
;; Packages
;;;;

;;; code:

;; -------------------------------------------------------
;; User Info
;; -------------------------------------------------------
(message "***** Loading (Initialize): %s" (current-time-string))
(setq user-full-name "Jason Graham")
(setq user-mail-address "jgraham20@gmail.com")
(load "~/.emacs.d/config/functions")

;; -------------------------------------------------------
;; Define package repositories
;; -------------------------------------------------------
(require 'package)

(setq package-archives (append package-archives
			 '(("melpa" . "http://melpa.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
         ("org" . "http://orgmode.org/elpa/")
			   ("elpy" . "http://jorgenschaefer.github.io/packages/"))))

(package-initialize)

(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Keep custom in a separate file
(defconst base-path (file-name-directory load-file-name))
(setq custom-file (concat base-path "config/custom.el"))

(message "****** Loading Packages: %s" (current-time-string))

;; -------------------------------------------------------
;; A
;; -------------------------------------------------------

(use-package avy :ensure t)

(use-package autothemer :ensure t)

;; autocomplete editor
(use-package auto-complete
  :diminish auto-complete-mode
  :config (ac-config-default))

(use-package ag :ensure t
  :commands (ag)
  :config
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (add-to-list 'ag-arguments "--word-regexp")))

(use-package all-the-icons :ensure t)

(use-package anzu :ensure t
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
  :diminish ""
  :config
  (beacon-mode 1))

(use-package bind-key)

(use-package bongo :ensure t
  :commands (bongo)
  :config
  (define-key bongo-playlist-mode-map (kbd "g") #'bongo-redisplay)
  (setq bongo-default-directory "~/Music/iTunes/iTunes Music/"))

;;This file is for lazy people wanting to swap buffers without typing C-x b on each window.
(use-package buffer-move)

;; -------------------------------------------------------
;; C
;; -------------------------------------------------------

;; Chords
;;Define chords using the :chords keyword in the same manner as :bind and related keywords, 
;;using a cons or a list of conses
(use-package use-package-chords :config (key-chord-mode 1))

;;Text completion framework for Emacs
(use-package company :ensure t
  :diminish ""
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
  (global-company-mode)

  (use-package company-statistics
    :config
    (company-statistics-mode))

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
(use-package counsel :ensure t
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c o"   . counsel-find-file-extern)
   ("C-S-s"   . counsel-ag)
   ("C-c l"   . counsel-locate))
  :config
  (setq counsel-find-file-at-point t)
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

(use-package counsel-projectile :ensure t
  :bind* (("H-P" . counsel-projectile-switch-to-buffer)
          ("H-p" . counsel-projectile))
  :config
  (counsel-projectile-on))

(use-package counsel-gtags :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package clojure-cheatsheet)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package color-theme 
  :ensure t
  :config
  (setq my-color-themes (list 'gruvbox 'leuven)))

;; -------------------------------------------------------
;; D
;; -------------------------------------------------------

;; deft
(use-package deft
  :config (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-directory "~/Notes")
  :bind ("<f7>" . deft))

(use-package dired
  :ensure nil
  :bind* (("C-x d" . dired-other-window)
          ("C-x C-d" . dired))
  :commands (dired)
  :config

  (use-package dired-x
    :ensure nil
    :bind* (("C-x C-'" . dired-jump))
    :commands (dired-omit-mode)
    :init
    (add-hook 'dired-load-hook (lambda () (load "dired-x")))
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    :config
    (setq dired-omit-verbose nil)
    (setq dired-omit-files
          (concat dired-omit-files "\\|^\\..*$\\|^.DS_Store$\\|^.projectile$\\|^.git$")))

  (use-package dired-details+ 
    :ensure t
    :config
    (dired-details-install)
    (setq-default dired-details-hidden-string " --- "
                  dired-details-hide-link-targets nil))

  (bind-keys :map dired-mode-map
    ("SPC" . dired-view-other-window)
    ("."   . hydra-dired-main/body)
    ("t"   . dired-next-line)
    ("s"   . dired-previous-line)
    ("r"   . dired-find-file)
    ("c"   . dired-up-directory)
    ("'"   . eshell-here)
    ("8"   . dired-mkdir-date)
    ("9"   . dired-mkdir-date-rstyle)
    ("C-'" . shell)
    ("q"   . (lambda () (interactive) (quit-window 4)))))

;; -------------------------------------------------------
;; E
;; -------------------------------------------------------

;; evil (vim) mode
(use-package evil :ensure t
  :config (setq evil-default-cursor t)
  (setq evil-insert-state-cursor '(bar "white")
      evil-emacs-state-cursor '(bar "white") 
      evil-normal-state-cursor '(hbar "#97C150"))
  (bind-keys :map evil-normal-state-map
         ("r" . evilmr-replace-in-defun))
  (evil-mode t))

;; Easy Motion
(use-package evil-easymotion :ensure t
  :init (evilem-default-keybindings "SPC"))

(use-package evil-escape :ensure t
  :diminish ""
  :init 
   (evil-escape-mode)
  )

(use-package eshell :ensure t
  :defines eshell-here
  :commands (eshell
             eshell-here)
  :config
  (use-package eshell-z :ensure t)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-list-files-after-cd t
        eshell-ls-initial-args "-alh")

  (general-define-key
   :keymaps 'eshell-mode-map
    "<tab>" (lambda () (interactive) (pcomplete-std-complete))
    "C-'" (lambda () (interactive) (insert "exit") (eshell-send-input) (delete-window))))

(use-package evil-matchit :ensure t
  :diminish ""
  :init (global-evil-matchit-mode 1))



;;(use-package esup)
;; -------------------------------------------------------
;; F
;; -------------------------------------------------------

; flycheck
(use-package flycheck :ensure t
  :commands flycheck-mode
  :diminish (flycheck-mode . "ⓕ")
  :config
  (setq flycheck-highlighting-mode 'symbols))

;; -------------------------------------------------------
;; G
;; -------------------------------------------------------

(use-package ggtags :ensure t
  :commands (ggtags-mode)
  :config
  (general-define-key :keymaps 'ggtags-mode-map
    "M-s-," 'ggtags-navigation-mode-abort
    "M-s-." 'ggtags-find-tag-dwim))

;; General
(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

    ;; simple command
    "'"   '(iterm-focus :which-key "iterm")
    "?"   '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "/"   'counsel-ag
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")
    "B" 'ivy-switch-buffer  

    ;; Applications
    "a" '(:ignore t :which-key "Applications")
    "ar" 'ranger
    "ad" 'dired
    "ao" 'org-agenda
    "ac" 'org-capture

    ;; Windows
    "w" '(:ignore t :which-key "Window")
    "wd" 'delete-window
    "wt" 'cycle-my-theme
    "wg" 'golden-ratio-adjust
    "w-" 'split-window-below
    "w|" 'split-window-right
    "b" '(:ignore t :which-key "Buffer")
    "bi" 'ibuffer
    "bb" 'helm-buffers-list

     ;; Tools
    "t" '(:ignore t :which-key "Tools")
    "tj" 'cider-jack-in
    "tt" 'neotree-toggle

     ;; Files
    "f"   '(:ignore t :which-key "files")
    "ff"  'counsel-find-file
    "fr"  'counsel-recentf
    "p"   '(:ignore t :which-key "project")
    "pf"  '(counsel-git :which-key "find file in git dir")))

(use-package git-timemachine)

(use-package git-gutter :ensure t
  :disabled t
  :diminish ""
  :commands (global-git-gutter-mode)
  :init
  (global-git-gutter-mode +1)
  (setq git-gutter:modified-sign "|")
  (setq git-gutter:added-sign "|")
  (setq git-gutter:deleted-sign "|")
  :config
  (setq git-gutter:update-interval 20)
  (git-gutter:linum-setup))

;; git gutter
(use-package git-gutter-fringe
  :if window-system
  :diminish git-gutter-mode
  :config (global-git-gutter-mode))

(use-package goto-chg :ensure t
  :commands (goto-last-change
             goto-last-change-reverse))

(use-package golden-ratio
  diminish ""
  :disabled t
  )

;; -------------------------------------------------------
;; H
;; -------------------------------------------------------

;; Helm
(use-package helm-config
  :ensure helm
  ;; disabled for now, but I've copy and pasted here the advice from
  ;; tuhdo about helm.
  :disabled t
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
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1))

(use-package helm-make
  :bind* (("C-c C" . helm-make)
          ("C-c p c" . helm-make-projectile))
  :config
  (setq helm-make-completion-method 'ivy))

(use-package helm-google :ensure t
  :commands (helm-google))

(use-package helm-gitignore :ensure t
  :commands helm-gitignore)

(use-package hideshow
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package hydra :ensure t
  :config
  (setq hydra-is-helpful t))

(use-package hy-mode :ensure t
  :mode (("\\.hy\\'" . hy-mode))
  :init
  (add-hook 'hy-mode-hook (lambda () (lispy-mode 1))))

;; -------------------------------------------------------
;; I
;; -------------------------------------------------------

(use-package ibuffer :ensure t
  :commands ibuffer
  :init
  (add-hook 'ibuffer-hook (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))
  :config
  (use-package ibuffer-vc :ensure t
    :config
    (setq ibuffer-vc-set-filter-groups-by-vc-root t))

  (general-define-key
   :keymaps 'ibuffer-mode-map
    "." 'hydra-ibuffer-main/body
    "t" 'next-line
    "s" 'previous-line
    "r" 'ibuffer-visit-buffer
    "c" 'ibuffer-backward-filter-group
    "p" 'ibuffer-backward-filter-group
    "n" 'ibuffer-forward-filter-group)

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
   
(use-package ibuffer-vc
  :requires ibuffer
  :config
  (add-hook 'ibuffer-hook
      (lambda ()
        (ibuffer-vc-set-filter-groups-by-vc-root)
        (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))))


(use-package info
  :mode (("\\.info\\'" . Info-mode))
  :config
  (define-key Info-mode-map (kbd ".") #'hydra-info/body)

  (defhydra hydra-info (:color pink
                        :hint nil)
    "
Info-mode:
  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos
  _1_ .. _9_ Pick first .. ninth item in the node's menu.
"
    ("]"   Info-forward-node)
    ("["   Info-backward-node)
    ("n"   Info-next)
    ("p"   Info-prev)
    ("s"   Info-search)
    ("S"   Info-search-case-sensitively)

    ("l"   Info-history-back)
    ("r"   Info-history-forward)
    ("H"   Info-history)
    ("t"   Info-top-node)
    ("<"   Info-top-node)
    (">"   Info-final-node)

    ("u"   Info-up)
    ("^"   Info-up)
    ("m"   Info-menu)
    ("g"   Info-goto-node)
    ("b"   beginning-of-buffer)
    ("e"   end-of-buffer)

    ("f"   Info-follow-reference)
    ("i"   Info-index)
    (","   Info-index-next)
    ("I"   Info-virtual-index)

    ("T"   Info-toc)
    ("d"   Info-directory)
    ("c"   Info-copy-current-node-name)
    ("C"   clone-buffer)
    ("a"   info-apropos)

    ("1"   Info-nth-menu-item)
    ("2"   Info-nth-menu-item)
    ("3"   Info-nth-menu-item)
    ("4"   Info-nth-menu-item)
    ("5"   Info-nth-menu-item)
    ("6"   Info-nth-menu-item)
    ("7"   Info-nth-menu-item)
    ("8"   Info-nth-menu-item)
    ("9"   Info-nth-menu-item)

    ("?"   Info-summary "Info summary")
    ("h"   Info-help "Info help")
    ("q"   Info-exit "Info exit")
    ("C-g" nil "cancel" :color blue))
  )

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :commands (ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :config
  (ivy-mode 1)

  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  ;; from https://github.com/company-mode/company-statistics
  ;; ignore buffers in the ignore buffer list.
  (setq ivy-use-ignore-default 'always)
  (setq ivy-ignore-buffers '("company-statistics-cache.el" "company-statistics-autoload.el"))
  ;; if ivy-flip is t, presents results on top of query.
  (setq ivy-flip t)
  (setq ivy-overlay-at nil)
  (setq ivy-re-builders-alist '((t      . ivy--regex-ignore-order)))

  (defun ivy--matcher-desc ()
    (if (eq ivy--regex-function
            'ivy--regex-fuzzy)
        "fuzzy"
      "ivy"))

  (defhydra hydra-ivy (:hint nil
                       :color pink)
    "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _b_/_é_/_p_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _s_ ^ ^ | _f_ollow occ_u_r | _i_nsert | _C_: calling %-5s(if ivy-calling \"on\" \"off\") _C-c_ase-fold: %-10`ivy-case-fold-search
_c_ ^+^ _r_ | _d_one      ^ ^  | _o_ops   | _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _T_runcate: %-11`truncate-lines
^ ^ _t_ ^ ^ | _g_o        ^ ^  | ^ ^      | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _D_efinition of this menu
"
    ;; arrows
    ("c" ivy-beginning-of-buffer)
    ("t" ivy-next-line)
    ("s" ivy-previous-line)
    ("r" ivy-end-of-buffer)
    ;; actions
    ("o" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("i" nil)
    ("C-o" nil)
    ("f" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ("d" ivy-done :exit t)
    ("g" ivy-call)
    ("C-m" ivy-done :exit t)
    ("C" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("b" ivy-prev-action)
    ("é" ivy-next-action)
    ("p" ivy-read-action)
    ("T" (setq truncate-lines (not truncate-lines)))
    ("C-c" ivy-toggle-case-fold)
    ("u" ivy-occur :exit t)
    ("D" (ivy-exit-with-action
          (lambda (_) (find-function 'hydra-ivy/body)))
     :exit t))

  (defun ivy-switch-project ()
    (interactive)
    (ivy-read
     "Switch to project: "
     (if (projectile-project-p)
         (cons (abbreviate-file-name (projectile-project-root))
               (projectile-relevant-known-projects))
       projectile-known-projects)
     :action #'projectile-switch-project-by-name))

  (global-set-key (kbd "C-c m") 'ivy-switch-project)

  (ivy-set-actions
   'ivy-switch-project
   '(("d" dired "Open Dired in project's directory")
     ("v" counsel-projectile "Open project root in vc-dir or magit")
     ("c" projectile-compile-project "Compile project")
     ("r" projectile-remove-known-project "Remove project(s)"))))

;; -------------------------------------------------------
;; M
;; -------------------------------------------------------

;; magit
(use-package magit :ensure t
  :commands (magit-blame
             magit-commit
             magit-commit-popup
             magit-diff-popup
             magit-diff-unstaged
             magit-fetch-popup
             magit-init
             magit-log-popup
             magit-pull-popup
             magit-push-popup
             magit-revert
             magit-stage-file
             magit-status
             magit-unstage-file
             magit-blame-mode)
  :bind (("s-v" . magit-status))

  :config
  (use-package git-modes)

  (global-git-commit-mode)

  (general-define-key
   :keymaps 'magit-mode-map
    "'" #'eshell-here)

  (use-package magit-popup :ensure t)
  (use-package git-commit :ensure t :defer t)

  (use-package magit-gitflow :ensure t
    :commands
    turn-on-magit-gitflow
    :general
    (:keymaps 'magit-mode-map
     "%" 'magit-gitflow-popup)
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

  (setq magit-completing-read-function 'ivy-completing-read))

;; markdown mode

(use-package markdown-mode :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\'"   . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode 0)))

  (defhydra hydra-markdown (:hint nil)
    "
Formatting         _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code
Headings           _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
Lists              _m_: insert item
Demote/Promote     _l_: promote       _r_: demote     _U_: move up      _D_: move down
Links, footnotes   _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
undo               _u_: undo
"


    ("s" markdown-insert-bold)
    ("e" markdown-insert-italic)
    ("b" markdown-insert-blockquote :color blue)
    ("p" markdown-insert-pre :color blue)
    ("c" markdown-insert-code)

    ("h" markdown-insert-header-dwim)
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)

    ("m" markdown-insert-list-item)

    ("l" markdown-promote)
    ("r" markdown-demote)
    ("D" markdown-move-down)
    ("U" markdown-move-up)

    ("L" markdown-insert-link :color blue)
    ("U" markdown-insert-uri :color blue)
    ("F" markdown-insert-footnote :color blue)
    ("W" markdown-insert-wiki-link :color blue)
    ("R" markdown-insert-reference-link-dwim :color blue)

    ("u" undo :color teal)
    )

  (general-define-key
   :keymaps 'markdown-mode-map
   :prefix "C-,"
    "," 'hydra-markdown/body
    "=" 'markdown-promote
    "°" 'markdown-promote-subtree
    "-" 'markdown-demote
    "8" 'markdown-demote-subtree
    "o" 'markdown-follow-thing-at-point
    "j" 'markdown-jump
    "»" 'markdown-indent-region
    "«" 'markdown-exdent-region
    "gc" 'markdown-forward-same-level
    "gr" 'markdown-backward-same-level
    "gs" 'markdown-up-heading)

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
  :config
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
  :defer t
  :config
  (load-file "~/.emacs.d/config/org.el"))

;; -------------------------------------------------------
;; P
;; -------------------------------------------------------

(use-package paradox :ensure t
  :commands (paradox-list-packages
             package-list-packages))

; (use-package paredit
;   :diminish ""
;   :ensure t
;   :config
;   (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;   ;; enable in the *scratch* buffer
;   (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
;   (add-hook 'ielm-mode-hook #'paredit-mode)
;   (add-hook 'lisp-mode-hook #'paredit-mode)
;   (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package persp-mode :ensure t
  :defer t
  ;;:quelpa (persp-mode :fetcher github :repo "Bad-ptr/persp-mode.el")
  :diminish (persp-mode . "")
  :commands (persp-mode
             persp-next
             persp-prev
             pers-switch)
  :config

  (setq wg-morph-on nil)                ; switch off animation ?
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (setq persp-nil-name "nil")

  (defhydra hydra-persp (:hint nil :color blue)
    "
^Nav^        ^Buffer^      ^Window^     ^Manage^      ^Save/load^
^---^        ^------^      ^------^     ^------^      ^---------^
_n_: next    _a_: add      ^ ^          _r_: rename   _w_: save
_p_: prev    _b_: → to     ^ ^          _c_: copy     _W_: save subset
_s_: → to    _i_: import   _S_: → to    _C_: kill     _l_: load
^ ^          ^ ^           ^ ^          ^ ^           _L_: load subset
"
    ("n" persp-next :color red)
    ("p" persp-prev :color red)
    ("s" persp-switch)
    ("S" persp-window-switch)
    ("r" persp-rename)
    ("c" persp-copy)
    ("C" persp-kill)
    ("a" persp-add-buffer)
    ("b" persp-switch-to-buffer)
    ("i" persp-import-buffers-from)
    ("I" persp-import-win-conf)
    ("o" persp-mode)
    ("w" persp-save-state-to-file)
    ("W" persp-save-to-file-by-names)
    ("l" persp-load-state-from-file)
    ("L" persp-load-from-file-by-names)
    ("q" nil "quit"))

  (global-set-key (kbd "H-p") 'persp-prev)
  (global-set-key (kbd "H-n") 'persp-next))

;; popwin
(use-package popwin :ensure t
  :config
  (popwin-mode 1))

(use-package powerline-evil :ensure t)

(use-package projectile :ensure t
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

  (use-package org-projectile :ensure t
    :config
    (org-projectile:per-repo)
    (setq org-projectile:per-repo-filename "project_todo.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p")))

  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (defun hydra-projectile-if-projectile-p ()
    (interactive)
    (if (projectile-project-p)
        (hydra-projectile/body)
      (counsel-projectile)))

  (defhydra hydra-projectile
    (:color teal :hint nil
     :pre (projectile-mode))
    "
     PROJECTILE: %(projectile-project-root)
    ^FIND FILE^        ^SEARCH/TAGS^        ^BUFFERS^       ^CACHE^                    ^PROJECT^
    _f_: file          _a_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch proj
    _F_: file dwim     _g_: update gtags    _b_: switch to  _x_: remove known project
  _C-f_: file pwd      _o_: multi-occur   _s-k_: Kill all   _X_: cleanup non-existing
    _r_: recent file   ^ ^                  ^ ^             _z_: cache current
    _d_: dir
   ^SHELL^
   _e_: eshell
"
    ("e"   projectile-run-eshell)
    ("a"   projectile-ag)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("f"   projectile-find-file)
    ("F"   projectile-find-file-dwim)
    ("C-f" projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("p"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("q"   nil "cancel" :color blue)))

(use-package python
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

(use-package rainbow-mode :ensure t
  :diminish rainbow-mode
  :init (rainbow-mode))

(use-package rainbow-delimiters  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ranger :ensure t
  :commands
  (ranger
   deer)
  :general
  (:keymaps 'ranger-mode-map
   "t" 'ranger-next-file      ; j
   "s" 'ranger-prev-file      ; k
   "r" 'ranger-find-file      ; l
   "c" 'ranger-up-directory   ; c
   "j" 'ranger-toggle-mark    ; t
   )

  :config
  (setq ranger-cleanup-eagerly t))

(use-package recentf :ensure t
  :commands (recentf-mode
             counsel-recentf)
  :config
  (setq recentf-max-saved-items 50))

(use-package restart-emacs :ensure t
  :commands restart-emacs)

;; -------------------------------------------------------
;; S
;; -------------------------------------------------------

(use-package smart-mode-line
    :ensure t)

(setq sml/no-confirm-load-theme t)
(setq sml/theme nil)
(add-hook 'after-init-hook #'sml/setup)

(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

;; SMEX
(use-package smex :ensure t
  :config (smex-initialize)
  :bind ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c M-x" . execute-extended-command))

(use-package swiper :ensure t
  :bind* (("M-s" . swiper)
          ("M-S" . swiper-all)
          :map swiper-map
          ("C-s" . ivy-previous-history-element)
          ("C-t" . ivy-yank-word)))

;; -------------------------------------------------------
;; T
;; -------------------------------------------------------

(use-package tile :ensure t
  :defines (hydra-tile/body)
  :bind* (("C-c t" . hydra-tile/body)
          ("s-t" . tile))
  :config
  (defhydra hydra-tile (:hint nil :color red :columns 4
                        :body-pre (winner-mode 1))
    "tile "
    ("a" (tile :strategy tile-tall-3) "tall 3")
    ("u" (tile :strategy (tile-split-n-tall 4)) "tall 4")
    ("i" (tile :strategy (tile-split-n-wide 2)) "wide 2")
    ("e" (tile :strategy (tile-split-n-wide 3)) "wide 3")
    ("c" (tile :strategy tile-master-left-3) "left 3")
    ("t" (tile :strategy tile-master-bottom-3) "bottom 3")
    ("s" (tile :strategy tile-master-top-3) "top 3")
    ("r" (tile :strategy tile-master-right-3) "right 3")

    ("m" tile-select "chose")
    ("w" (tile :strategy tile-one) "one")
    ("n" tile "tile")
    ("C-u" winner-undo "undo")
    ("M-u" winner-redo "redo")
    ("é" hydra-window/body "windows" :color blue)

    ("q" nil :color blue "quit"))

  (setq tile-cycler
        (tile-strategies :strategies
          (list tile-tall-3
                tile-master-left-3
                tile-master-top-3
                tile-one))))

(use-package tiny :ensure t
  :bind* (("C-;" . tiny-expand)))

;; -------------------------------------------------------
;; U
;; -------------------------------------------------------

;; better redo/undo
(use-package undo-tree :ensure t
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
        which-key-min-display-lines 7))

(use-package window-numbering :ensure t
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

;; -------------------------------------------------------
;; Y
;; -------------------------------------------------------

;; snippets
(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode 1))

(use-package yaml-mode
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

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; OSX specific settings
(when (eq system-type 'darwin)
  (load "~/.emacs.d/config/osx"))

;; date and time in status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(setq frame-title-format "Some days you eat the bear, some days the bear eats you.")
(setq version-control t )   ; use version control
(setq vc-make-backup-files t )    ; make backups file even when in version controlled dir
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

(set-frame-font "Source Code Pro-14")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)

;;;;; Theme ;;;;;
(setq my-themes '(gruvbox leuven))

(setq my-cur-theme nil)
(defun cycle-my-theme ()
  "Cycle through a list of themes, my-themes"
  (interactive)
  (when my-cur-theme
    (disable-theme my-cur-theme)
    (setq my-themes (append my-themes (list my-cur-theme))))
  (setq my-cur-theme (pop my-themes))
  (load-theme my-cur-theme t))

;; Switch to the first theme in the list above
(cycle-my-theme)

;; Bind this to C-t
(global-set-key (kbd "C-t") 'cycle-my-theme)

(message "***** Loading Additional Config Files: %s" (current-time-string))
(load "~/.emacs.d/config/custom")
(load "~/.emacs.d/config/keybindings")

;; go full screen
(toggle-frame-maximized) 
