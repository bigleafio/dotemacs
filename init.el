;;;;
;; Packages
;;;;

;;; code:

;; -------------------------------------------------------
;; User Info
;; -------------------------------------------------------

(setq user-full-name "Jason Graham")
(setq user-mail-address "jgraham20@gmail.com")
(load "~/.emacs.d/config/functions")

;; -------------------------------------------------------
;; Define package repositories
;; -------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives (append package-archives
			 '(("melpa" . "http://melpa.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
         ("org" . "http://orgmode.org/elpa/")
			   ("elpy" . "http://jorgenschaefer.github.io/packages/"))))

(package-initialize)

;; install use package, the rest of the time we use :ensure to install.
(package-install 'use-package)

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; bring in use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; Keep custom in a separate file
(defconst base-path (file-name-directory load-file-name))
(setq custom-file (concat base-path "config/custom.el"))

;; -------------------------------------------------------
;; AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
;; -------------------------------------------------------

(use-package avy :ensure t)

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
;; BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
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

;; -------------------------------------------------------
;; CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
;; -------------------------------------------------------

;; Chords
(use-package use-package-chords :config (key-chord-mode 1))

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
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")

  ;; from http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
  (defun counsel-yank-bash-history ()
    "Yank the bash history"
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r")      ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                             (buffer-string))
                           "\n"
                           t)))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Bash history:") collection))))
        (insert val)
        (message "%s => kill-ring" val))))

  ;; TODO make the function respects reverse order of file
  (defun counsel-yank-zsh-history ()
    "Yank the zsh history"
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r")      ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zhistory"))
                                             (buffer-string))
                           "\n"
                           t)))
      (setq collection (mapcar (lambda (it) (replace-regexp-in-string ".*;" "" it)) collection))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Zsh history:") collection :re-builder #'ivy--regex-ignore-order))))
        (kill-new val)
        (insert val)
        (message "%s => kill-ring" val))))

  (defun counsel-package-install ()
    (interactive)
    (ivy-read "Install package: "
              (delq nil
                    (mapcar (lambda (elt)
                              (unless (package-installed-p (car elt))
                                (symbol-name (car elt))))
                            package-archive-contents))
              :action (lambda (x)
                        (package-install (intern x)))
              :caller 'counsel-package-install))
  (ivy-set-actions
   'counsel-find-file
   '(("o" (lambda (x) (counsel-find-file-extern x)) "open extern"))))

(use-package counsel-osx-app :ensure t
  :commands counsel-osx-app
  :bind*
  ("C-c a" . counsel-osx-app)
  :config
  (setq counsel-osx-app-location
        '("/Applications/" "~/Applications/" )))

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

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; -------------------------------------------------------
;; DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
;; -------------------------------------------------------

;; duplication things
(use-package duplicate-thing
  :bind ("s-d" . duplicate-thing))

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

  (use-package dired-sort 
    :ensure t
    :defines (hydra-dired-sort/body)
    :commands (hydra-dired-sort/body
               dired-sort-name
               dired-sort-size
               dired-sort-time
               dired-sort-ctime
               dired-sort-utime
               dired-sort-extension)
    :config
    (defhydra hydra-dired-sort
      (:color blue
       :hint nil
       :post (hydra-dired-main/body))
      "
_n_ame
_s_ize
_t_ime
_e_xtension"
      ("n" dired-sort-name)
      ("s" dired-sort-size)
      ("t" dired-sort-time)
      ("e" dired-sort-extension)))

  (use-package dired-quick-sort :ensure t
    ;; press S in dired to see a nice hydra for sorting
    :config
    (dired-quick-sort-setup))


  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls)
        (setq insert-directory-program gls)))

  (setq dired-listing-switches "-XGalg --group-directories-first --human-readable --dired")
  (setq dired-dwim-target t)     ; guess copy target based on other dired window

  (defun dired-view-other-window ()
    "View the current file in another window (possibly newly created)."
    (interactive)
    (if (not (window-parent))
        (split-window))
    (let ((file (dired-get-file-for-visit))
          (dbuffer (current-buffer)))
      (other-window 1)
      (unless (equal dbuffer (current-buffer))
        (if (or view-mode (equal major-mode 'dired-mode))
            (kill-buffer)))
      (let ((filebuffer (get-file-buffer file)))
        (if filebuffer
            (switch-to-buffer filebuffer)
          (view-file file))
        (other-window -1))))

  (defun dired-mkdir-date (dir-name)
    "Make a directory with current date style"
    (interactive "sDirectory content: ")
    (mkdir (format "%s-%s" (format-time-string "%Y-%m-%d" (current-time)) dir-name))
    (revert-buffer))

  (defun dired-mkdir-date-rstyle (dir-name)
    (interactive "sDirectory content: ")
    (mkdir (format "%s.%s" dir-name (format-time-string "%Y%m%d" (current-time))))
    (revert-buffer))

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
;; EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
;; -------------------------------------------------------

;; evil (vim) mode
(use-package evil
  :config (setq evil-default-cursor t)
  (setq evil-insert-state-cursor '(bar "white")
      evil-emacs-state-cursor '(bar "white") 
      evil-normal-state-cursor '(hbar "#97C150"))
  (bind-keys :map evil-normal-state-map
         ("r" . evilmr-replace-in-defun))
  (evil-mode t))

;; Easy Motion
(use-package evil-easymotion
  :init (evilem-default-keybindings "SPC"))

(use-package evil-escape
  :init 
   (evil-escape-mode)
  )

(use-package edebug
  :defer t
  :config
  (setq edebug-active nil)
  (setq edebug-outside-windows t))

(use-package eshell
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

  (setq eshell-directory-name "~/dotfile/emacs/eshell/")

;; from http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun eshell-here ()
"
Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier.
"
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))

      (insert (concat "ls"))
      (eshell-send-input)))

  (defun eshell/ll ()
    (insert "ls -l")
    (eshell-send-input))

  (general-define-key
   :keymaps 'eshell-mode-map
    "<tab>" (lambda () (interactive) (pcomplete-std-complete))
    "C-'" (lambda () (interactive) (insert "exit") (eshell-send-input) (delete-window))))

(use-package ereader :ensure t
  :mode (("\\.epub\\'" . ereader-mode)))

(use-package evil-matchit
  :diminish t
  :init (global-evil-matchit-mode 1))

;; path fix
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; -------------------------------------------------------
;; FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
;; -------------------------------------------------------

; flycheck
(use-package flycheck :ensure t
  :commands flycheck-mode
  :diminish (flycheck-mode . "ⓕ")
  :config
  (setq flycheck-highlighting-mode 'symbols))

;; -------------------------------------------------------
;; GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
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
    "B" 'ivy-switch-buffer  ; change buffer, chose using ivy
    ;; Applications
    "a" '(:ignore t :which-key "Applications")
    "ar" 'ranger
    "ad" 'dired
    "ao" 'org-agenda
    "ac" 'org-capture
    "w" '(:ignore t :which-key "Window")
    "wd" 'delete-window
    "b" '(:ignore t :which-key "Window")
    "bi" 'ibuffer
    "bb" 'helm-buffers-list
     ;; bind to double key press
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

(use-package grab-mac-link :ensure t
  :commands grab-mac-link)

;; -------------------------------------------------------
;; HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
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
;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
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

(use-package ivy
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
;; MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;; -------------------------------------------------------

;; magit
(use-package magit
  ;;:quelpa (magit :fetcher github :repo "magit/magit")
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
  (use-package git-modes
    ;;:quelpa (git-modes :fetcher github :repo "magit/git-modes")
    )

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
;; NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
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
;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
;; -------------------------------------------------------

(use-package osx-clipboard :ensure t
  :if (not (window-system))
  :init
  (osx-clipboard-mode +1))

;; TODO work on outline hydra. useful for tex
(use-package outline
  :bind (("H-<tab>" . hydra-outline/body))
  :commands (outline-hide-body
             outline-show-all
             outline-minor-mode
             hydra-outline/body)
  :general ("M-s-c" 'outline-previous-heading
            "M-s-r" 'outline-next-heading)
  :diminish ((outline-minor-mode . "")
             (outline-mode . ""))
  :config
  (outline-minor-mode)

  (defun outline-narrow-to-subtree ()
    (interactive)
    (outline-mark-subtree)
    (narrow-to-region (region-beginning) (region-end)))

  (defun outline-indent-subtree ()
    (interactive)
    (save-excursion
      (outline-mark-subtree)
      (indent-region (region-beginning) (region-end))
      (message nil)))

  (bind-key "C-c @ n" 'outline-narrow-to-subtree)
  (bind-key "C-c @ i" 'outline-indent-subtree)

  (defhydra hydra-outline
    (:hint nil :body-pre (outline-minor-mode 1))
    "
Outline
^CURRENT^    ^ALL^      ^LEAVES^      ^MANIPULATE^
_c_: hide    _C_: hide  _C-c_: hide    _M-r_: demote
_t_: next    ^ ^        _C-r_: show    _M-c_: promote
_s_: prev    ^ ^        ^   ^          _M-t_: move down
_r_: show    _R_: show  ^   ^          _M-s_: move up
"
    ("C" outline-hide-body)
    ("t" outline-next-visible-heading)
    ("s" outline-previous-visible-heading)
    ("R" outline-show-all)
    ("c" outline-hide-subtree)
    ("r" outline-show-subtree)

    ("C-c" outline-hide-leaves)
    ("C-r" outline-show-subtree)

    ("M-r" outline-demote)
    ("M-c" outline-promote)
    ("M-t" outline-move-subtree-down)
    ("M-s" outline-move-subtree-up)

    ("i" outline-insert-heading "insert heading" :color blue)
    ("q" nil "quit" :color blue)))

;; -------------------------------------------------------
;; PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
;; -------------------------------------------------------

(use-package paradox :ensure t
  :commands (paradox-list-packages
             package-list-packages))

(use-package pbcopy :ensure t
  :if (not (display-graphic-p))
  :init
  (turn-on-pbcopy))

(use-package pdf-tools :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq pdf-tools-enabled-modes
        '(pdf-isearch-minor-mode
          pdf-links-minor-mode
          pdf-misc-minor-mode
          pdf-outline-minor-mode
          pdf-misc-size-indication-minor-mode
          pdf-misc-menu-bar-minor-mode
          pdf-sync-minor-mode
          pdf-misc-context-menu-minor-mode
          pdf-cache-prefetch-minor-mode
          pdf-view-auto-slice-minor-mode))
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (require 'pdf-occur)
  (require 'pdf-links)
  (require 'pdf-outline)
  (require 'pdf-sync)
  (pdf-tools-install)

  (setq pdf-view-continuous t)

  (defun pdf-view-open-in-external-app ()
    (interactive)
    (shell-command (format "open -a Preview %s" (buffer-file-name))))

  (defun pdf-view-finder ()
    (interactive)
    (shell-command "open -a Finder ./"))

  (bind-keys :map pdf-view-mode-map
    ("r" . pdf-view-next-page)
    ("c" . pdf-view-previous-page)
    ("O" . pdf-view-open-in-external-app)
    ("s" . pdf-view-previous-line-or-previous-page)
    ("t" . pdf-view-next-line-or-next-page)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package persp-mode
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
(use-package popwin
  :config
  (popwin-mode 1))

;; powerline
(use-package powerline
 :init
  (powerline-evil-center-color-theme))

(add-hook 'after-init-hook 'powerline-reset)

(use-package powerline-evil)

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
;; RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
;; -------------------------------------------------------
;; Rainbow Mode
(use-package rainbow-mode
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

(use-package recentf
  :commands (recentf-mode
             counsel-recentf)
  :config
  (setq recentf-max-saved-items 50))

(use-package restart-emacs :ensure t
  :commands restart-emacs)

;; -------------------------------------------------------
;; SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
;; -------------------------------------------------------

;; SMEX
(use-package smex
  :config (smex-initialize)
  :bind ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c M-x" . execute-extended-command))

(use-package smart-mode-line)
(use-package smart-mode-line-powerline-theme)
(setq sml/no-confirm-load-theme t)
(sml/setup)

(use-package solarized-color-themes
  :disabled t
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/color-theme-solarized-20160626.743/")
  (set-frame-parameter (selected-frame) 'background-mode 'dark)
  (load-theme 'solarized t)

  (setq-default cursor-type 'box)
  (add-to-list 'default-frame-alist '(cursor-color . "#d33682"))
  (set-cursor-color "#d33682")

  (defun solarized--dark-or-light (bgd)
    (set-frame-parameter (selected-frame) 'background-mode bgd)
    (enable-theme 'solarized)
    (set-cursor-color "#d33682")))

(use-package swiper :ensure t
  :bind* (("M-s" . swiper)
          ("M-S" . swiper-all)
          :map swiper-map
          ("C-s" . ivy-previous-history-element)
          ("C-t" . ivy-yank-word)))

;; -------------------------------------------------------
;; TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
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
;; UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU
;; -------------------------------------------------------

;; better redo/undo
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  :bind ("s-z" . undo)
  ("s-Z" . redo))


(use-package undohist
  :config (undohist-initialize))

;; -------------------------------------------------------
;; WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
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
;; YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
;; -------------------------------------------------------

;; snippets
(use-package yasnippet
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


;; ------------------------------------------------------
;; Theme
;; ------------------------------------------------------

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;;(use-package color-theme-solarized)
;;(load-theme color-theme-solarized t)
(load-theme 'leuven t)
(set-frame-font "Source Code Pro 12")
;;(load-theme 'tomorrow-night t)

(load "~/.emacs.d/config/custom")
(load "~/.emacs.d/config/keybindings")
(load "~/.emacs.d/config/org")

;; go full screen
(toggle-frame-maximized) 
