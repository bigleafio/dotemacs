(general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

    ;; simple command
    "'"   '(iterm-focus :which-key "iterm")
    "?"   '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "/"   'counsel-ag
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" '(counsel-M-x)
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
    "wc" 'cycle-my-theme
    "wt" 'transpose-frame
    "wg" 'golden-ratio-adjust
    "w-" 'split-window-below
    "w|" 'split-window-right

    "b" '(:ignore t :which-key "Buffer")
    "bi" 'ibuffer
    "bb" 'helm-buffers-list
    "bd" 'kill-buffer

     ;; Tools
    "t" '(:ignore t :which-key "Tools")
    "tj" 'cider-jack-in
    "tt" 'neotree-toggle

     ;; Files
    "f"   '(:ignore t :which-key "files")
    "ff"  'counsel-find-file
    "fr"  'counsel-recentf
    "p"   '(:ignore t :which-key "project")
    "pf"  '(counsel-git :which-key "find file in git dir"))

;;; Shift
  (general-define-key
   "S-<return>" 'projectile-find-file-dwim)

;;; Alt-
  (general-define-key
   "ð" 'kill-word-ap)

;;; C-
  (general-define-key
   "C-S-c" 'sp-splice-sexp
   "C-S-z" 'undo-tree-redo
   "C-S-s" 'counsel-ag
   "C-S-k" 'kill-whole-line
   "C-S-n" 'next-line-end
   "C-S-p" 'prev-line-end

   "C- " 'mark-line
   "C-é" 'hydra-window/body
   "C-€" 'hydra-transparency/body
   "C-l" (lambda () (interactive) (avy-goto-line 4))
   "C-'" 'avy-goto-word-or-subword-1
   "C-." 'hydra-main/body
   "C--" 'kill-word-ap)

;;; C-M-
  (general-define-key
   "C-M-i" 'complete-symbol)

;;; C-c
  (general-define-key
   "C-c v" 'magit-status
   "C-c T" 'hydra-transparency/body)

;;; C-x
  (general-define-key
   "C-x SPC" 'hydra-rectangle/body
   "C-x d" 'dired-other-window
   "C-x l" 'sam--firefox-plain-link
   "C-x n" 'narrow-or-widen-dwim
   "C-x p" 'hydra-projectile/body
   "C-x o" 'other-window
   "C-x v" 'hydra-git/body
   "C-x =" 'balance-windows

   "C-x C-b" 'ibuffer
   "C-x C-r" 'ivy-switch-buffer
   "C-x M-b" 'hydra-frame/body
   "C-x M-i" 'sam--insert-timestamp
   "C-x M-c" 'compile)

;;; M-
  (general-define-key
   "M-<backspace>" 'delete-to-sentence-beg
   "M-é" 'ace-window
   "M-/" 'hippie-expand
   "M-«" 'beginning-of-buffer
   "M-»" 'end-of-buffer
   "M-ê" 'hydra-error/body

   "M-s-n" 'forward-paragraph
   "M-s-p" 'backward-paragraph
   "M-s-i" 'shell-command-on-buffer)

;;; s-
  (general-define-key
   "s-<backspace>" 'ivy-switch-buffer
   "s-S-<backspace>" 'projectile-switch-to-buffer
   "s-<tab>" 'sam--switch-to-other-buffer
   "s-c" 'windmove-left
   "s-r" 'windmove-right
   "s-d" 'kill-buffer-and-window
   "s-f" 'projectile-find-file
   "s-i" (lambda () (interactive) (save-excursion (mark-paragraph) (indent-region (region-beginning) (region-end))))
   "s-j" (lambda () (interactive) (join-line 4))
   "s-k" (lambda () (interactive)
           (save-excursion
             (move-beginning-of-line nil)
             (kill-visual-line -1)))    ; delete previous line
   "s-l" 'sam--comment-or-uncomment-region-or-line
   "s-o" 'sam--open-in-external-app
   "s-q" nil                            ; don't close emacs with option q.
   "s-t" nil                            ; don't show font panel with s-t.
   "s-u" 'negative-argument
   "s-w" 'delete-other-windows
   "s-n" 'narrow-or-widen-dwim
   "s-z" 'hydra-zoom/body
   "s-'" 'avy-goto-char-2
   "s-." 'hydra-secondary/body
   "s-\"" 'ffap
   "s-(" 'hydra-sp/body
   "s-SPC" 'pop-global-mark)

;;; H-
  (general-define-key
   "H-<backspace>" 'ivy-switch-buffer-other-window
   "H-<tab>" 'hydra-outline/body
   ;; "H-'" 'sam--iterm-goto-filedir-or-home
   "H-F" 'toggle-frame-maximized
   "H-b" 'projectile-ibuffer
   "H-e" 'eshell-here
   "H-f" 'toggle-frame-fullscreen
   "H-i" 'sam/insert-filename
   "H-l" 'sam--duplicate-line
   "H-m" 'delete-other-frames
   "H-n" 'buffer-to-new-frame
   "H-s" 'move-text-up
   "H-t" 'move-text-down
   "H-r" 'counsel-recentf
   "H-u" 'revert-buffer
   "H-w" 'ace-delete-window

   ;; H-M-
   "H-M-p" 'scroll-up-command
   "H-M-n" 'scroll-down-command
   "H-M-s" 'mark-sexp)

  (general-define-key
   (general-chord ",,") (lambda () (interactive) (insert ";"))
   (general-chord "aa") (lambda () (interactive) (insert "@"))
   (general-chord "qq") #'avy-goto-word-or-subword-1
   (general-chord "qb") #'ivy-switch-buffer
   (general-chord "qd") #'kill-this-buffer
   (general-chord "qf") #'delete-frame
   (general-chord "ql") (lambda () (interactive) (avy-goto-line 4))
   (general-chord "qs") #'save-buffer
   (general-chord "qw") #'delete-window
   (general-chord "VV") #'magit-status)

;;; Mode specific map
  (general-define-key :keymaps 'Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
  (general-define-key :keymaps 'emacs-lisp-mode-map "s-e" 'eval-defun)
  (general-define-key :keymaps 'shell-mode-map "H-c" 'erase-buffer)
  (general-define-key :keymaps 'term-mode-map "H-c" 'erase-buffer)
  (general-define-key
   :keymaps 'compilation-mode-map
    "t" 'compilation-next-error
    "s" 'compilation-previous-error
    "r" 'compile-goto-error)

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

  (general-define-key
   :keymaps 'ibuffer-mode-map
    "." 'hydra-ibuffer-main/body
    "t" 'next-line
    "s" 'previous-line
    "r" 'ibuffer-visit-buffer
    "c" 'ibuffer-backward-filter-group
    "p" 'ibuffer-backward-filter-group
    "n" 'ibuffer-forward-filter-group)
