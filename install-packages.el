(require 'package)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

 (eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

 (setq mac-option-modifier 'meta)
 (setq mac-command-modifier 'super)
 (setq mac-pass-command-to-system nil)

(eval-and-compile
  (setq load-prefer-newer t
      package-user-dir "~/.emacs.d/elpa"
      package--init-file-ensured t
      package-enable-at-startup nil)

(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t))):

(setq use-package-verbose t)

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(eval-when-compile
  (require 'package)

 (unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
 (unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
 (unless (assoc-default "gnu" package-archives)
 (add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(unless (assoc-default "marmalade" package-archives)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t))
(unless package--initialized (package-initialize t))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t))


(defvar my-packages
  '(delight
    s
    diminish
    sr-speedbar
    eyebrowse
    vscode-icon
    shackle
    imenu-list
    tabbar
    evil
    evil-escape
    evil-anzu
    smart-mode-line
    rich-minority
    ivy
    counsel
    perspective
    persp-projectile
    swiper
    avy
    all-the-icons-ivy
    eyebrowse
    ivy-rich
    helm
    ;;dired
    ;;dired-x
    ;;dired-sidebar
    ibuffer
    ibuffer-sidebar
    ag
    shell-pop
    restart-emacs
    smex
    simpleclip
    which-key
    general
    ace-window
    treemacs
    treemacs-evil
    treemacs-projectile
    undo-tree
    aggressive-indent
    fill-column-indicator
    company
    exec-path-from-shell
    flycheck
    magit
    evil-magit
    diff-hl
    git-gutter
    git-timemachine
    gist
    yasnippet
    projectile
    all-the-icons
    counsel-projectile
    lispy
    cider
    clojure-mode
    smartparens
    evil-smartparens
    rainbow-mode
    multiple-cursors
    elpy
    pyenv-mode
    py-autopep8
    web-mode
    markdown-mode
    imenu-list
    org
    toc-org
    helm-bibtex
    biblio
    org-ref
    rainbow-delimiters
    ox-hugo
    adaptive-wrap
    s
    org-download
    calfw
    atom-one-dark
    )
    )

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))


(when (not package-archive-contents)
    (package-refresh-contents))
(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'install-packages)
