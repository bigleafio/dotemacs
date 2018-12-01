(require 'package)

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
    ll-the-icons-ivy
    eyebrowse
    ivy-rich
    helm
    dired
    dired-x
    dired-sidebar
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
