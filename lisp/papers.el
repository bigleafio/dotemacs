(use-package org-ref)


(setq org-ref-notes-directory "$SOME"
      org-ref-bibliography-notes "$SOME/index.org"
      org-ref-default-bibliography '("$SOME/index.bib")
      org-ref-pdf-directory "~/Dropbox/Library/")

(setq helm-bibtex-bibliography "$SOME/index.bib" ;; where your references are stored
      helm-bibtex-library-path "$SOME/lib/" ;; where your pdfs etc are stored
      helm-bibtex-notes-path "$SOME/index.org" ;; where your notes are stored
      bibtex-completion-bibliography "$SOME/index.bib" ;; writing completion
      bibtex-completion-notes-path "$SOME/index.org"
)

(provide 'papers)
