
;; ---------- GLOBAL ------------------------------------------------------
;;(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
;;(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key [remap fill-paragraph] #'sam/fill-or-unfill)
(global-set-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)
(global-set-key [remap move-end-of-line] #'sam/end-of-code-or-line)
(global-set-key (kbd "C-x C-S-e") #'eval-and-replace)

(global-set-key (kbd "<f5>") 'mu4e)
(global-set-key (kbd "<f6>") 'elfeed)
(global-set-key (kbd "<f7>") 'org-capture)
(global-set-key (kbd "<f8>") 'org-agenda)
(global-set-key (kbd "<f9>") 'bongo)

(when (eq system-type 'darwin)
  (defvar *my-previous-buffer* t
    "can we switch?")

  (defun my-previous-buffer ()
    (interactive)
    (message "custom prev: *my-previous-buffer*=%s" *my-previous-buffer*)
    (when *my-previous-buffer*
      (previous-buffer)
      (setq *my-previous-buffer* nil)
      (run-at-time "1 sec" nil (lambda ()
                                 (setq *my-previous-buffer* t)))))

  (defvar *my-next-buffer* t
    "can we switch?")

  (defun my-next-buffer ()
    (interactive)
    (message "custom prev: *my-next-buffer*=%s" *my-next-buffer*)
    (when *my-next-buffer*
      (next-buffer)
      (setq *my-next-buffer* nil)
      (run-at-time "1 sec" nil (lambda ()
                                 (setq *my-next-buffer* t)))))

  (global-set-key [double-wheel-right] 'my-previous-buffer)
  (global-set-key [double-wheel-left] 'my-next-buffer))

