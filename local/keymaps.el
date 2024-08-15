;; Helper Function
(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

;; Simplify yes/no Prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; Selection
(global-set-key (kbd "M-w") 'mark-word)
(global-set-key (kbd "M-a") 'mark-page)
(global-set-key (kbd "M-F") 'mark-defun)
(global-set-key (kbd "M-s") 'mark-paragraph)

;; Jumping
(global-set-key (kbd "C-c C-a") 'beginning-of-defun)
(global-set-key (kbd "C-c C-e") 'end-of-defun)

;; Misc
(global-set-key (kbd "C-c C-n") 'duplicate-line)
(global-set-key (kbd "C-c C-d") 'delete-current-line)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "M-z") 'undo)

;; Shorter Versions of given Keymaps
(global-set-key (kbd "C-.") 'find-file)
(global-set-key (kbd "C-d") 'dired)
(global-set-key (kbd "C-g") 'magit)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c n") 'duplicate-line)
(global-set-key (kbd "C-c d") 'delete-current-line)
(global-set-key (kbd "C-c a") 'beginning-of-defun)
(global-set-key (kbd "C-c e") 'end-of-defun)

;; Shell
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "C-c s") 'shell-command)

;; Multi Cursor
(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m") 'mc/mark-more-like-this-extended)

;; Font Size
(global-set-key (kbd "M-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (text-scale-decrease 1)))

;; Dired
(global-set-key (kbd "C-x .") 'dired)
(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-x C-d") 'dired)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-log-all)

;; Buffer 
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "C-c i") 'ibuffer)
(global-set-key (kbd "C-c l") 'counsel-switch-buffer)
(global-set-key (kbd "C-c k") (lambda () (interactive) (kill-current-buffer)))

;; Copy and Paste
(simpleclip-mode 1)
(global-set-key (kbd "C-c c") 'simpleclip-copy)
(global-set-key (kbd "C-c v") 'simpleclip-paste)

;; Move Text
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
