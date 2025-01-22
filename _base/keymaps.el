; ==================================================
; ===== KEYMAPS ====================================
; ==================================================

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "N") 'dired-create-empty-file))

(global-set-key (kbd "M-w") 'mark-word)
(global-set-key (kbd "M-a") 'mark-page)
(global-set-key (kbd "M-F") 'mark-defun)
(global-set-key (kbd "M-s") 'mark-paragraph)

(global-set-key (kbd "M-j") 'join-line)

(global-set-key (kbd "M-z") 'undo)

(global-set-key (kbd "C-,") 'find-file)
(global-set-key (kbd "C-.") 'dired)
(global-set-key (kbd "C-c C-,") 'project-find-file)

(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c s") 'shell-command)

(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-log)

(global-set-key (kbd "M-i") 'ibuffer)
(global-set-key (kbd "M-l") 'switch-to-buffer)
(global-set-key (kbd "C-c i") 'ibuffer)
(global-set-key (kbd "C-c l") 'switch-to-buffer)

(global-set-key (kbd "C-w") 'rc/copy)
(global-set-key (kbd "C-y") 'rc/paste)
(global-set-key (kbd "C-t") 'rc/cut)

(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m") 'mc/mark-more-like-this-extended)

(global-set-key (kbd "M-d") 'rc/duplicate-line)
(global-set-key (kbd "M-r") 'rc/delete-line)

(global-set-key (kbd "<f3>") 'rc/toggle-themes)
