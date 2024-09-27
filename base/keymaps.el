(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "N") 'dired-create-empty-file))

(global-set-key (kbd "M-w") 'mark-word)
(global-set-key (kbd "M-a") 'mark-page)
(global-set-key (kbd "M-F") 'mark-defun)
(global-set-key (kbd "M-s") 'mark-paragraph)

(global-set-key (kbd "C-c C-a") 'beginning-of-defun)
(global-set-key (kbd "C-c C-e") 'end-of-defun)

(global-set-key (kbd "C-c C-n") 'duplicate-line)
(global-set-key (kbd "C-c C-d") 'delete-current-line)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "M-z") 'undo)

(global-set-key (kbd "C-,") 'find-file)
(global-set-key (kbd "C-.") 'dired)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c n") 'duplicate-line)
(global-set-key (kbd "C-c d") 'delete-current-line)
(global-set-key (kbd "C-c a") 'beginning-of-defun)
(global-set-key (kbd "C-c e") 'end-of-defun)

(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "C-c s") 'shell-command)

(rc/require 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m") 'mc/mark-more-like-this-extended)

(global-set-key (kbd "M-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (text-scale-decrease 1)))

(global-set-key (kbd "C-x .") 'dired)
(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-log-all)

(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "C-c i") 'ibuffer)
(global-set-key (kbd "C-c k") (lambda () (interactive) (kill-current-buffer)))

(rc/require 'simpleclip)
(simpleclip-mode 1)
(global-set-key (kbd "C-c c") 'simpleclip-copy)
(global-set-key (kbd "C-c v") 'simpleclip-paste)

(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(rc/require 'which-key)
(use-package which-key
    :ensure nil
    :hook (after-init . which-key-mode)
    :config
    (setq which-key-separator "  ")
    (setq which-key-prefix-prefix "... ")
    (setq which-key-max-display-columns 3)
    (setq which-key-idle-delay 0.125)
    (setq which-key-idle-secondary-delay 0.25)
    (setq which-key-add-column-padding 1)
    (setq which-key-max-description-length 40))
(which-key-mode t)

