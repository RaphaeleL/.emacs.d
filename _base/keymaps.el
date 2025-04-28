; ==================================================
; ===== KEYMAPS ====================================
; ==================================================

;; Selecting Stuff
(global-set-key (kbd "M-w")     'mark-word)
(global-set-key (kbd "M-a")     'mark-page)
(global-set-key (kbd "M-F")     'mark-defun)
(global-set-key (kbd "M-s")     'mark-paragraph)

;; No Description needed
(global-set-key (kbd "M-j")     'join-line)
(global-set-key (kbd "M-z")     'undo)
(global-set-key (kbd "M-d")     'rc/duplicate-line)
(global-set-key (kbd "M-r")     'rc/delete-line)

;; Default Emacs stuff, just simpler
(global-set-key (kbd "C-,")     'find-file)
(global-set-key (kbd "C-.")     'dired)
(global-set-key (kbd "C-c C-,") 'project-find-file)
(global-set-key (kbd "C-c g")   'magit-status)

;; Shell Commands
(global-set-key (kbd "C-c m")   'compile)
(global-set-key (kbd "C-c s")   'shell-command)

;; Buffer Management
(global-set-key (kbd "M-i")     'ibuffer)
(global-set-key (kbd "M-l")     'switch-to-buffer)
(global-set-key (kbd "C-c i")   'ibuffer)
(global-set-key (kbd "C-c l")   'switch-to-buffer)

;; Copy and Paste
(global-set-key (kbd "C-w")     'rc/copy)
(global-set-key (kbd "C-y")     'rc/paste)
(global-set-key (kbd "C-t")     'rc/cut)

;; Moving a Selection up and down
(global-set-key (kbd "M-p")     'move-text-up)
(global-set-key (kbd "M-n")     'move-text-down)

;; Multi line Editing
(global-set-key (kbd "M-/")     'mc/mark-next-like-this)
(global-set-key (kbd "C-j")     'mc/mark-next-like-this)
(global-set-key (kbd "C-k")     'mc/mark-previous-like-this)
(global-set-key (kbd "M-m")     'mc/mark-more-like-this-extended)

;; Stuff i do not need daily, but i definitely want a shortcut for
(global-set-key (kbd "<f1>")    (lambda () (interactive) (rc/toggle-buffer "*scratch*")))
(global-set-key (kbd "<f2>")    (lambda () (interactive) (rc/toggle-buffer "*compilation*")))
(global-set-key (kbd "<f3>")    'rc/toggle-themes)
(global-set-key (kbd "<f4>")    (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Zooming
(global-set-key (kbd "C-=")     (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-+")     (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--")     (lambda () (interactive) (text-scale-decrease 1)))
