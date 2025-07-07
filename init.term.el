(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(global-display-line-numbers-mode 1)

(setq display-line-numbers-major-tick 0)
(setq display-line-numbers-minor-tick 0)
(setq-default display-line-numbers-widen t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(delete-selection-mode 1)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

(set-fringe-mode 0)

(setq use-dialog-box nil)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.01)
(setq mouse-yank-at-point t)

(load "~/.emacs.d/_base/garbage.el")
(load "~/.emacs.d/_base/ssh.el")
(load "~/.emacs.d/_base/garbage.el")

(load "~/.emacs.d/_term/xclip.el")

;(global-set-key (kbd (concat "C-y")) 'lr/paste)
;(global-set-key (kbd (concat "C-w")) 'lr/copy)
(global-set-key (kbd (concat "M-w")) 'mark-word)
(global-set-key (kbd (concat "M-a")) 'mark-page)
(global-set-key (kbd (concat "M-F")) 'mark-defun)
(global-set-key (kbd (concat "M-s")) 'mark-paragraph)
(global-set-key (kbd (concat "C-,")) 'find-file)
(global-set-key (kbd (concat "C-.")) 'dired)
(global-set-key (kbd (concat "M-,")) 'project-find-file)
(global-set-key (kbd (concat "M-g")) 'magit-status)
(global-set-key (kbd (concat "M-c")) 'compile)
(global-set-key (kbd (concat "C-l")) 'shell-command)
(global-set-key (kbd (concat "M-q")) 'kill-compilation)
(global-set-key (kbd (concat "M-i")) 'ibuffer)
(global-set-key (kbd (concat "M-p")) 'move-text-up)
(global-set-key (kbd (concat "M-n")) 'move-text-down)
(global-set-key (kbd (concat "C-o")) 'other-window)

(add-hook 'after-init-hook (lambda () (advice-add 'yes-or-no-p :override #'y-or-n-p)))
