(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

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

(defun lr/create-keymap     (key action) (global-set-key (kbd key) action))
(defun lr/create-keymap-cc  (key action) (global-set-key (kbd (concat "C-c "   key)) action))
(defun lr/create-keymap-ccc (key action) (global-set-key (kbd (concat "C-c C-" key)) action))
(defun lr/create-keymap-cx  (key action) (global-set-key (kbd (concat "C-x "   key)) action))
(defun lr/create-keymap-m   (key action) (global-set-key (kbd (concat "M-"     key)) action))
(defun lr/create-keymap-c   (key action) (global-set-key (kbd (concat "C-"     key)) action))

(lr/create-keymap-m   "w"     'mark-word)
(lr/create-keymap-m   "a"     'mark-page)
(lr/create-keymap-m   "F"     'mark-defun)
(lr/create-keymap-m   "s"     'mark-paragraph)
(lr/create-keymap-c   ","     'find-file)
(lr/create-keymap-c   "."     'dired)
(lr/create-keymap-m   ","     'project-find-file)
(lr/create-keymap-m   "g"     'magit-status)
(lr/create-keymap-m   "c"     'compile)
(lr/create-keymap-c   "l"     'shell-command)
(lr/create-keymap-m   "q"     'kill-compilation)
(lr/create-keymap-m   "i"     'ibuffer)
(lr/create-keymap-m   "p"     'move-text-up)
(lr/create-keymap-m   "n"     'move-text-down)
(lr/create-keymap-c   "o"     'other-window)
