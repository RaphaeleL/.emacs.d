; =================================================
; ===== MINIMAL EMACS CONFIGURATION ===============
; =================================================

; This Configs aims to provide a minimal, fast and 
; yet robust emacs config. It doesn't use any
; package manager, the only external package is 
; simpleclip to improve copy/paste/cut behaviour
; on the different operation systems. In addition
; the config should work seamlessly and fast on any
; operation system. For a fully fledged, configured 
; and ide like emacs, check out init.el.

; to hold both configs (init.el and init.term.el) 
; considure to alias one of them, like following:
; alias em="emacs -q -l ~/.emacs.d/init.term.el"

; === BASIC SETTINGS ============================

; (scroll-bar-mode -1)
(tool-bar-mode -1)
; (tooltip-mode -1)
; (menu-bar-mode -1)

(global-display-line-numbers-mode 1)

(setq display-line-numbers-major-tick 0)
(setq display-line-numbers-minor-tick 0)
(setq-default display-line-numbers-widen t)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

(set-fringe-mode 0)

(setq dired-dwim-target t)

(setq use-dialog-box nil)

(setq inhibit-startup-message t)
; (setq initial-scratch-message nil)
; (setq initial-major-mode 'text-mode)

(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.01)
(setq mouse-yank-at-point t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(delete-selection-mode 1)

(setq line-move-visual nil)

(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

; === HELPER ====================================

; --- BETTER COPY HELPER FUNCTIONS ---
(defun lr/cut ()
  (interactive)
  (simpleclip-cut (region-beginning) (region-end))
  (deactivate-mark))

(defun lr/copy ()
  (interactive)
  (simpleclip-copy (region-beginning) (region-end))
  (deactivate-mark))

(defun lr/paste ()
  (interactive)
  (simpleclip-paste)
  (deactivate-mark))

(load "~/.emacs.d/_term/sclip.el")     ; --- COPY TO CLIPBOARD
(load "~/.emacs.d/_base/ssh.el")       ; --- FCO SSH
(load "~/.emacs.d/_base/garbage.el")   ; --- PERFORMANCE

; === KEYMAPS ===================================
(global-set-key (kbd (concat "C-y")) 'lr/paste)
(global-set-key (kbd (concat "C-w")) 'lr/copy)
(global-set-key (kbd (concat "C-t")) 'lr/cut)
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
(global-set-key (kbd (concat "M-z")) 'undo)
(global-set-key (kbd (concat "M-p")) 'move-text-up)
(global-set-key (kbd (concat "M-n")) 'move-text-down)
(global-set-key (kbd (concat "C-o")) 'other-window)

