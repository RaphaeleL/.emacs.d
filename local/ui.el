(load "~/.emacs.d/local/rc.el")

;; Theme
(rc/require-theme 'gruber-darker)
(set-background-color "#1e1e1e")

;; No Startup Message
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Cleanup the UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Theme
(load-theme 'gruber-darker 1)

;; Mode Line
(mood-line-mode 1)

;; Font
(add-to-list 'default-frame-alist `(font . ,(get-default-font)))

;; Ido Mode for Files
(ido-mode 1)
(ido-everywhere 1)

;; Package Manager
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Ido Mode for M-x
(smex-initialize)

;; Line Numbering
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Bigger Font
(set-face-attribute 'default nil :height 130)

;; Window Size
(when window-system (set-frame-size (selected-frame) 120 30))

;; Disable Backup and Autosave Settings
(setq make-backup-files nil)
(setq auto-save-default nil)

