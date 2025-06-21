;;; emacs.el -*- lexical-binding: t; -*-
;; Core Emacs configuration

;; === DIRED =====================================
(when (boundp 'lira-module-emacs-dired)
  (setq dired-listing-switches "-la")
  (setq dired-dwim-target t))

;; === ELECTRIC ==================================
(when (boundp 'lira-module-emacs-electric)
  (electric-indent-mode 1))

;; === UNDO ======================================
(when (boundp 'lira-module-emacs-undo)
  (use-package undo-tree
    :ensure t
    :config
    (global-undo-tree-mode 1)
    ;; Disable .~undo-tree~ files
    (setq undo-tree-auto-save-history nil)))

;; === VC ========================================
(when (boundp 'lira-module-emacs-vc)
  (setq vc-follow-symlinks t))

;; === BASIC SETTINGS ============================
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 80)

;; === Y/N PROMPTS ===============================
(fset 'yes-or-no-p 'y-or-n-p)

;; === GARBAGE COLLECTION ========================
(setq gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-percentage 0.1)

;; === BACKUP SETTINGS ===========================
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t) 