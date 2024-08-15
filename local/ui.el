(load "~/.emacs.d/local/rc.el")

;; Theme
(load-theme 'modus-operandi-tinted)

;; Mode Line
(mood-line-mode 1)
(spacious-padding-mode 1)

;; No Fringes
(set-fringe-mode 0)

;; No Startup Message
(setq inhibit-startup-message t)
;; (setq initial-scratch-message "")

;; Cleanup the UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Font
(add-to-list 'default-frame-alist `(font . ,(get-default-font)))

;; Package Manager
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Bigger Font
(set-face-attribute 'default nil :height 130)

;; Window Size
(when window-system (set-frame-size (selected-frame) 120 30))

;; Disable Backup and Autosave Settings
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Whitespace
(use-package whitespace
  :ensure nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . delete-trailing-whitespace))
  :config
  (setq whitespace-style
        '(face
          tabs
          spaces
          tab-mark
          space-mark
          trailing
          missing-newline-at-eof
          space-after-tab::tab
          space-after-tab::space
          space-before-tab::tab
          space-before-tab::space)))

;; Line numbers on the side of the window
(use-package display-line-numbers
  :ensure nil
  :bind
  ("<f7>" . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  (setq-default display-line-numbers-widen t))
