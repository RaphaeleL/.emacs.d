; ==================================================
; ====== PACKAGE SETTINGS ==========================
; ==================================================

(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
(setq dired-listing-switches "-laGh1Dv --group-directories-first")
(setq ls-lisp-ignore-case t)

(which-key-mode t)
(setq which-key-separator "  ")
(setq which-key-prefix-prefix "... ")
(setq which-key-max-display-columns 3)
(setq which-key-idle-delay 0.125)
(setq which-key-idle-secondary-delay 0.25)
(setq which-key-add-column-padding 1)
(setq which-key-max-description-length 40)

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

;(setq compilation-scroll-output t)

(use-package display-line-numbers
  :ensure nil
  :bind
  ("<f7>" . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  (setq-default display-line-numbers-widen t)
  :init
  (global-display-line-numbers-mode 1))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

