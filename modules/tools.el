;;; tools.el -*- lexical-binding: t; -*-
;; Tools configuration

;; === MAGIT =====================================
(when (boundp 'lira-module-tools-magit)
  (use-package magit
    :ensure t))

;; === LSP =======================================
(when (boundp 'lira-module-tools-lsp)
  (use-package eglot
    :ensure t
    :config
    (add-to-list 'exec-path "~/.local/bin")))

;; === EVAL ======================================
(when (boundp 'lira-module-tools-eval)
  (use-package eval-in-repl
    :ensure t))

;; === LOOKUP ====================================
(when (boundp 'lira-module-tools-lookup)
  (use-package helpful
    :ensure t
    :bind
    (("C-h f" . helpful-callable)
     ("C-h v" . helpful-variable)
     ("C-h k" . helpful-key)
     ("C-h x" . helpful-command)
     ("C-c C-d" . helpful-at-point))))

;; === COMPILATION ===============================
(setq compilation-scroll-output t)

;; === ANSI COLOR ================================
(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; === IBUFFER ===================================
(setq ibuffer-saved-filter-groups
      '(("default"
         ("dev" (or (mode . c-mode) (mode . c++-mode) (mode . python-mode) (mode . go-mode) (mode . rust-mode)))
         ("dired" (mode . dired-mode))
         ("web" (or (mode . html-mode) (mode . css-mode)))
         ("emacs" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default"))) 