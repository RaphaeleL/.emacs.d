;;; tools.el -*- lexical-binding: t; -*-
;; Tools configuration - Lazy loaded for better startup performance

;; === MAGIT =====================================
(when (boundp 'lira-module-tools-magit)
  (use-package magit
    :ensure t
    :defer t
    :commands (magit-status magit-log magit-diff)))

;; === LSP =======================================
(when (boundp 'lira-module-tools-lsp)
  (use-package eglot
    :ensure t
    :defer t
    :commands (eglot eglot-ensure)
    :config
    (add-to-list 'exec-path "~/.local/bin")
    ;; Only load LSP when needed
    (add-hook 'prog-mode-hook #'eglot-ensure)))

;; === EVAL ======================================
(when (boundp 'lira-module-tools-eval)
  (use-package eval-in-repl
    :ensure t
    :defer t))

;; === LOOKUP ====================================
(when (boundp 'lira-module-tools-lookup)
  (use-package helpful
    :ensure t
    :defer t
    :commands (helpful-callable helpful-variable helpful-key helpful-command helpful-at-point)
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
  :defer t
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; === IBUFFER ===================================
(setq ibuffer-saved-filter-groups
      '(("default"
         ("dev" (or (mode . c-mode) (mode . c++-mode) (mode . python-mode) (mode . go-mode) (mode . rust-mode)))
         ("dired" (mode . dired-mode))
         ("web" (or (mode . html-mode) (mode . css-mode)))
         ("emacs" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$"))))))

;; Lazy load ibuffer configuration
(with-eval-after-load 'ibuffer
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))) 