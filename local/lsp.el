;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;; Enable LSP support in Python and C modes
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

;; Enable completion using company mode
(unless (package-installed-p 'company)
  (package-install 'company))
(add-hook 'after-init-hook 'global-company-mode)

;; Set up company backend for LSP
(with-eval-after-load 'company
  (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

;; Optional: Configure key bindings for LSP features
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c C-d") 'eldoc-doc-buffer))

