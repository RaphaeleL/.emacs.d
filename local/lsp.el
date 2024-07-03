(use-package python
  :bind (:map python-ts-mode-map
              ("<f5>" . recompile)
              ("<f6>" . eglot-format))
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . company-mode))
  :mode (("\\.py\\'" . python-ts-mode)))

(use-package c-mode
  :bind (:map python-ts-mode-map
              ("<f5>" . recompile)
              ("<f6>" . eglot-format))
  :hook ((c-ts-mode . eglot-ensure)
	 (c-ts-mode . company-mode))
  :mode (("\\.c\\'" . c-ts-mode)))

(use-package c++-mode
  :bind (:map c++-ts-mode-map
              ("<f5>" . recompile)
              ("<f6>" . eglot-format))
  :hook ((c++-ts-mode . eglot-ensure)
	 (c++-ts-mode . company-mode)
	 (c++-ts-mode . c++-mode))
  :mode (("\\.cpp\\'" . c++-ts-mode)))

(use-package go-mode
  :bind (:map go-ts-mode-map
              ("<f5>" . recompile)
              ("<f6>" . eglot-format))
  :hook ((go-ts-mode . eglot-ensure)
	 (go-ts-mode . company-mode)
	 (go-ts-mode . go-mode))
  :mode (("\\.go\\'" . go-ts-mode)))

;; TODO
;; - Rust

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.4
        company-minimum-prefix-length 1))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c d" . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c f" . flymake-show-buffer-diagnostics)
              ("C-c r" . eglot-rename)))

;; No Fringes
(set-fringe-mode 0)

(setq package-selected-packages '(lsp-mode yasnippet helm-lsp
    projectile hydra flycheck company avy helm-xref))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'go-mode-hook 'lsp)

(setq lsp-headerline-breadcrumb-enable nil)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
