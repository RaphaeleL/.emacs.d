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

;; TODO
;; - Rust
;; - Go

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

