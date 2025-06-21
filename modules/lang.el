;;; lang.el -*- lexical-binding: t; -*-
;; Language-specific configurations - Lazy loaded for better performance

;; === EMACS LISP ================================
(when (boundp 'lira-module-lang-emacs-lisp)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

;; === MARKDOWN ==================================
(when (boundp 'lira-module-lang-markdown)
  (use-package markdown-mode
    :ensure t
    :defer t
    :mode ("\\.md\\'" . markdown-mode)
    :init
    (autoload 'markdown-mode "markdown-mode" nil t)))

;; === WEB =======================================
(when (boundp 'lira-module-lang-web)
  (use-package web-mode
    :ensure t
    :defer t
    :mode ("\\.html\\'" "\\.css\\'" "\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'")
    :init
    (autoload 'web-mode "web-mode" nil t)
    :config
    ;; Only load web-mode config when actually used
    (with-eval-after-load 'web-mode
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2))))

;; === YAML ======================================
(when (boundp 'lira-module-lang-yaml)
  (use-package yaml-mode
    :ensure t
    :defer t
    :mode ("\\.yml\\'" "\\.yaml\\'")
    :init
    (autoload 'yaml-mode "yaml-mode" nil t)))

;; === JSON ======================================
(when (boundp 'lira-module-lang-json)
  (use-package json-mode
    :ensure t
    :defer t
    :mode ("\\.json\\'")
    :init
    (autoload 'json-mode "json-mode" nil t)))

;; === GO ========================================
(when (boundp 'lira-module-lang-go)
  (use-package go-mode
    :ensure t
    :defer t
    :mode ("\\.go\\'")
    :init
    (autoload 'go-mode "go-mode" nil t)
    :config
    ;; Only load go-mode config when actually used
    (with-eval-after-load 'go-mode
      (add-hook 'go-mode-hook #'eglot-ensure))))

;; === RUST ======================================
(when (boundp 'lira-module-lang-rust)
  (use-package rust-mode
    :ensure t
    :defer t
    :mode ("\\.rs\\'")
    :init
    (autoload 'rust-mode "rust-mode" nil t)
    :config
    ;; Only load rust-mode config when actually used
    (with-eval-after-load 'rust-mode
      (add-hook 'rust-mode-hook #'eglot-ensure))))

;; === PYTHON ====================================
(when (boundp 'lira-module-lang-python)
  (use-package python-mode
    :ensure t
    :defer t
    :mode ("\\.py\\'" . python-mode)
    :init
    (autoload 'python-mode "python-mode" nil t)
    :config
    ;; Only load python-mode config when actually used
    (with-eval-after-load 'python-mode
      (add-hook 'python-mode-hook #'eglot-ensure))))

;; === C =========================================
(when (boundp 'lira-module-lang-c)
  (add-hook 'c-mode-hook #'eglot-ensure))

;; === C++ =======================================
(when (boundp 'lira-module-lang-cpp)
  (add-hook 'c++-mode-hook #'eglot-ensure))

;; === SH ========================================
(when (boundp 'lira-module-lang-sh)
  (add-hook 'sh-mode-hook #'eglot-ensure))

;; === DOCKERFILE ================================
(when (boundp 'lira-module-lang-dockerfile)
  (use-package dockerfile-mode
    :ensure t
    :defer t
    :mode ("Dockerfile\\'")
    :init
    (autoload 'dockerfile-mode "dockerfile-mode" nil t)))

;; === JENKINSFILE ===============================
(when (boundp 'lira-module-lang-jenkinsfile)
  (use-package jenkinsfile-mode
    :ensure t
    :defer t
    :mode ("Jenkinsfile\\'")
    :init
    (autoload 'jenkinsfile-mode "jenkinsfile-mode" nil t)))

;; === JINJA2 ====================================
(when (boundp 'lira-module-lang-jinja2)
  (use-package jinja2-mode
    :ensure t
    :defer t
    :mode ("\\.j2\\'" "\\.jinja\\'")
    :init
    (autoload 'jinja2-mode "jinja2-mode" nil t))) 