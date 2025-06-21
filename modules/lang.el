;;; lang.el -*- lexical-binding: t; -*-
;; Language-specific configurations

;; === EMACS LISP ================================
(when (boundp 'lira-module-lang-emacs-lisp)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

;; === MARKDOWN ==================================
(when (boundp 'lira-module-lang-markdown)
  (use-package markdown-mode
    :ensure t
    :mode ("\\.md\\'" . markdown-mode)))

;; === WEB =======================================
(when (boundp 'lira-module-lang-web)
  (use-package web-mode
    :ensure t
    :mode ("\\.html\\'" "\\.css\\'" "\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'")))

;; === YAML ======================================
(when (boundp 'lira-module-lang-yaml)
  (use-package yaml-mode
    :ensure t
    :mode ("\\.yml\\'" "\\.yaml\\'")))

;; === JSON ======================================
(when (boundp 'lira-module-lang-json)
  (use-package json-mode
    :ensure t
    :mode ("\\.json\\'")))

;; === GO ========================================
(when (boundp 'lira-module-lang-go)
  (use-package go-mode
    :ensure t
    :mode ("\\.go\\'")
    :config
    (add-hook 'go-mode-hook #'eglot-ensure)))

;; === RUST ======================================
(when (boundp 'lira-module-lang-rust)
  (use-package rust-mode
    :ensure t
    :mode ("\\.rs\\'")
    :config
    (add-hook 'rust-mode-hook #'eglot-ensure)))

;; === PYTHON ====================================
(when (boundp 'lira-module-lang-python)
  (use-package python-mode
    :ensure t
    :mode ("\\.py\\'" . python-mode)
    :config
    (add-hook 'python-mode-hook #'eglot-ensure)))

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
    :mode ("Dockerfile\\'")))

;; === JENKINSFILE ===============================
(when (boundp 'lira-module-lang-jenkinsfile)
  (use-package jenkinsfile-mode
    :ensure t
    :mode ("Jenkinsfile\\'")))

;; === JINJA2 ====================================
(when (boundp 'lira-module-lang-jinja2)
  (use-package jinja2-mode
    :ensure t
    :mode ("\\.j2\\'" "\\.jinja\\'"))) 