;;; editor.el -*- lexical-binding: t; -*-
;; Editor configuration

;; === MULTIPLE CURSORS ==========================
(when (boundp 'lira-module-editor-multiple-cursors)
  (use-package multiple-cursors
    :ensure t))

;; === SNIPPETS ==================================
(when (boundp 'lira-module-editor-snippets)
  (use-package yasnippet
    :ensure t
    :config
    (yas-global-mode 1)))

;; === FOLD ======================================
(when (boundp 'lira-module-editor-fold)
  (use-package hideshow
    :config
    (add-hook 'prog-mode-hook #'hs-minor-mode)))

;; === FILE TEMPLATES ============================
(when (boundp 'lira-module-editor-file-templates)
  (use-package autoinsert
    :config
    (auto-insert-mode 1)))

;; === WORD WRAP =================================
(when (boundp 'lira-module-editor-word-wrap)
  (global-visual-line-mode 1))

;; === PAREDIT ===================================
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode))

;; === MOVE TEXT =================================
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; === SIMPLECLIP ================================
(use-package simpleclip
  :ensure t
  :config
  (simpleclip-mode 1)) 