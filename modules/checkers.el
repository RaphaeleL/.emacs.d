;;; checkers.el -*- lexical-binding: t; -*-
;; Syntax checking configuration

;; === SYNTAX ====================================
(when (boundp 'lira-module-checkers-syntax)
  (use-package flycheck
    :ensure t
    :config
    (global-flycheck-mode 1)))

;; === WHITESPACE ================================
(setq whitespace-style '(face trailing tabs newline tab-mark newline-mark))
(setq whitespace-display-mappings
      '((newline-mark 10 [172 10])
        (tab-mark 9 [187 9] [92 9]))) 