; ==================================================
; ===== HOOKS ======================================
; ==================================================

;; LSP
(when (require 'eglot nil 'noerror)
  (when (executable-find "pylsp") (add-hook 'python-mode-hook 'eglot-ensure))
  (when (executable-find "clangd") (add-hook 'c-mode-hook 'eglot-ensure))
  (when (executable-find "clangd") (add-hook 'c++-mode-hook 'eglot-ensure))
  (when (executable-find "rust-analyzer") (add-hook 'rust-mode-hook 'eglot-ensure))
  (when (executable-find "gopls") (add-hook 'go-mode-hook 'eglot-ensure)))

;; Text Completion
(add-hook 'after-init-hook         'global-company-mode)

;; Auto Close Par.
(add-hook 'text-mode-hook          'rc/turn-on-paredit)

;; On Save Behaviour
(add-hook 'c-mode-hook             'rc/on_save)
(add-hook 'c++-mode-hook           'rc/on_save)
(add-hook 'simpc-mode-hook         'rc/on_save)
(add-hook 'python-mode-hook        'rc/on_save)
(add-hook 'markdown-mode-hook      'rc/on_save)
(add-hook 'text-mode-hook          'rc/on_save)
(add-hook 'emacs-lisp-mode-hook    'rc/on_save)
(add-hook 'jenkinsfile-mode-hook   'rc/on_save)
(add-hook 'dockerfile-mode-hook    'rc/on_save)
(add-hook 'makefile-mode-hook      'rc/on_save)
(add-hook 'rpm-spec-mode-hook      'rc/on_save)

;; Better Moving in Different Kind of Buffers
(add-hook 'minibuffer-setup-hook   'rc/my-compile-minibuffer-setup)
(add-hook 'minibuffer-setup-hook   'rc/my-fido-minibuffer-setup)
