; ==================================================
; ===== LSP ========================================
; ==================================================

(add-to-list 'exec-path "~/.local/bin")
(with-eval-after-load 'company
  (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

;; LSP
(when (require 'eglot nil 'noerror)
  (when (executable-find "pylsp") (add-hook 'python-mode-hook 'eglot-ensure))
  (when (executable-find "clangd") (add-hook 'c-mode-hook 'eglot-ensure))
  (when (executable-find "clangd") (add-hook 'c++-mode-hook 'eglot-ensure))
  (when (executable-find "rust-analyzer") (add-hook 'rust-mode-hook 'eglot-ensure))
  (when (executable-find "gopls") (add-hook 'go-mode-hook 'eglot-ensure)))