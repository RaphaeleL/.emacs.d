(unless (package-installed-p 'eglot)
  (package-install 'eglot))

(rc/require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c C-d") 'eldoc-doc-buffer))

;; TODO: Remove inline hints in methods params
;; add(a: 5, b: 2) --> add(5, 2)

(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(rc/require 'markdown-mode)

(rc/require 'web-mode)
(rc/require 'flycheck)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))

