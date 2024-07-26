;; LaTeX support
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Enable LaTeX math support
(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

;; Enable reference mangment
(add-hook 'LaTeX-mode-hook #'reftex-mode)

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
