;; ===============================================
;; === CUSTOM ADJUSTMENTS ========================
;; ===============================================

;; === TREESITTER ================================
;; (require 'treesit)
;; (setq treesit-language-source-alist
;;       '((python . ("https://github.com/tree-sitter/tree-sitter-python"))
;;         (yaml   . ("https://github.com/ikatyang/tree-sitter-yaml"))))
;; (dolist (lang '(python yaml))
;;   (unless (treesit-ready-p lang)
;;     (message "Installing tree-sitter grammar for %s..." lang)
;;     (treesit-install-language-grammar lang)))
;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)
;;         (yaml-mode   . yaml-ts-mode)))

;; === MACOS =====================================
(when (eq system-type 'darwin)
  (rc/disable-custom-font)
  (set-face-attribute 'default nil :height 160))

;; === WINDOWS =====================================
(when (eq system-type 'windows-nt)
  (setq default-directory "C:/Users/RaLiccia/"))

;; === GNU/LINUX ===================================
(when (eq system-type 'gnu/linux)
  (rc/disable-custom-font)
  (set-face-attribute 'default nil :height 160))
