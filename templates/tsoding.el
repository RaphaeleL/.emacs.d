;; ============================================================
;; ========= Base Config ======================================
;; ============================================================

(load "~/.emacs.d/base/melpa.el")
(load "~/.emacs.d/base/packages.el")
(load "~/.emacs.d/base/functions.el")
(load "~/.emacs.d/base/garbage.el")
(load "~/.emacs.d/base/ui.el")
(load "~/.emacs.d/base/base.el")
(load "~/.emacs.d/base/keymaps.el")
(load "~/.emacs.d/base/lsp.el")
(load "~/.emacs.d/base/custom.el")

;; ============================================================
;; ========= Custom Adjustments ===============================
;; ============================================================

(rc/require 'gruber-darker-theme)
(load-theme 'gruber-darker 1)

(global-set-key (kbd "C-c l") 'switch-to-buffer)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(rc/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style
        '(face
          tabs
          spaces
          tab-mark
          space-mark
          trailing
          missing-newline-at-eof
          space-after-tab::tab
          space-after-tab::space
          space-before-tab::tab
          space-before-tab::space)))

(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'simpc-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
