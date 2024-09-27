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
