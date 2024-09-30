;; ============================================================
;; ========= Base Config ======================================
;; ============================================================

(load "~/.emacs.d/_base/melpa.el")
(load "~/.emacs.d/_base/packages.el")
(load "~/.emacs.d/_base/functions.el")
(load "~/.emacs.d/_base/garbage.el")
(load "~/.emacs.d/_base/ui.el")
(load "~/.emacs.d/_base/base.el")
(load "~/.emacs.d/_base/mac.el")
(load "~/.emacs.d/_base/keymaps.el")
(load "~/.emacs.d/_base/lsp.el")
(load "~/.emacs.d/_base/custom.el")

;; ============================================================
;; ========= Custom Adjustments ===============================
;; ============================================================

(rc/require 'doom-themes)
(load-theme 'doom-one-light 1)

(rc/require 'mood-line)
(mood-line-mode 1)

(rc/require 'spacious-padding)
(spacious-padding-mode 1)

(global-set-key (kbd "C-c l") 'counsel-switch-buffer)

(use-package whitespace
  :ensure nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . delete-trailing-whitespace))
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

(use-package display-line-numbers
  :ensure nil
  :bind
  ("<f7>" . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  (setq-default display-line-numbers-widen t))

(rc/require 'vertico)
(use-package vertico
    :ensure t
    :config
        (vertico-mode))
(vertico-mode 1)

(rc/require 'orderless)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(rc/require 'marginalia)
(use-package marginalia
    :bind (:map minibuffer-local-map
            ("M-A" . marginalia-cycle))
    :init
        (marginalia-mode))
(marginalia-mode 1)

(rc/require 'counsel)
(use-package counsel
    :init
        (counsel-mode))
(counsel-mode 1)
