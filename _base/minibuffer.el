; ==================================================
; ====== MINIBUFFER ================================
; ==================================================

; (fido-mode 1)

(use-package vertico
    :ensure t
    :config
        (vertico-mode))
(vertico-mode 1)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
    :bind (:map minibuffer-local-map
            ("M-A" . marginalia-cycle))
    :init
        (marginalia-mode))
(marginalia-mode 1)

(use-package counsel
    :init
        (counsel-mode))
(counsel-mode 1)
