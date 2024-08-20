;; Completion
(use-package vertico
    :ensure t
    :config
        (vertico-mode)
)

;; Styling of the completion
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Command Description
(use-package marginalia
    :bind (:map minibuffer-local-map
            ("M-A" . marginalia-cycle))

    :init
        (marginalia-mode)
)

;; Preview in Switch Buffer
(use-package counsel
    :init
        (counsel-mode)
)
