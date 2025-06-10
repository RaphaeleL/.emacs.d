; ==================================================
; ====== MINIBUFFER ================================
; ==================================================


(use-package vertico
    :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
    :bind (:map minibuffer-local-map
            ("M-A" . marginalia-cycle)))

;; (use-package counsel
;;     :init
;;         (counsel-mode))
;; (counsel-mode 1)


;; (vertico-mode 1)
;; (marginalia-mode 1)
(fido-mode 1)
