; ==================================================
; ====== MINIBUFFER ================================
; ==================================================

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(vertico-mode 1)
(marginalia-mode 1)
; (counsel-mode 1)
;; (fido-mode 1)
