;;; completion.el -*- lexical-binding: t; -*-
;; Completion configuration

;; === COMPANY ===================================
(when (boundp 'lira-module-completion-company)
  (use-package company
    :ensure t
    :config
    (global-company-mode 1)
    ;; Enhanced backends that work with LSP
    (setq company-backends '((company-capf 
                              company-dabbrev-code 
                              company-dabbrev
                              company-files
                              company-keywords)))
    ;; Better completion experience
    (setq company-idle-delay 0.1)
    (setq company-minimum-prefix-length 2)
    (setq company-selection-wrap-around t)
    ;; Show completion immediately
    (setq company-show-numbers t)
    (setq company-tooltip-align-annotations t)))

;; === VERTICO ===================================
(when (boundp 'lira-module-completion-vertico)
  (use-package vertico
    :ensure t
    :config
    (vertico-mode 1)))

;; === ORDERLESS =================================
(when (boundp 'lira-module-completion-orderless)
  (use-package orderless
    :ensure t
    :config
    (setq completion-styles '(orderless basic))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles basic partial-completion))))))

;; === MARGINALIA ================================
(when (boundp 'lira-module-completion-marginalia)
  (use-package marginalia
    :ensure t
    :bind (:map minibuffer-local-map
            ("M-A" . marginalia-cycle))
    :config
    (marginalia-mode 1)))

;; === FIDO ======================================
(when (boundp 'lira-module-completion-fido)
  ;; fido-mode is built into Emacs 28+, no need to install
  (fido-mode 1))

;; === LSP INTEGRATION ===========================
;; Ensure Company works with LSP when both are enabled
(when (and (boundp 'lira-module-completion-company) 
           (boundp 'lira-module-tools-lsp))
  ;; Add LSP completion to company backends
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local company-backends
                          '(company-capf
                            company-dabbrev-code
                            company-dabbrev
                            company-files
                            company-keywords))))
  ;; Enable completion at point for LSP
  (setq eglot-completion-func #'company-complete))

;; === MINIBUFFER SETUP ==========================
(defun lira-fido-minibuffer-setup ()
  (setq-local completion-styles '(basic substring))
  ;; Add C-n and C-p keybindings for navigation in fido-mode
  (when (or fido-mode fido-vertical-mode)
    (local-set-key (kbd "C-s") 'next-history-element)
    (local-set-key (kbd "C-r") 'previous-history-element)
    (local-set-key (kbd "C-n") 'icomplete-forward-completions)
    (local-set-key (kbd "C-p") 'icomplete-backward-completions)))

(add-hook 'minibuffer-setup-hook #'lira-fido-minibuffer-setup)

;; === COMPLETION UTILITY FUNCTIONS ===============
(defun lira-toggle-mini-buffer-mode ()
  (interactive)
  (condition-case err
      (if (bound-and-true-p fido-mode)
          (progn
            (fido-mode -1)
            (vertico-mode 1)
            (marginalia-mode 1)
            (message "Switched to vertico-mode"))
        (progn
          (vertico-mode -1)
          (marginalia-mode -1)
          (fido-mode 1)
          (message "Switched to fido-mode")))
    (error
     (message "Warning: Completion modes not available")))) 