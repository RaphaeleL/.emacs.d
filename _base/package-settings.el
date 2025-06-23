; ==================================================
; ====== PACKAGE SETTINGS ==========================
; ==================================================

(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
(setq dired-listing-switches "-laGh1Dv --group-directories-first")
(setq ls-lisp-ignore-case t)

(setq which-key-separator "  ")
(setq which-key-prefix-prefix "... ")
(setq which-key-max-display-columns 3)
(setq which-key-idle-delay 0.125)
(setq which-key-idle-secondary-delay 0.25)
(setq which-key-add-column-padding 1)
(setq which-key-max-description-length 40)

(use-package whitespace
  :ensure nil
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

;(setq compilation-scroll-output t)

(use-package display-line-numbers
  :ensure nil
  :config
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  (setq-default display-line-numbers-widen t)
  :init
  (global-display-line-numbers-mode 1))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(setq prefix-help-command #'embark-prefix-help-command)

(global-corfu-mode 1)
(setq corfu-cycle t)               ;; Cycle through candidates
(setq corfu-auto t)                ;; Enable auto popup
(setq corfu-auto-prefix 2)         ;; Minimum chars before popup
(setq corfu-quit-no-match t)       ;; Don't hang if no match
(setq corfu-preview-current nil)   ;; No inline preview

(setq completion-styles '(orderless basic))
(setq completion-category-overrides
      '((file (styles basic partial-completion)))) ;; better file completion

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(savehist-mode t)
(recentf-mode t)
; (smooth-scrolling-mode t)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Coding" (or
                    (mode . python-mode)
                    (mode . go-mode)
                    (mode . rust-mode)
                    (mode . web-mode)
                    (mode . yaml-mode)
                    (mode . jinja2-mode)
                    (mode . dockerfile-mode)
                    (mode . markdown-mode)
                    (mode . jenkinsfile-mode)))
         ("Dired" (mode . dired-mode))
         ("Magit" (or
				    (name . "Magit")
					(name . "Magit\\*$")))
         ("Emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Help\\*$")
                   (name . "^\\*Compile-Log\\*$")))
         ("Others" (name . ".*")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
