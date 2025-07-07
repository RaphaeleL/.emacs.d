; ==================================================
; ===== PACKAGES ===================================
; ==================================================

; === Essential Editing & Programming Tools ===

(lr/require 'simpleclip)        ;; Integrates Emacs kill-ring with system clipboard
(lr/require 'move-text)         ;; Move lines or regions up/down with ease
(lr/require 'multiple-cursors)  ;; Edit multiple lines or places at once (like in Sublime)
(lr/require 'magit)             ;; Full-featured Git interface inside Emacs
(lr/require 'eglot)             ;; Lightweight LSP client for IDE-like code features
(lr/require 'paredit)           ;; Structured editing of Lisp code (maintains parentheses)
(lr/require 'dired-x)           ;; Extended Dired features (e.g. copy, paste, etc.)

; === Optional UI Enhancements ===

(lr/require 'mood-line)         ;; Minimal and pretty modeline replacement
(lr/require 'ansi-color)        ;; Display ANSI colors in compilation or shell buffers
(lr/require 'which-key)         ;; Show possible keybindings after a prefix is typed
; (lr/require 'smooth-scrolling)  ;; Make Emacs scroll smoothly

; === Completion Frameworks (minibuffer/UI) ===

(lr/require 'vertico)           ;; Vertical minibuffer completion UI (replaces default M-x and more)
(lr/require 'orderless)         ;; Completion style allowing out-of-order, fuzzy-like matching
(lr/require 'marginalia)        ;; Annotations next to minibuffer candidates (e.g. describe functions)
(lr/require 'corfu)             ;; Popup-style in-buffer completion UI (replaces company)
(lr/require 'consult)           ;; Powerful completion commands for files, buffers, search, etc.
; (lr/require 'company)           ;; In-buffer completion popup (mostly replaced by corfu if used)
; (lr/require 'counsel)           ;; Ivy-based commands (e.g. counsel-M-x), not needed if using Vertico/Consult

; === Language Modes (Syntax highlighting, indentation, etc.) ===

(lr/require 'markdown-mode)     ;; Major mode for editing Markdown files
(lr/require 'dockerfile-mode)   ;; Syntax highlighting for Dockerfiles
(lr/require 'jenkinsfile-mode)  ;; Major mode for Jenkins pipeline scripts
(lr/require 'web-mode)          ;; General mode for HTML, JS, templates, etc.
(lr/require 'yaml-mode)         ;; Major mode for YAML files
(lr/require 'jinja2-mode)       ;; Support for Jinja2 templates (used in Ansible, etc.)
(lr/require 'go-mode)           ;; Major mode for Go programming language
(lr/require 'rust-mode)         ;; Major mode for Rust programming language
(lr/require 'rpm-spec-mode)     ;; Support for RPM Spec Files

; === History / Recent Files ===

(lr/require 'savehist)          ;; Save minibuffer history across Emacs sessions
(lr/require 'recentf)           ;; Track recently opened files and offer easy reopening

; ==================================================
; ====== PACKAGE SETTINGS ==========================
; ==================================================

(with-eval-after-load 'dired
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-laGh1Dv --group-directories-first")
  (setq ls-lisp-ignore-case t)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (define-key dired-mode-map (kbd "M-o") #'dired-omit-mode)
  (define-key dired-mode-map (kbd "M-r") #'wdired-change-to-wdired-mode)
  (setq dired-omit-files "^\\.[^.].*"))

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

(add-to-list 'auto-mode-alist '("\\.log\\'" . syslog-mode))

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Coding" (or
					(mode . c-mode)
					(mode . c++-mode)
					(mode . java-mode)
					(mode . js-mode)
					(mode . typescript-mode)
					(mode . lua-mode)
					(mode . php-mode)
					(mode . terraform-mode)
					(mode . ansible-mode)
					(mode . nginx-mode)
					(mode . conf-mode)
					(mode . groovy-mode)
                    (mode . python-mode)
                    (mode . makefile-mode)
                    (mode . rpm-spec-mode)
					(mode . sh-mode)
                    (mode . rust-mode)
                    (mode . go-mode)
                    (mode . web-mode)
                    (mode . jinja2-mode)
                    (mode . dockerfile-mode)
					(mode . syslog-mode)
                    (mode . jenkinsfile-mode)))
         ("Markup" (or
					(mode . json-mode)
                    (mode . yaml-mode)
					(mode . markdown-mode)))
         ("Dired" (mode . dired-mode))
         ("Magit" (or
				   (name . "Magit")
				   (name . ".*magit.*")
				   (name . "Magit\\*$")))
		 ("Logs" (or
				  (mode . syslog-mode)
				  (name . "^\\*Messages\\*$")))
         ("Emacs" (or
				   (mode . emacs-lisp-mode)
				   (mode . lisp-interaction-mode)
				   (name . "^\\*Compile-Log\\*$")
				   (name . "^\\*Backtrace\\*$")
				   (name . "^\\*Warnings\\*$")
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Help\\*$")))
                   ;; (name . "^\\*Messages\\*$")
                   ;; (name . "^\\*Compile-Log\\*$")
         ("Other" (or
                   (mode . compilation-mode)
    			   ;; (mode . fundamental-mode)
                   (mode . elisp-compile-mode)
                   (mode . special-mode)
                   (mode . custom-mode)
                   (mode . help-mode)
                   (mode . fundamental-mode)
                   (name . ".*"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

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

; ==================================================
; ===== COLORIZE THE COMPILATION BUFFER ============
; ==================================================

(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))
