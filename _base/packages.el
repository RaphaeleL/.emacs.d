; ==================================================
; ===== PACKAGES ===================================
; ==================================================

; === Essential Editing & Programming Tools ===

(use-package simpleclip
  :ensure t
  :defer t
  :bind (("C-y" . lr/paste)
         ("C-w" . lr/copy)
         ("C-t" . lr/cut)
         ("C-/" . lr/visual-or-line-copy)
         ("M-w" . mark-word)
         ("M-a" . mark-page)
         ("M-F" . mark-defun)
         ("M-s" . mark-paragraph)))

(use-package move-text
  :ensure t
  :defer t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)
         ("C-c C-j" . join-line)
         ("M-d" . lr/duplicate-line)
         ("M-r" . lr/delete-line)
         ("M-z" . undo)
         ("C-c g" . indent-region)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("M-SPC" . rectangle-mark-mode)
         ("M-e"   . mc/edit-lines)
         ("C-j"   . mc/mark-next-like-this)
         ("C-k"   . mc/mark-previous-like-this)))

(use-package magit
  :ensure t
  :defer t
  :bind (("M-g" . magit)
          ("C-x g" . magit-status)))

(use-package eglot :ensure t :defer t)
(use-package paredit :ensure t :defer t)

(use-package dired
  :ensure nil
  :defer t
  :bind (("C-." . dired-jump)))

(use-package dired-x :after dired :config
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
    (setq dired-omit-files "^\\.[^.].*")))

; === Optional UI Enhancements ===

(use-package mood-line :ensure t :defer t)
(use-package ansi-color
  :ensure nil
  :defer t
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

(use-package which-key
  :ensure t
  :defer t
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 0.125)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40))

; === Completion Frameworks (minibuffer/UI) ===

(use-package vertico :ensure t :config (vertico-mode 1))
(use-package orderless :ensure t :config nil)
(use-package marginalia :ensure t :config (marginalia-mode 1))
(use-package corfu :ensure t :config
  (global-corfu-mode 1)
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current nil))
(use-package consult
  :ensure t
  :defer t
  :bind (("C-c C-k" . consult-line)
         ("C-c C-g" . consult-ripgrep)
         ("C-r"     . consult-recent-file)
         ("M-o"     . consult-buffer)))

; === Language Modes (Syntax highlighting, indentation, etc.) ===

(use-package markdown-mode :ensure t :mode ("\\.md\\'" . markdown-mode))
(use-package dockerfile-mode :ensure t :mode ("Dockerfile\\'" . dockerfile-mode))
(use-package jenkinsfile-mode :ensure t :mode ("Jenkinsfile\\'" . jenkinsfile-mode))
(use-package yaml-mode :ensure t :mode ("\\.yaml\\'" . yaml-mode))
(use-package jinja2-mode :ensure t :mode ("\\.j2\\'" . jinja2-mode))
(use-package go-mode :ensure t :mode ("\\.go\\'" . go-mode))
(use-package rust-mode :ensure t :mode ("\\.rs\\'" . rust-mode))
(use-package rpm-spec-mode :ensure t :mode ("\\.spec\\'" . rpm-spec-mode))
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.css\\'" . web-mode)))

; === History / Recent Files ===

(use-package savehist :ensure t :init (savehist-mode 1))
(use-package recentf :ensure t :init (recentf-mode 1))

(use-package whitespace
  :ensure nil
  :defer t
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
  :defer t
  :config
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  (setq-default display-line-numbers-widen t)
  :init
  (global-display-line-numbers-mode 1))

(setq prefix-help-command #'embark-prefix-help-command)

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

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
         ("Other" (or
                   (mode . compilation-mode)
                   (mode . elisp-compile-mode)
                   (mode . special-mode)
                   (mode . custom-mode)
                   (mode . help-mode)
                   (mode . fundamental-mode)
                   (name . ".*"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(use-package emacs
  :ensure nil
  :bind (("C-," . find-file)
         ("M-," . project-find-file)
         ("M-i" . ibuffer)
         ("M-c" . compile)
         ("C-l" . shell-command)
         ("M-q" . kill-compilation)
         ("C-o" . other-window)
         ("C-=" . lr/font-increase)
         ("C-+" . lr/font-increase)
         ("C--" . lr/font-decrease)
         ("M-=" . global-text-scale-adjust)
         ("M-+" . global-text-scale-adjust)))

(use-package custom-keys
  :ensure nil
  :bind (("<f1>" . lr/toggle-scratch-buffer)
         ("<f2>" . lr/toggle-compilation-buffer)
         ("<f3>" . lr/load-theme)
         ("<f4>" . lr/open_config)
         ("<f5>" . lr/toggle-mini-buffer-mode)
         ("<f6>" . whitespace-mode)
         ("<f7>" . display-line-numbers-mode)
         ("<f8>" . which-key-mode)
         ("<f9>" . embark-bindings)))
