; ==================================================
; ===== PACKAGES ===================================
; ==================================================

; === Essential Editing & Programming Tools ===

(use-package move-text
  :ensure t
  :defer t
  :bind (("M-p"     . move-text-up)
         ("M-n"     . move-text-down)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("M-SPC"     . rectangle-mark-mode)
         ("C-x SPC"   . rectangle-mark-mode)
         ("M-e"       . mc/edit-lines)
         ("C-j"       . mc/mark-next-like-this)
         ("C-c C-j"   . mc/mark-next-like-this)))

(use-package magit
  :ensure t
  :defer t
  :bind (("M-g"    . magit)
         ("C-x g"  . magit-status)))

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
    (define-key dired-mode-map (kbd "M-r") #'wdired-change-to-wdired-mode)))

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
         ("C-c C-g" . consult-ripgrep)      ; ripgrep might not be available everywhere ..
         ("C-r"     . consult-recent-file)
         ("M-o"     . consult-buffer)
         ("C-c C-o" . consult-buffer)))

; === Language Modes (Syntax highlighting, indentation, etc.) ===

(use-package markdown-mode      :ensure t :mode ("\\.md\\'"        . markdown-mode))
(use-package dockerfile-mode    :ensure t :mode ("Dockerfile\\'"   . dockerfile-mode))
(use-package jenkinsfile-mode   :ensure t :mode ("Jenkinsfile\\'"  . jenkinsfile-mode))
(use-package yaml-mode          :ensure t :mode ("\\.yaml\\'"      . yaml-mode))
(use-package jinja2-mode        :ensure t :mode ("\\.j2\\'"        . jinja2-mode))
(use-package go-mode            :ensure t :mode ("\\.go\\'"        . go-mode))
(use-package rust-mode          :ensure t :mode ("\\.rs\\'"        . rust-mode))
(use-package rpm-spec-mode      :ensure t :mode ("\\.spec\\'"      . rpm-spec-mode))
(use-package web-mode           :ensure t :mode (("\\.html?\\'"    . web-mode)
                                                 ("\\.js\\'"       . web-mode)
                                                 ("\\.jsx\\'"      . web-mode)
                                                 ("\\.tsx\\'"      . web-mode)
                                                 ("\\.css\\'"      . web-mode)))

; === History / Recent Files ===

(use-package savehist :ensure t :init (savehist-mode 1))
(use-package recentf  :ensure t :init (recentf-mode 1))
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
  (setq-default display-line-numbers-widen t)
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  :init
  (global-display-line-numbers-mode 1))

(setq prefix-help-command #'embark-prefix-help-command)

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Coding" (or
                    (mode . c-mode)
                    (mode . simpc-mode)
                    (mode . c++-mode)
                    (mode . java-mode)
                    (mode . js-mode)
                    (mode . typescript-mode)
                    (mode . lua-mode)
                    (mode . yaml-mode)
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
                    (mode . text-mode)
                    (mode . markdown-mode)))
         ("Dired" (mode   . dired-mode))
         ("Magit" (or
                   (name  . "Magit")
                   (name  . ".*magit.*")
                   (name  . "Magit\\*$")))
         ("Logs" (or
                  (mode   . syslog-mode)
                  (name   . "^\\*Messages\\*$")))
         ("Emacs" (or
                   (mode  . emacs-lisp-mode)
                   (mode  . lisp-interaction-mode)
                   (mode  . help-mode)
                   (name  . "^\\*Compile-Log\\*$")
                   (name  . "^\\*Backtrace\\*$")
                   (name  . "^\\*Warnings\\*$")
                   (name  .  "^\\*scratch\\*$")
                   (name  . "^\\*Help\\*$")))
         ("Other" (or
                   (mode  . compilation-mode)
                   (mode  . elisp-compile-mode)
                   (mode  . special-mode)
                   (mode  . custom-mode)
                   (mode  . fundamental-mode)
                   (name  . ".*"))))))

(add-hook 'ibuffer-mode-hook (lambda ()
                               (ibuffer-switch-to-saved-filter-groups "default")))

;; NOTE: Default Keybindings which are based on Emacs- or Custom-Functions.
(use-package emacs
  :ensure nil
  :bind (("C-,"     . find-file)
         ("M-,"     . project-find-file)
         ("M-i"     . ibuffer)
         ("M-j"     . indent-region)
         ("C-c g"   . indent-region)
         ("M-c"     . compile)
         ("C-l"     . shell-command)
         ("M-q"     . kill-compilation)
         ("C-o"     . other-window)
         ("C-x ("   . start-kbd-macro)
         ("C-x )"   . end-kbd-macro)
         ("C-x e"   . call-last-kbd-macro)
         ("M-w"     . mark-word)
         ("M-a"     . mark-page)
         ("M-F"     . mark-defun)
         ("M-s"     . mark-paragraph)
         ("M-z"     . undo)
         ("M-d"     . lr/duplicate-line)
         ("C-c C-d" . lr/duplicate-line)
         ("M-r"     . lr/delete-line)
         ("C-c C-r" . lr/delete-line)
         ("C-="     . lr/font-increase)
         ("C-+"     . lr/font-increase)
         ("C--"     . lr/font-decrease)))

; NOTE: originally those keymaps were meant to be on the function row, since
;  some keyboards, like the HHKB Boards, dont have a seperated function row,
;  those keymaps are kinda hard to hit. thereby they are also mapped into non
;  function row keybindings. Only to fit into such keyboards as well. In the
;  future this might get solved in other way.
(use-package custom-keys
  :ensure nil
  :bind (
         ("M-1"     . lr/toggle-scratch-buffer)
         ("<f1>"    . lr/toggle-scratch-buffer)
         ("M-2"     . lr/toggle-compilation-buffer)
         ("<f2>"    . lr/toggle-compilation-buffer)
         ("M-3"     . isearch-forward-symbol-at-point)
         ("<f3>"    . isearch-forward-symbol-at-point)
         ("M-4"     . lr/toggle-config)
         ("<f4>"    . lr/toggle-config)
         ("M-5"     . lr/toggle-mini-buffer-mode)
         ("<f5>"    . lr/toggle-mini-buffer-mode)))

;; NOTE: We want to Copy to Clipboard, which is done with simpleclip. However
;;  simpleclip is not working with multiplecursors. Thereby we are copy/paste
;;  to clipboard with simpleclip on default, and if multiple cursors are
;;  activated we are moving back to the default way of copy/past which is
;;  working on all cursors in that case. However, don't forget to reset this
;;  if multiple cursors are disabled. All this is in a hook.
(use-package simpleclip
  :ensure t
  :defer t
  :bind (("C-t" . lr/cut))
  :config
  ;; Default bindings when NOT in multiple-cursors-mode
  (global-set-key (kbd "C-y") #'lr/paste)
  (global-set-key (kbd "C-w") #'lr/copy)

  ;; Function to swap bindings dynamically
  (defun lr/mc-setup-bindings ()
    "Set key bindings for multiple-cursors mode."
    (local-set-key (kbd "C-y") #'clipboard-yank)
    (local-set-key (kbd "C-w") #'clipboard-kill-ring-save))

  (defun lr/mc-reset-bindings ()
    "Restore global bindings after leaving multiple-cursors mode."
    (local-set-key (kbd "C-y") #'lr/paste)
    (local-set-key (kbd "C-w") #'lr/copy))

  ;; Hook into multiple-cursors
  (add-hook 'multiple-cursors-mode-enabled-hook #'lr/mc-setup-bindings)
  (add-hook 'multiple-cursors-mode-disabled-hook #'lr/mc-reset-bindings))
