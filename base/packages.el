; === Essential Editing & Programming Tools ===

(require 'cl-lib)

(use-package multiple-cursors :defer t
  :bind (("M-SPC"   . rectangle-mark-mode)
         ("C-x SPC" . rectangle-mark-mode)
         ("M-e"     . mc/edit-lines)
         ("C-c C-j" . mc/mark-next-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)))

(use-package move-text :defer t :bind (("M-p" . move-text-up) ("M-n" . move-text-down)))
(use-package magit :defer t :bind (("M-g" . magit) ("C-x g" . magit-status)))
(use-package paredit :defer t)

(use-package dired
  :ensure nil :bind (("C-." . dired-jump)) :config
  (setq dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-dwim-target t
        dired-listing-switches "-alh"
        ls-lisp-ignore-case t)
  (define-key dired-mode-map (kbd "M-r") #'wdired-change-to-wdired-mode))
(use-package diredfl :after dired)
(use-package dired-x :ensure nil :after dired)

; === Optional UI Enhancements ===

(use-package mood-line :defer t)
(use-package ansi-color :defer t :config
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . colorize-compilation-buffer))

; === Completion Frameworks (minibuffer/UI) ===

(use-package company :config (global-company-mode 1))                           ;; auto-completion
(use-package vertico :init (vertico-mode 1) :custom (vertico-count-format nil)) ;; minibuffer completion ui
(use-package vertico-flat :after vertico :init (vertico-flat-mode -1))          ;; minibuffer completion ui (flat)
(use-package orderless :config nil)                                             ;; fzf in minibuffer
(use-package marginalia :config (marginalia-mode 1))                            ;; minibuffer ui/ux
(use-package consult :defer t :bind (("C-l" . consult-line) ("C-r" . consult-ripgrep))) ;; more powerful commands

;; NOTE: Default Keybindings which are based on Emacs- or Custom-Functions.
(use-package emacs
  :bind (; ---- Navigate through Project, File and Buffers
         ("C-,"     . find-file)
         ("M-C-,"   . project-find-file)
         ("C-o"     . other-window)
         ("M-o"     . switch-to-buffer) ; alternative could be: ido-switch-buffer
         ; ---- Compile
         ("M-i"     . buffer-menu) ; alternative could be: ibuffer
         ("M-c"     . compile)
         ("M-s"     . shell-command)
         ("M-q"     . kill-compilation)
         ; ---- Macro
         ("C-x ("   . start-kbd-macro)
         ("C-x )"   . end-kbd-macro)
         ("C-x e"   . call-last-kbd-macro)
         ; ---- Selection
         ("M-w"     . mark-word)
         ("M-a"     . mark-page)
         ("M-t"     . mark-paragraph)
         ("M-F"     . mark-defun)
         ; ---- Manipulation
         ("M-z"     . undo)
         ("M-d"     . lr/duplicate-line)
         ("C-c C-d" . lr/duplicate-line)
         ("M-r"     . lr/delete-line)
         ("C-c C-r" . lr/delete-line)
         ; ---- (G)UI
         ("C-="     . (lambda () (interactive) (text-scale-increase 1)))
         ("C-+"     . (lambda () (interactive) (text-scale-increase 1)))
         ("C--"     . (lambda () (interactive) (text-scale-decrease 1)))
         ("M-ESC"   . lr/reload)))

; NOTE: originally those keymaps were meant to be on the function row, since
;  some keyboards, like the HHKB Boards, dont have a seperated function row,
;  those keymaps are kinda hard to hit. thereby they are also mapped into non
;  function row keybindings. Only to fit into such keyboards as well. In the
;  future this might get solved in other way.
(use-package custom-keys :bind
  (("M-1" . lr/toggle-scratch-buffer)       ("<f1>" . lr/toggle-scratch-buffer)
   ("M-2" . lr/toggle-compilation-buffer)   ("<f2>" . lr/toggle-compilation-buffer)
   ("M-3" . isearch-forward-thing-at-point) ("<f3>" . isearch-forward-thing-at-point)
   ("M-4" . lr/toggle-config)               ("<f4>" . lr/toggle-config)
   ("M-5" . lr/toggle-theme)                ("<f5>" . lr/toggle-theme)
   ("M-6" . lr/default-theme)               ("<f6>" . lr/default-theme)
   ("M-7" . lr/toggle-system)               ("<f7>" . lr/toggle-system)
   ("M-9" . fundamental-mode)               ("<f9>" . fundamental-mode)))

;; NOTE: We want to Copy to Clipboard, which is done with simpleclip. However
;;  simpleclip is not working with multiplecursors. Thereby we are copy/paste
;;  to clipboard with simpleclip on default, and if multiple cursors are
;;  activated we are moving back to the default way of copy/past which is
;;  working on all cursors in that case. However, don't forget to reset this
;;  if multiple cursors are disabled. All this is in a hook.
(use-package simpleclip
  :config
  ;; Default bindings when NOT in multiple-cursors-mode
  (global-set-key (kbd "C-y") #'lr/paste)
  (global-set-key (kbd "C-w") #'lr/copy)
  (global-set-key (kbd "C-t") #'lr/cut)

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

; === Language Modes (Syntax highlighting, indentation, etc.) ===

(use-package markdown-mode      :mode ("\\.md\\'"        . markdown-mode))
(use-package dockerfile-mode    :mode ("Dockerfile\\'"   . dockerfile-mode))
(use-package jenkinsfile-mode   :mode ("Jenkinsfile\\'"  . jenkinsfile-mode))
(use-package yaml-mode          :mode ("\\.yaml\\'"      . yaml-mode))
(use-package jinja2-mode        :mode ("\\.j2\\'"        . jinja2-mode))
(use-package go-mode            :mode ("\\.go\\'"        . go-mode))
(use-package rust-mode          :mode ("\\.rs\\'"        . rust-mode))
(use-package rpm-spec-mode      :mode ("\\.spec\\'"      . rpm-spec-mode))
(use-package web-mode           :mode (("\\.html?\\'"    . web-mode)
                                       ("\\.js\\'"       . web-mode)
                                       ("\\.jsx\\'"      . web-mode)
                                       ("\\.tsx\\'"      . web-mode)
                                       ("\\.css\\'"      . web-mode)))

; === History / Recent Files ===

(use-package savehist :init (savehist-mode 1))
(use-package recentf :init (recentf-mode 1))
(use-package whitespace :defer t :config
  (setq whitespace-style
        '(face tabs spaces tab-mark space-mark trailing missing-newline-at-eof
               space-after-tab::tab space-after-tab::space space-before-tab::tab
               space-before-tab::space)))

(use-package display-line-numbers :defer t :config
  (setq-default display-line-numbers-widen t)
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  :init (global-display-line-numbers-mode 1))

(setq prefix-help-command #'embark-prefix-help-command)

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))
