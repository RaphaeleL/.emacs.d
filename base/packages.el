;; packages.el --- Package configuration -*- lexical-binding: t; -*-

; === Essential Editing & Programming Tools ===

(require 'cl-lib)

(use-package multiple-cursors :ensure t
  :bind (("M-SPC"   . rectangle-mark-mode)
         ("C-x SPC" . rectangle-mark-mode)
         ("M-e"     . mc/edit-lines)
         ("C-c C-j" . mc/mark-next-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)))

(use-package move-text :ensure t :bind (("M-p" . move-text-up) ("M-n" . move-text-down)))
(use-package magit     :ensure t)

(use-package dired
  :ensure nil :bind (("C-." . dired-jump)) :config
  (setq dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-dwim-target t
        dired-listing-switches "-alh")
  (define-key dired-mode-map (kbd "M-r") #'wdired-change-to-wdired-mode))
(use-package diredfl :ensure t :after dired :config (diredfl-global-mode 1))
(use-package dired-x :ensure nil :after dired)

; === Optional UI Enhancements ===

(use-package mood-line :ensure t :config (mood-line-mode 1))
(use-package ansi-color :ensure t :config
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . colorize-compilation-buffer))

; === Completion Frameworks (minibuffer/UI) ===

(use-package company    :ensure t :config (global-company-mode 1))                             ;; auto-completion
(use-package vertico    :ensure t :config (vertico-mode 1) :custom (vertico-count-format nil)) ;; minibuffer completion ui
(use-package orderless  :ensure t :config nil)                                                 ;; fzf in minibuffer
(use-package marginalia :ensure t :config (marginalia-mode 1))                                 ;; better minibuffer ui/ux
(use-package consult
  :ensure t
  :bind (("C-l"   . consult-line)
         ("C-r"   . consult-ripgrep)
         ("C-x b" . consult-buffer)))

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; NOTE: Default Keybindings which are based on Emacs- or Custom-Functions.
(use-package emacs
  :bind (; ---- Navigate through Project, File and Buffers
         ("C-,"     . find-file)
         ("M-C-,"   . project-find-file)
         ("C-o"     . other-window)
         ("M-o"     . switch-to-buffer)
         ; ---- Compile
         ("M-i"     . buffer-menu)
         ("M-c"     . compile)
         ("M-s"     . shell-command)
         ("M-q"     . kill-compilation)
         ; ---- Macro
         ("C-x ("   . start-kbd-macro)
         ("C-x )"   . end-kbd-macro)
         ("C-x e"   . call-last-kbd-macro)
         ; ---- Selection
         ("M-w"     . mark-word)
         ("M-k"     . mark-sexp)
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
         ("C-c n"   . lr/toggle-line-numbers)
         ("C-c w"   . lr/toggle-whitespace)
         ("C-c t"   . lr/transparent)))

;; NOTE: originally those keymaps were meant to be on the function row, since
;;  some keyboards, like the HHKB Boards, dont have a seperated function row,
;;  those keymaps are kinda hard to hit. thereby they are also mapped into non
;;  function row keybindings. Only to fit into such keyboards as well. In the
;;  future this might get solved in other way.
(use-package custom-keys :bind
  (("M-1" . lr/toggle-scratch-buffer)       ("<f1>" . lr/toggle-scratch-buffer)
   ("M-2" . lr/toggle-compilation-buffer)   ("<f2>" . lr/toggle-compilation-buffer)
   ("M-3" . isearch-forward-thing-at-point) ("<f3>" . isearch-forward-thing-at-point)
   ("M-4" . lr/toggle-ui)                   ("<f4>" . lr/toggle-ui)
   ; ... space for more
   ("M-9" . fundamental-mode)               ("<f9>" . fundamental-mode)))

;; NOTE: We want to Copy to Clipboard, which is done with simpleclip. However
;;  simpleclip is not working with multiplecursors. Thereby we are copy/paste
;;  to clipboard with simpleclip on default, and if multiple cursors are
;;  activated we are moving back to the default way of copy/past which is
;;  working on all cursors in that case. However, don't forget to reset this
;;  if multiple cursors are disabled. All this is in a hook.
(use-package simpleclip
  :demand t
  :ensure t
  :config
  (simpleclip-mode 1)
  ;; --- Default behavior ---
  (global-set-key (kbd "C-w") #'lr/copy)
  (global-set-key (kbd "C-y") #'lr/paste)
  (global-set-key (kbd "C-t") #'lr/cut)

  ;; --- Multiple cursors override ---
  (with-eval-after-load 'multiple-cursors
    (define-key mc/keymap (kbd "C-w") #'kill-ring-save)
    (define-key mc/keymap (kbd "C-y") #'yank)
    (define-key mc/keymap (kbd "C-t") #'kill-region)))

; === Language Modes (Syntax highlighting, indentation, etc.) ===

(use-package markdown-mode    :ensure t :mode ("\\.md\\'"       . markdown-mode))
(use-package dockerfile-mode  :ensure t :mode ("Dockerfile\\'"  . dockerfile-mode))
(use-package jenkinsfile-mode :ensure t :mode ("Jenkinsfile\\'" . jenkinsfile-mode))
(use-package yaml-mode        :ensure t :mode ("\\.yaml\\'"     . yaml-mode))
(use-package jinja2-mode      :ensure t :mode ("\\.j2\\'"       . jinja2-mode))
(use-package go-mode          :ensure t :mode ("\\.go\\'"       . go-mode))
(use-package rust-mode        :ensure t :mode ("\\.rs\\'"       . rust-mode))
(use-package rpm-spec-mode    :ensure t :mode ("\\.spec\\'"     . rpm-spec-mode))
(use-package web-mode         :ensure t :mode (("\\.html?\\'"   . web-mode)
                                               ("\\.js\\'"      . web-mode)
                                               ("\\.jsx\\'"     . web-mode)
                                               ("\\.tsx\\'"     . web-mode)
                                               ("\\.css\\'"     . web-mode)))

; === Misc ===

(use-package savehist :init (savehist-mode 1))
(use-package recentf :init (recentf-mode 1))
(use-package whitespace :ensure t :config
  (setq whitespace-style
        '(face tabs spaces tab-mark space-mark trailing missing-newline-at-eof
               space-after-tab::tab space-after-tab::space space-before-tab::tab
               space-before-tab::space)))

(use-package display-line-numbers :ensure t :config
  (setq-default display-line-numbers-widen t)
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  :init (global-display-line-numbers-mode 1))

(provide 'packages)
;;; packages.el ends here
