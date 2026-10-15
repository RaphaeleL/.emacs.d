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

; === Language Server Eglot Stuff ===

(use-package eglot
  :ensure nil
  :config
  (setq eglot-send-changes-idle-time 0.1
        eglot-extend-to-xref t
        eglot-autoshutdown t
        eglot-code-action-indicator "h"
        eglot-stay-out-of '(flymake))
  ;; Eglot's JSON-RPC event log is extremely noisy and needlessly expensive.
  (when (fboundp 'jsonrpc--log-event) (fset #'jsonrpc--log-event #'ignore))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-category-defaults nil
                          eglot-events-buffer-size 0))))

; === Optional UI Enhancements ===

(use-package mood-line :ensure t :config (mood-line-mode 1))
(use-package ansi-color :ensure t :config
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . colorize-compilation-buffer))

; === Modern Completion Frameworks (minibuffer/UI) ===

;; (use-package vertico
;;   :demand t
;;   :init (vertico-mode 1)
;;   :bind (:map vertico-map ("M-DEL" . vertico-directory-delete-word)))

;; (use-package vertico-directory :ensure nil :after vertico)

;; (use-package marginalia :after vertico :init (marginalia-mode 1))

;; (use-package orderless
;;   :after vertico
;;   :config
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles basic partial-completion)))))

;; (use-package corfu
;;   :demand t
;;   :init (global-corfu-mode 1)
;;   :bind (:map corfu-map
;;               ("SPC" . corfu-insert-separator)
;;               ("C-n" . corfu-next)
;;               ("C-p" . corfu-previous)))

;; (use-package corfu-popupinfo
;;   :ensure nil
;;   :after corfu
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :custom (corfu-popupinfo-delay '(0.25 . 0.1)))

;; (use-package cape
;;   :after corfu
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file))

;; (use-package kind-icon
;;   :if (display-graphic-p)
;;   :after corfu
;;   :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package avy
;;   :bind (("C-c j" . avy-goto-line)
;;          ("s-j" . avy-goto-char-timer)
;;          :map isearch-mode-map
;;          ("s-j" . avy-isearch)))

;; (use-package embark
;;   :demand t
;;   :after avy
;;   :bind (("C-c a" . embark-act))
;;   :init
;;   (defun lr/avy-action-embark (point)
;;     "Run `embark-act' at the Avy candidate selected at POINT."
;;     (unwind-protect
;;         (save-excursion (goto-char point) (embark-act))
;;       (select-window (cdr (ring-ref avy-ring 0))))
;;     t)
;;   (setf (alist-get ?. avy-dispatch-alist) #'lr/avy-action-embark)
;;   :config
;;   (when (fboundp 'which-key-mode) (which-key-mode -1))
;;   (setq embark-auto-prefix-help-delay 1.0)
;;   (embark-auto-prefix-help-mode))

;; (use-package embark-consult :after (embark consult))
;; (use-package consult
;;   :ensure t
;;   :bind (("C-l"   . consult-line)
;;          ("C-r"   . consult-ripgrep)
;;          ("C-x b" . consult-buffer)))

;; Modern built-in completion defaults, kept separate from the optional Vertico setup so the latter can still be disabled per platform.
; (setq enable-recursive-minibuffers t
;       completion-cycle-threshold 1
;       completions-detailed t
;       tab-always-indent 'complete
;       completion-styles '(basic initials substring)
;       completion-auto-help 'always
;       completions-format 'one-column
;       completions-max-height 20)
; (setq minibuffer-visible-completions t)
; (keymap-set minibuffer-mode-map "TAB" #'minibuffer-complete)

(use-package isearch
  :ensure nil
  :custom
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-allow-scroll t)
  (isearch-repeat-on-direction-change t)
  (isearch-wrap-pause 'no-ding))

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
         ("C-c f"   . lr/find-corresponding-clang-file)
         ("C-c t"   . lr/transparent))
  :config
      (when (boundp 'treesit-auto-install-grammar) (setq treesit-auto-install-grammar 'ask))
      (setq treesit-font-lock-level 3)
      (add-hook 'prog-mode-hook #'electric-pair-mode)
      (setq sentence-end-double-space nil
            save-interprogram-paste-before-kill t
            kill-do-not-save-duplicates t
            reb-re-syntax 'string)
      (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
      (advice-add 'save-place-find-file-hook :after
                  (lambda (&rest _)
                    (when buffer-file-name (ignore-errors (recenter))))))

;; NOTE: originally those keymaps were meant to be on the function row, since
;;  some keyboards, like the HHKB Boards, dont have a seperated function row,
;;  those keymaps are kinda hard to hit. thereby they are also mapped into non
;;  function row keybindings. Only to fit into such keyboards as well. In the
;;  future this might get solved in other way.
(use-package custom-keys :bind
  (("M-1" . lr/toggle-scratch-buffer)       ("<f1>" . lr/toggle-scratch-buffer)
   ("M-2" . lr/toggle-compilation-buffer)   ("<f2>" . lr/toggle-compilation-buffer)
   ("M-3" . isearch-forward-thing-at-point) ("<f3>" . isearch-forward-thing-at-point)
   ("M-4" . lr/toggle-line-numbers)         ("<f4>" . lr/toggle-line-numbers)
   ("M-5" . lr/toggle-colors)               ("<f5>" . lr/toggle-colors)
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

;; Built-in defaults for programming and navigation.
; (add-hook 'prog-mode-hook #'electric-pair-mode)
; (global-hl-line-mode 1)
; (setq global-hl-line-sticky-flag 'window)

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
