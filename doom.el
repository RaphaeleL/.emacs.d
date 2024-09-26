; =============================================================================
; ===== Package Management ====================================================
; =============================================================================

(require 'package)
(add-to-list 'package-archives  '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

; =============================================================================
; ===== Custom Functions ======================================================
; =============================================================================

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (unless rc/package-contents-refreshed
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (unless (package-installed-p package)
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(defun rc/require-theme (theme)
  (let
         (theme-package-name (concat theme-name "-theme"))
         (theme-package (intern theme-package-name)))
    (rc/require theme-package)
    (load-theme theme t))

; =============================================================================
; ===== Custom Configuration ==================================================
; =============================================================================

(save-place-mode 1)

(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  default-input-method "MacOSX"))

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq default-directory "~/")
(setq package-enable-at-startup nil)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

; =============================================================================
; ===== Garbage Collection ====================================================
; =============================================================================

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq gc-cons-threshold 100000000)

(defvar better-gc-cons-threshold 134217728)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold most-positive-fixnum))
              
            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold)) 
              
            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

; =============================================================================
; ===== UI Configuration ======================================================
; =============================================================================

(rc/require 'doom-themes)
(load-theme 'doom-one-light 1)

(rc/require 'mood-line)
(mood-line-mode 1)

(rc/require 'perfect-margin)
(perfect-margin-mode 0)

(rc/require 'spacious-padding)
(spacious-padding-mode 1)

(set-fringe-mode 0)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(defun get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-12")
   ((eq system-type 'darwin) "Iosevka-14")
   ((eq system-type 'gnu/linux) "Iosevka-12")))

(add-to-list 'default-frame-alist `(font . ,(get-default-font)))

(setq custom-safe-themes 1) 

; =============================================================================
; ===== Package Settings ======================================================
; =============================================================================

(use-package whitespace
  :ensure nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . delete-trailing-whitespace))
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
  :bind
  ("<f7>" . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  (setq-default display-line-numbers-widen t))

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-lah")
  (setq ls-lisp-ignore-case t))

; =============================================================================
; ===== Completion Configuration ==============================================
; =============================================================================

(rc/require 'vertico)
(use-package vertico
    :ensure t
    :config
        (vertico-mode))
(vertico-mode 1)

(rc/require 'orderless)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(rc/require 'marginalia)
(use-package marginalia
    :bind (:map minibuffer-local-map
            ("M-A" . marginalia-cycle))

    :init
        (marginalia-mode))
(marginalia-mode 1)

(rc/require 'counsel)
(use-package counsel
    :init
        (counsel-mode))
(counsel-mode 1)

; =============================================================================
; ===== Custom Keybindings ====================================================
; =============================================================================

(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "N") 'dired-create-empty-file))

(global-set-key (kbd "M-w") 'mark-word)
(global-set-key (kbd "M-a") 'mark-page)
(global-set-key (kbd "M-F") 'mark-defun)
(global-set-key (kbd "M-s") 'mark-paragraph)

(global-set-key (kbd "C-c C-a") 'beginning-of-defun)
(global-set-key (kbd "C-c C-e") 'end-of-defun)

(global-set-key (kbd "C-c C-n") 'duplicate-line)
(global-set-key (kbd "C-c C-d") 'delete-current-line)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "M-z") 'undo)

(global-set-key (kbd "C-,") 'find-file)
(global-set-key (kbd "C-.") 'dired)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c n") 'duplicate-line)
(global-set-key (kbd "C-c d") 'delete-current-line)
(global-set-key (kbd "C-c a") 'beginning-of-defun)
(global-set-key (kbd "C-c e") 'end-of-defun)

(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "C-c s") 'shell-command)

(rc/require 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m") 'mc/mark-more-like-this-extended)

(global-set-key (kbd "M-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (text-scale-decrease 1)))

(global-set-key (kbd "C-x .") 'dired)
(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-log-all)

(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "C-c i") 'ibuffer)
(global-set-key (kbd "C-c l") 'counsel-switch-buffer)
(global-set-key (kbd "C-c k") (lambda () (interactive) (kill-current-buffer)))

(rc/require 'simpleclip)
(simpleclip-mode 1)
(global-set-key (kbd "C-c c") 'simpleclip-copy)
(global-set-key (kbd "C-c v") 'simpleclip-paste)

(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(rc/require 'which-key)
(use-package which-key
    :ensure nil ; built into Emacs 30
    :hook (after-init . which-key-mode)
    :config
    (setq which-key-separator "  ")
    (setq which-key-prefix-prefix "... ")
    (setq which-key-max-display-columns 3)
    (setq which-key-idle-delay 0.125)
    (setq which-key-idle-secondary-delay 0.25)
    (setq which-key-add-column-padding 1)
    (setq which-key-max-description-length 40))
(which-key-mode t)

; =============================================================================
; ===== Language Server Protocol ==============================================
; =============================================================================

(unless (package-installed-p 'eglot)
  (package-install 'eglot))

(rc/require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c C-d") 'eldoc-doc-buffer))

(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(rc/require 'markdown-mode)

(rc/require 'web-mode)
(rc/require 'flycheck)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
	 default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
