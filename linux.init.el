; ==================================================
; ===== MELPA ======================================
; ==================================================

(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

; ==================================================
; ===== CUSTOM FUNCTIONS ===========================
; ==================================================

; some 'rc'-functions are copied from
; https://github.com/rexim/dotfiles/blob/master/.emacs.rc/rc.el

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

(defun rc/delete-line ()
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun rc/duplicate-line ()
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defun rc/on_save ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-12")
   ((eq system-type 'darwin) "Iosevka-20")
   ((eq system-type 'gnu/linux) "IosevkaNerdFont-12")))

(defun rc/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

(defun rc/cut ()
  (interactive)
  (simpleclip-cut (region-beginning) (region-end))
  (deactivate-mark)
  (message "Cutted")
  (sit-for 1))

(defun rc/copy ()
  (interactive)
  (simpleclip-copy (region-beginning) (region-end))
  (deactivate-mark)
  (message "Copied")
  (sit-for 1))

(defun rc/paste ()
  (interactive)
  (simpleclip-paste)
  (deactivate-mark)
  (message "Pasted")
  (sit-for 1))

(defun rc/colorize-compilation-buffer ()
  (read-only-mode 'toggle)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 'toggle)
  )

(defun rc/toggle-themes ()
  "Toggle between 'doom-one-light' and 'gruber-darker' themes."
  (interactive)
  (if (member 'doom-one-light custom-enabled-themes)
      (progn
        (disable-theme 'doom-one-light)
        (load-theme 'gruber-darker t))
    (progn
      (disable-theme 'gruber-darker)
      (load-theme 'doom-one-light t))))

; ==================================================
; ===== PACKAGES ===================================
; ==================================================

(rc/require 'which-key)
(rc/require 'simpleclip)
(rc/require 'move-text)
(rc/require 'multiple-cursors)
(rc/require 'paredit)
(rc/require 'magit)

(rc/require 'eglot)
(rc/require 'company)

(rc/require 'mood-line)
(rc/require 'gruber-darker-theme)
(rc/require 'modus-themes)
(rc/require 'doom-themes)
(rc/require 'ansi-color)

(rc/require 'vertico)
(rc/require 'orderless)
(rc/require 'marginalia)
(rc/require 'counsel)

(rc/require 'markdown-mode)
; (rc/require 'yaml-mode)
(rc/require 'dockerfile-mode)
(rc/require 'jenkinsfile-mode)

; ==================================================
; ====== MINIBUFFER ================================
; ==================================================

; (fido-mode 1)

(use-package vertico
    :ensure t
    :config
        (vertico-mode))
(vertico-mode 1)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
    :bind (:map minibuffer-local-map
            ("M-A" . marginalia-cycle))
    :init
        (marginalia-mode))
(marginalia-mode 1)

(use-package counsel
    :init
        (counsel-mode))
(counsel-mode 1)

; ==================================================
; ====== SSH =======================================
; ==================================================

(setq tramp-default-method "ssh")
(setq tramp-ssh-controlmaster-options "")
(setq tramp-verbose 10)
(defun open-devvm ()
  "Open the CCMS README file via plink SSH."
  (interactive)
  (find-file "/plink:raliccia@10.26.15.66:/home/raliccia/"))
(global-set-key (kbd "C-x C-r") 'open-devvm)

; ==================================================
; ====== PACKAGE SETTINGS ==========================
; ==================================================

(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
(setq dired-listing-switches "-laGh1Dv --group-directories-first")
(setq ls-lisp-ignore-case t)

(which-key-mode t)
(setq which-key-separator "  ")
(setq which-key-prefix-prefix "... ")
(setq which-key-max-display-columns 3)
(setq which-key-idle-delay 0.125)
(setq which-key-idle-secondary-delay 0.25)
(setq which-key-add-column-padding 1)
(setq which-key-max-description-length 40)

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

;(setq compilation-scroll-output t)

(use-package display-line-numbers
  :ensure nil
  :bind
  ("<f7>" . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  (setq-default display-line-numbers-widen t)
  :init
  (global-display-line-numbers-mode 1))

; ==================================================
; ===== UI =========================================
; ==================================================

(set-fringe-mode 0)

(mood-line-mode 1)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

(setq custom-safe-themes 1)

;; Load initial theme
; (load-theme 'modus-operandi 1)
; (set-face-attribute 'mode-line nil :box nil)
; (load-theme 'doom-one-light 1)
(load-theme 'gruber-darker t)

(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 120))

(blink-cursor-mode 0)
(setq x-stretch-cursor nil)

(setq ring-bell-function 'ignore)

(setq echo-keystrokes 0.01)

(setq mouse-yank-at-point t)

(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

; ==================================================
; ===== BASIC SHIT =================================
; ==================================================

(save-place-mode 1)
(simpleclip-mode 1)

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq package-enable-at-startup nil)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

; ==================================================
; ===== KEYMAPS ====================================
; ==================================================

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "N") 'dired-create-empty-file))

(global-set-key (kbd "M-w") 'mark-word)
(global-set-key (kbd "M-a") 'mark-page)
(global-set-key (kbd "M-F") 'mark-defun)
(global-set-key (kbd "M-s") 'mark-paragraph)

(global-set-key (kbd "M-j") 'join-line)

(global-set-key (kbd "M-z") 'undo)

(global-set-key (kbd "C-,") 'find-file)
(global-set-key (kbd "C-.") 'dired)
(global-set-key (kbd "C-c C-,") 'project-find-file)

(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c s") 'shell-command)

(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-log)

(global-set-key (kbd "M-i") 'ibuffer)
(global-set-key (kbd "M-l") 'switch-to-buffer)
(global-set-key (kbd "C-c i") 'ibuffer)
(global-set-key (kbd "C-c l") 'switch-to-buffer)

(global-set-key (kbd "C-w") 'rc/copy)
(global-set-key (kbd "C-y") 'rc/paste)
(global-set-key (kbd "C-t") 'rc/cut)

(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m") 'mc/mark-more-like-this-extended)

(global-set-key (kbd "M-d") 'rc/duplicate-line)
(global-set-key (kbd "M-r") 'rc/delete-line)

(global-set-key (kbd "<f3>") 'rc/toggle-themes)

; ==================================================
; ===== LSP ========================================
; ==================================================

(add-to-list 'exec-path "~/.local/bin")
(with-eval-after-load 'company
  (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

; ==================================================
; ===== HOOKS ======================================
; ==================================================

;; (add-hook 'c-mode-hook             'eglot-ensure)
;; (add-hook 'c++-mode-hook           'eglot-ensure)
;; (add-hook 'python-mode-hook        'eglot-ensure)

(add-hook 'after-init-hook         'global-company-mode)

(add-hook 'text-mode-hook          'rc/turn-on-paredit)

(add-hook 'c-mode-hook             'rc/on_save)
(add-hook 'c++-mode-hook           'rc/on_save)
(add-hook 'simpc-mode-hook         'rc/on_save)
(add-hook 'python-mode-hook        'rc/on_save)
(add-hook 'markdown-mode-hook      'rc/on_save)
(add-hook 'text-mode-hook          'rc/on_save)
(add-hook 'emacs-lisp-mode-hook    'rc/on_save)
(add-hook 'jenkinsfile-mode-hook   'rc/on_save)
(add-hook 'dockerfile-mode-hook    'rc/on_save)
(add-hook 'makefile-mode-hook      'rc/on_save)
(add-hook 'rpm-spec-mode-hook      'rc/on_save)

; ==================================================
; ===== COLORIZE THE COMPILATION BUFFER ============
; ==================================================

; (setq compilation-environment '("TERM=xterm-256color"))
; (defun my/advice-compilation-filter (f proc string)
;   (funcall f proc (xterm-color-filter string)))
; (advice-add 'compilation-filter :around #'my/advice-compilation-filter)

; ==================================================
; ===== CUSTOM =====================================
; ==================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rpm-spec-mode dockerfile-mode doom-themes markdown-mode ssh theme-changer jenkinsfile-mode xterm-color groovy-mode magit multiple-cursors move-text simpleclip gruber-darker-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
