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

(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-14")
   ((eq system-type 'darwin) "Iosevka-20")
   ((eq system-type 'gnu/linux) "Iosevka-20")))

(defun rc/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

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
  (read-only-mode 'toggle))

; ==================================================
; ===== PACKAGES ===================================
; ==================================================

(rc/require 'which-key)
(rc/require 'eglot)
(rc/require 'company)
(rc/require 'mood-line)
(rc/require 'gruber-darker-theme)
(rc/require 'simpleclip)
(rc/require 'move-text)
(rc/require 'multiple-cursors)
(rc/require 'paredit)
(rc/require 'magit)

; ==================================================
; ====== PACKAGE SETTINGS ==========================
; ==================================================

(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lah")
(setq ls-lisp-ignore-case t)

(which-key-mode t)
(setq which-key-separator "  ")
(setq which-key-prefix-prefix "... ")
(setq which-key-max-display-columns 3)
(setq which-key-idle-delay 0.125)
(setq which-key-idle-secondary-delay 0.25)
(setq which-key-add-column-padding 1)
(setq which-key-max-description-length 40)

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
      space-before-tab::space))

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

(load-theme 'gruber-darker 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(fido-mode 1)

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
; ===== MACOS STUFF ================================
; ==================================================

(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  default-input-method "MacOSX"))

(if (eq system-type 'darwin)
    (add-to-list 'exec-path "/opt/homebrew/bin/"))

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

(global-set-key (kbd "C-c j") 'join-line)

(global-set-key (kbd "M-z") 'undo)

(global-set-key (kbd "C-,") 'find-file)
(global-set-key (kbd "C-.") 'dired)

(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c s") 'shell-command)

(global-set-key (kbd "M-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (text-scale-decrease 1)))

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-log-all)

(global-set-key (kbd "C-c i") 'ibuffer)
(global-set-key (kbd "C-c l") 'switch-to-buffer)

(global-set-key (kbd "C-w") 'rc/copy)
(global-set-key (kbd "C-y") 'rc/paste)

(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m") 'mc/mark-more-like-this-extended)

; ==================================================
; ===== LSP ========================================
; ==================================================

(with-eval-after-load 'company
  (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

; ==================================================
; ===== HOOKS ======================================
; ==================================================

(add-hook 'c-mode-hook             'eglot-ensure)
(add-hook 'c++-mode-hook           'eglot-ensure)
(add-hook 'python-mode-hook        'eglot-ensure)

(add-hook 'after-init-hook         'global-company-mode)

(add-hook 'text-mode-hook          'rc/turn-on-paredit)

(add-hook 'c-mode-hook             'rc/set-up-whitespace-handling)
(add-hook 'c++-mode-hook           'rc/set-up-whitespace-handling)
(add-hook 'simpc-mode-hook         'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode         'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook      'rc/set-up-whitespace-handling)
(add-hook 'text-mode-hook          'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook        'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode-hook    'rc/set-up-whitespace-handling)

(add-hook 'compilation-filter-hook 'rc/colorize-compilation-buffer)

; ==================================================
; ===== CUSTOM =====================================
; ==================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit multiple-cursors move-text simpleclip gruber-darker-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
