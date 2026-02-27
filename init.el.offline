;;; init.el --- Offline, deterministic Emacs init -*- lexical-binding: t -*-

;; --------------------------------------------------
;; 1. Core paths
;; --------------------------------------------------

(defvar emacs-dir (file-name-as-directory user-emacs-directory))
(defvar base-dir  (expand-file-name "base/"      emacs-dir))
(defvar modes-dir (expand-file-name "modes/"     emacs-dir))
(defvar site-dir  (expand-file-name "site-lisp/" emacs-dir))

(add-to-list 'load-path base-dir)
(add-to-list 'load-path modes-dir)
(add-to-list 'load-path site-dir)

;; --------------------------------------------------
;; 2. Package system (offline, passive)
;; --------------------------------------------------

(require 'package)

(setq package-archives nil
      package-check-signature nil
      package-enable-at-startup nil
      use-package-always-ensure nil
      use-package-always-defer t
      use-package-expand-minimally t)

(package-initialize)

;; Optional: harden load-path (audit-friendly)
; (setq load-path
;       (seq-filter
;        (lambda (p)
;          (or (string-prefix-p emacs-dir p)
;              (string-prefix-p "/usr/share/emacs" p)))
;        load-path))

;; --------------------------------------------------
;; 3. Core configuration (NO packages here)
;; --------------------------------------------------

(load (expand-file-name "functions.el" base-dir) 'noerror)
(load (expand-file-name "packages.el"  base-dir) 'noerror)
(load (expand-file-name "garbage.el"   base-dir) 'noerror)
(load (expand-file-name "basic.el"     base-dir) 'noerror)
(load (expand-file-name "hooks.el"     base-dir) 'noerror)
(load (expand-file-name "lsp.el"       base-dir) 'noerror)

;; --------------------------------------------------
;; 4. Package-dependent configuration
;; --------------------------------------------------

(load (expand-file-name "magit.el"        modes-dir) 'noerror)
(load (expand-file-name "org.el"          modes-dir) 'noerror)
(load (expand-file-name "programming.el"  modes-dir) 'noerror)

;; --------------------------------------------------
;; 5. OS-specific configuration
;; --------------------------------------------------

(lr/modern)
(lr/line-relative)
(lr/default-theme)
;;(lr/theme 'lr_solarized_light)

(setq mac-command-modifier 'meta     ; Command as Meta
      mac-option-modifier 'control   ; Option as Control
      mac-control-modifier 'control  ; Ensure Control is Control
      default-input-method "MacOSX")

(add-to-list 'default-frame-alist '(width  . 136))
(add-to-list 'default-frame-alist '(height . 38))
; (add-to-list 'default-frame-alist '(left . 200))
; (add-to-list 'default-frame-alist '(top  . 120))

; (global-set-key (kbd "C-c 8") (lambda () (interactive) (insert "{")))
; (global-set-key (kbd "C-c 9") (lambda () (interactive) (insert "}")))
; (global-set-key (kbd "C-c 5") (lambda () (interactive) (insert "[")))
; (global-set-key (kbd "C-c 6") (lambda () (interactive) (insert "]")))

; (global-set-key (kbd "C-c C-8") (lambda () (interactive) (insert "{")))
; (global-set-key (kbd "C-c C-9") (lambda () (interactive) (insert "}")))
; (global-set-key (kbd "C-c C-5") (lambda () (interactive) (insert "[")))
; (global-set-key (kbd "C-c C-6") (lambda () (interactive) (insert "]")))

;; --------------------------------------------------
;; 6. Custom file
;; --------------------------------------------------

(setq custom-file (expand-file-name "output.el" emacs-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;; init.el ends here
