; ==================================================
; ===== BASIC SHIT =================================
; ==================================================

(when (lr/feature-enabled-p 'lr/defaults)
  ;; Disable native compilation to avoid startup errors
  (setq native-comp-enable-subr-trampolines nil)
  (setq native-comp-async-report-warnings-errors nil)

  (save-place-mode 1)
  (simpleclip-mode 1)

  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (setq package-enable-at-startup nil)

  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (delete-selection-mode 1)
  (electric-indent-mode -1)

  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)

  ;; Use y-or-n-p instead of yes-or-no-p (safer approach)
  (setq use-dialog-box nil)

  (with-eval-after-load 'dired
	(define-key dired-mode-map (kbd "N") 'dired-create-empty-file))

  (when (eq system-type 'windows-nt) (setq default-directory "C:/Users/"))
  (when (eq system-type 'darwin) (add-to-list 'default-frame-alist '(undecorated-round . t))))

; ==================================================
; ===== UI =========================================
; ==================================================

(when (lr/feature-enabled-p 'lr/ui)

  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode 0)
  (mood-line-mode 1)

  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'text-mode)

  (add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-themes/enhanced")

  (blink-cursor-mode 0)
  (setq x-stretch-cursor nil)

  (setq ring-bell-function 'ignore)

  (setq echo-keystrokes 0.01)

  (setq mouse-yank-at-point t)

  (setq window-resize-pixelwise t
		frame-resize-pixelwise t)

  (add-to-list 'default-frame-alist '(width . 88))
  (add-to-list 'default-frame-alist '(height . 33))

  (setq compilation-scroll-output t))
