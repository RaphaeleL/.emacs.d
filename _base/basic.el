; ==================================================
; ===== BASIC SHIT =================================
; ==================================================

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

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

;; Use y-or-n-p instead of yes-or-no-p (safer approach)
(setq use-dialog-box nil)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "N") 'dired-create-empty-file))

(when (eq system-type 'windows-nt)
  (setq default-directory "C:/Users/"))
