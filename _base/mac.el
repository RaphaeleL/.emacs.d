; ==================================================
; ===== MacOS=======================================
; ==================================================

;; macOS-specific configuration
(when (eq system-type 'darwin)
  ;; Key remapping:
  (setq mac-command-modifier 'meta       ; Command as Meta
        mac-option-modifier 'control     ; Option as Control
        mac-control-modifier 'control    ; Ensure Control is Control
        default-input-method "MacOSX")

  ;; Use GNU ls from coreutils (via Homebrew)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

;; Homebrew
(when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

;; WTF
(global-set-key (kbd "C-c 8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (insert "]")))
