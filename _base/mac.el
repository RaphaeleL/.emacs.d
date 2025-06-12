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


(when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))
