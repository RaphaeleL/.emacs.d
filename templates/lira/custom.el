; ====== CUSTOM USER SETTINGS =======================================

;; Load SSH configuration
(load "~/.config/lira/ssh.el")

; ====== UI =======================================

;; Load UI settings safely
(lira-enable-custom-font-safe)
(lira-load-theme-safe 'nord)

;; Frame settings
(when window-system 
  (set-frame-size (selected-frame) 88 30))

;; MacOS specific frame settings
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated-round . t)))

