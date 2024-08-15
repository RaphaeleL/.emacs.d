(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(setq projectile-switch-project-action 'magit-status)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  "Restores the previous window configuration and kills the magit buffer"
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-mode-quit-window)

