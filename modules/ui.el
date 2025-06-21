;;; ui.el -*- lexical-binding: t; -*-
;; UI configuration

;; === BASIC UI SETTINGS =========================
(set-fringe-mode 0)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; Disable UI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Cursor and bell settings
(blink-cursor-mode 0)
(setq x-stretch-cursor nil)
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.01)

;; Mouse and window settings
(setq mouse-yank-at-point t)
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; === LINE NUMBERS ==============================
(when (boundp 'lira-module-ui-line-numbers)
  ;; Enable line numbers globally
  (global-display-line-numbers-mode 1)
  
  ;; Configure line number appearance
  (if (boundp 'lira-module-ui-relative-line-numbers)
      (setq display-line-numbers-type 'relative)  ; relative when both enabled
    (setq display-line-numbers-type 'absolute))   ; absolute when only line-numbers enabled
  
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-grow-only t)
  
  ;; Customize line number face
  (set-face-attribute 'line-number nil
                      :foreground "#666666"
                      :background nil
                      :height 0.9)
  (set-face-attribute 'line-number-current-line nil
                      :foreground "#ffffff"
                      :background nil
                      :weight 'bold))

;; === MOOD LINE =================================
(when (boundp 'lira-module-ui-mood-line)
  (use-package mood-line
    :ensure t
    :config
    (mood-line-mode 1)))

;; === WHICH KEY =================================
(when (boundp 'lira-module-ui-which-key)
  (use-package which-key
    :ensure t
    :config
    (which-key-mode 1)))

;; === HL TODO ===================================
(when (boundp 'lira-module-ui-hl-todo)
  (use-package hl-todo
    :ensure t
    :config
    (global-hl-todo-mode)))

;; === THEMES ====================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-themes/enhanced")

;; === SMOOTH SCROLL =============================
(when (boundp 'lira-module-ui-smooth-scroll)
  (use-package smooth-scroll
    :ensure t
    :config
    (smooth-scroll-mode 1)))

;; === POPUP =====================================
(when (boundp 'lira-module-ui-popup)
  (use-package popup
    :ensure t))

;; === SAFE UI FUNCTIONS ===========================
;; Safe theme loading function
(defun lira-load-theme-safe (theme)
  "Safely load a theme with error handling."
  (condition-case err
      (progn
        (setq custom-safe-themes t)
        (load-theme theme t)
        (message "Theme '%s' loaded successfully" theme))
    (error
     (message "Warning: Could not load theme '%s': %s" theme (error-message-string err))
     ;; Try to load a fallback theme
     (condition-case err2
         (progn
           (load-theme 'deeper-blue t)
           (message "Loaded fallback theme 'deeper-blue'"))
       (error
        (message "Warning: Could not load fallback theme either: %s" (error-message-string err2)))))))

;; Safe font loading function
(defun lira-enable-custom-font-safe ()
  "Safely enable custom font with error handling."
  (condition-case err
      (lira-enable-custom-font)
    (error
     (message "Warning: Could not load custom font: %s" (error-message-string err)))))

;; === THEME UTILITY FUNCTIONS ====================
(defun lira-load-theme (theme)
  (interactive
   (list (intern (completing-read "Theme: " (custom-available-themes)))))
   (condition-case err
       (progn
         (setq custom-safe-themes t)
         (load-theme theme t)
         (message "Theme '%s' loaded successfully" theme))
     (error
      (message "Warning: Could not load theme '%s': %s" theme (error-message-string err))))) 