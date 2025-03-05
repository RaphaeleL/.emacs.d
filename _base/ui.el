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
; (load-theme 'gruber-darker t)
; (load-theme 'doom-solarized-light t)
(load-theme 'solarized-light t)

(eval-after-load 'dired
  '(custom-set-faces
    '(dired-header ((t (:foreground "base01" :background "base03" :weight normal))))))

;; (solarized-create-theme-file-with-palette 'light 'solarized-light-custom
;;   '("#000000" "#faf5ee"
;;     "#3388dd" "#ac3d1a" "#dd2222" "#8b008b" "#00b7f0" "#1388a2" "#104e8b" "#00688b"))

(blink-cursor-mode 0)
(setq x-stretch-cursor nil)

(setq ring-bell-function 'ignore)

(setq echo-keystrokes 0.01)

(setq mouse-yank-at-point t)

(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
