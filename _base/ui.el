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
; (load-theme 'gruber-darker t)
(load-theme 'doom-solarized-light t)

(when (eq (car custom-enabled-themes) 'doom-solarized-light)
  (custom-set-faces
   '(dired-directory ((t (:foreground "olive drab" :weight bold))))
   '(shadow ((t (:foreground "light slate gray"))))
   '(dired-header ((t (:foreground "base01" :background "base03" :weight normal))))
   '(magit-header-line ((t (:background "old lace" :foreground "slate gray" :box (:line-width (3 . 3) :color "old lace") :weight bold))))
   '(magit-diff-hunk-heading ((t (:extend t :background "old lace" :foreground "medium purple" :weight bold))))
   '(magit-section-heading ((t (:extend t :foreground "black" :weight bold))))))

(blink-cursor-mode 0)
(setq x-stretch-cursor nil)

(setq ring-bell-function 'ignore)

(setq echo-keystrokes 0.01)

(setq mouse-yank-at-point t)

(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
