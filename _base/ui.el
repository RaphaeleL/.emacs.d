; (rc/require 'mood-line)
; (mood-line-mode 1)

(set-fringe-mode 0)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(defun get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-12")
   ((eq system-type 'darwin) "Iosevka-20")
   ((eq system-type 'gnu/linux) "Iosevka-20")))

(add-to-list 'default-frame-alist `(font . ,(get-default-font)))

(setq custom-safe-themes 1) 

; (set-frame-height (selected-frame) 35)
; (set-frame-width (selected-frame) 120)

(rc/require 'gruber-darker-theme)
(load-theme 'gruber-darker 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(rc/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq whitespace-style
    '(face
      tabs
      spaces
      tab-mark
      space-mark
      trailing
      missing-newline-at-eof
      space-after-tab::tab
      space-after-tab::space
      space-before-tab::tab
      space-before-tab::space))

